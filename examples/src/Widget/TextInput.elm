module Widget.TextInput exposing
    ( Msg
    , TextInput
    , UpdateOptions
    , ViewOptions
    , init
    , update
    , view
    )

import Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task


type TextInput
    = TextInput
        { value : String
        , debouncer : Debouncer String
        }


init : String -> Int -> TextInput
init initialValue wait =
    TextInput
        { value = initialValue
        , debouncer = Debouncer.trailing wait
        }


type Msg msg
    = EnteredValue (String -> msg) (String -> msg) String
    | ReadyToInvoke (String -> msg) String
    | ChangedDebouncer (Debouncer.Msg (Msg msg) String)


type alias UpdateOptions msg =
    { onChange : Msg msg -> msg
    }


update : (Msg msg -> msg) -> Msg msg -> TextInput -> ( TextInput, Cmd msg )
update onChange msg ((TextInput state) as textInput) =
    case msg of
        EnteredValue onInput onReady value ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call
                        { onReady = ReadyToInvoke onReady
                        , onChange = ChangedDebouncer
                        }
                        value
                        state.debouncer
            in
            ( TextInput { value = value, debouncer = debouncer }
            , Cmd.batch
                [ Cmd.map onChange cmd
                , dispatch <| onInput value
                ]
            )

        ReadyToInvoke onReady value ->
            if String.isEmpty value then
                ( textInput
                , Cmd.none
                )

            else
                ( textInput
                , dispatch <| onReady value
                )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update
                        ChangedDebouncer
                        debouncerMsg
                        state.debouncer
            in
            ( TextInput { state | debouncer = debouncer }
            , Cmd.map onChange cmd
            )


type alias ViewOptions msg =
    { autofocus : Bool
    , onInput : String -> msg
    , onReady : String -> msg
    , onChange : Msg msg -> msg
    }


view : ViewOptions msg -> TextInput -> H.Html msg
view { autofocus, onInput, onReady, onChange } (TextInput { value }) =
    H.input
        [ HA.type_ "text"
        , HA.autofocus autofocus
        , HA.value value
        , HE.onInput (EnteredValue onInput onReady)
        ]
        []
        |> H.map onChange


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
