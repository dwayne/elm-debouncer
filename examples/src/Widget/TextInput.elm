module Widget.TextInput exposing
    ( Config
    , Msg
    , TextInput
    , ViewOptions
    , config
    , init
    , update
    , view
    )

import Debouncer2 as Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task


type TextInput
    = TextInput
        { value : String
        , debouncer : Debouncer String
        }


init : String -> TextInput
init initialValue =
    TextInput
        { value = initialValue
        , debouncer = Debouncer.init
        }

type Config msg
    = Config
        { debouncerConfig : Debouncer.Config String Msg
        , onInput : String -> msg
        , onReady : String -> msg
        , onChange : Msg -> msg
        }


config
    : { wait : Int
      , onInput : String -> msg
      , onReady : String -> msg
      , onChange : Msg -> msg
      }
    -> Config msg
config { wait, onInput, onReady, onChange } =
    Config
        { debouncerConfig =
            Debouncer.trailing
                { wait = wait
                , onReady = ReadyToInvoke
                , onChange = ChangedDebouncer
                }
        , onInput = onInput
        , onReady = onReady
        , onChange = onChange
        }


type Msg
    = EnteredValue String
    | ReadyToInvoke String
    | ChangedDebouncer Debouncer.Msg


update : Config msg -> Msg -> TextInput -> ( TextInput, Cmd msg )
update (Config c) msg ((TextInput state) as textInput) =
    case msg of
        EnteredValue value ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call c.debouncerConfig value state.debouncer
            in
            ( TextInput { value = value, debouncer = debouncer }
            , Cmd.batch
                [ Cmd.map c.onChange cmd
                , dispatch <| c.onInput value
                ]
            )

        ReadyToInvoke value ->
            if String.isEmpty value then
                ( textInput
                , Cmd.none
                )

            else
                ( textInput
                , dispatch <| c.onReady value
                )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update c.debouncerConfig debouncerMsg state.debouncer
            in
            ( TextInput { state | debouncer = debouncer }
            , Cmd.map c.onChange cmd
            )


type alias ViewOptions =
    { autofocus : Bool
    }


view : ViewOptions -> Config msg -> TextInput -> H.Html msg
view { autofocus } (Config c) (TextInput { value }) =
    H.input
        [ HA.type_ "text"
        , HA.autofocus autofocus
        , HA.value value
        , HE.onInput EnteredValue
        ]
        []
        |> H.map c.onChange


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
