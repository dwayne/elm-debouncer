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


config :
    { wait : Int
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
                , onReady = Ready
                , onChange = ChangedDebouncer
                }
        , onInput = onInput
        , onReady = onReady
        , onChange = onChange
        }


type Msg
    = InputValue String
    | Ready String
    | ChangedDebouncer Debouncer.Msg


update : Config msg -> Msg -> TextInput -> ( TextInput, Cmd msg )
update (Config c) msg ((TextInput s) as textInput) =
    case msg of
        InputValue value ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call c.debouncerConfig value s.debouncer
            in
            ( TextInput
                { value = value
                , debouncer = debouncer
                }
            , Cmd.batch
                [ Cmd.map c.onChange cmd
                , dispatch <| c.onInput value
                ]
            )

        Ready value ->
            ( textInput
            , dispatch <| c.onReady value
            )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update c.debouncerConfig debouncerMsg s.debouncer
            in
            ( TextInput { s | debouncer = debouncer }
            , Cmd.map c.onChange cmd
            )


type alias ViewOptions =
    { autofocus : Bool
    }


view : ViewOptions -> Config msg -> TextInput -> H.Html msg
view { autofocus } (Config { onChange }) (TextInput { value }) =
    H.input
        [ HA.type_ "text"
        , HA.autofocus autofocus
        , HA.value value
        , HE.onInput InputValue
        ]
        []
        |> H.map onChange


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
