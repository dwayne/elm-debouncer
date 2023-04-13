module AjaxInput exposing (main)

-- This example is based on
-- https://css-tricks.com/debouncing-throttling-explained-examples/#aa-keypress-on-autocomplete-form-with-ajax-request.

import Browser as B
import Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Timer as Timer exposing (Timer)


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { query : String
    , debouncer : Debouncer String
    , timer : Timer
    , result : RemoteData
    , status : Status
    }


type RemoteData
    = Initial
    | Loading
    | Success


type Status
    = NotTyping
    | Typing


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Debouncer.init Timer.init Initial NotTyping
    , Cmd.none
    )



-- UPDATE


type Msg
    = EnteredQuery String
    | ReadyToInvoke String
    | ChangedDebouncer Debouncer.Msg
    | GotResult
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredQuery query ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call debouncerConfig query model.debouncer
            in
            ( { model
                | query = query
                , debouncer = debouncer
                , timer = Timer.cancel model.timer
                , result = Initial
                , status = Typing
              }
            , cmd
            )

        ReadyToInvoke query ->
            if String.isEmpty query then
                ( { model | status = NotTyping }
                , Cmd.none
                )

            else
                let
                    ( timer, cmd ) =
                        Timer.setTimeout timerConfig model.timer
                in
                ( { model | result = Loading, timer = timer }
                , cmd
                )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update debouncerConfig debouncerMsg model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )

        GotResult ->
            ( { model | query = "", result = Success, status = NotTyping }
            , Cmd.none
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update timerConfig timerMsg model.timer
            )


debouncerConfig : Debouncer.Config String Msg
debouncerConfig =
    Debouncer.trailing
        { wait = 1300
        , onReady = ReadyToInvoke
        , onChange = ChangedDebouncer
        }


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 2000
        , onExpire = GotResult
        , onChange = ChangedTimer
        }



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.p []
            [ case model.status of
                NotTyping ->
                    H.text "Type here. I will detect when you stop typing."

                Typing ->
                    H.text "Waiting for more keystrokes..."
            ]
        , H.p []
            [ H.input
                [ HA.type_ "text"
                , HA.autofocus True
                , HA.value model.query
                , HE.onInput EnteredQuery
                ]
                []
            ]
        , H.p []
            [ case model.result of
                Loading ->
                    H.text "That's enough waiting. Making the ajax request now."

                _ ->
                    H.text ""
            ]
        ]
