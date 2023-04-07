module AjaxInput exposing (main)

-- This example is based on
-- https://css-tricks.com/debouncing-throttling-explained-examples/#aa-keypress-on-autocomplete-form-with-ajax-request

import Browser as B
import Browser.Events as BE
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
    let
        debouncer =
            Debouncer.trailing 1300

        timer =
            Timer.init
    in
    ( Model "" debouncer timer Initial NotTyping
    , Cmd.none
    )



-- UPDATE


type Msg
    = EnteredQuery String
    | ReadyToInvoke String
    | ChangedDebouncer (Debouncer.Msg Msg String)
    | GotResult
    | ChangedTimer (Timer.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredQuery query ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call
                        { onReady = ReadyToInvoke
                        , onChange = ChangedDebouncer
                        }
                        query
                        model.debouncer
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
                        Timer.setTimeout
                            { onExpire = GotResult
                            , onChange = ChangedTimer
                            }
                            2000
                            model.timer
                in
                ( { model | result = Loading, timer = timer }
                , cmd
                )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update
                        ChangedDebouncer
                        debouncerMsg
                        model.debouncer
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
            , Timer.update ChangedTimer timerMsg model.timer
            )



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
