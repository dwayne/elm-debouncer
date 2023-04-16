module HttpInput exposing (main)

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
    { status : Status
    , query : String
    , debouncer : Debouncer String

    --
    -- We use a timer to simulate an HTTP request that takes some time to
    -- successfully complete.
    --
    , timer : Timer
    , result : RemoteData
    }


type Status
    = NotTyping
    | Typing


type RemoteData
    = Initial
    | Loading
    | Success


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NotTyping "" Debouncer.init Timer.init Initial
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputQuery String
    | ReadyToMakeHTTPRequest String
    | ChangedDebouncer Debouncer.Msg
    | GotResult
    | ChangedTimer Timer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputQuery query ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call debouncerConfig query model.debouncer
            in
            ( { model
                | status = Typing
                , query = query
                , debouncer = debouncer

                --
                -- Cancel any active HTTP request.
                --
                , timer = Timer.cancel model.timer
                , result = Initial
              }
            , cmd
            )

        ReadyToMakeHTTPRequest rawQuery ->
            let
                query =
                    String.trim rawQuery
            in
            if String.isEmpty query then
                ( { model | status = NotTyping }
                , Cmd.none
                )

            else
                --
                -- Make the HTTP request.
                --
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
            ( { model | status = NotTyping, query = "", result = Success }
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
        , onReady = ReadyToMakeHTTPRequest
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
                    H.text "Type in the input box below. I will detect when you stop typing."

                Typing ->
                    H.text "Waiting for more keystrokes..."
            ]
        , H.p []
            [ H.input
                [ HA.type_ "text"
                , HA.autofocus True
                , HA.value model.query
                , HE.onInput InputQuery
                ]
                []
            ]
        , H.p []
            [ case model.result of
                Initial ->
                    H.text ""

                Loading ->
                    H.text "That's enough waiting. I am making the HTTP request now."

                Success ->
                    H.text "Success!"
            ]
        ]
