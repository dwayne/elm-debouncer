module RegistrationForm exposing (main)

-- This example is based on
-- https://github.com/Orasund/elm-cookbook/issues/1#issue-456089065

import Browser as B
import Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Lib.Timer as Timer exposing (Timer)
import Random


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
    { username : String
    , status : Status
    , debouncer : Debouncer String
    , timer : Timer
    }


type Status
    = Normal
    | Checking String
    | Error
    | Success


init : () -> ( Model, Cmd msg )
init _ =
    ( { username = ""
      , status = Normal
      , debouncer = Debouncer.trailing 500
      , timer = Timer.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EnteredUsername String
    | ReadyToInvoke String
    | ChangedDebouncer (Debouncer.Msg Msg String)
    | TimerExpired
    | ChangedTimer (Timer.Msg Msg)
    | GotResult Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername username ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call
                        { onReady = ReadyToInvoke
                        , onChange = ChangedDebouncer
                        }
                        username
                        model.debouncer
            in
            ( { model
                | username = username
                , status = Normal
                , debouncer = debouncer
              }
            , cmd
            )

        ReadyToInvoke username ->
            if String.isEmpty username then
                ( model
                , Cmd.none
                )

            else
                let
                    ( timer, cmd ) =
                        Timer.setTimeout
                            { onExpire = TimerExpired
                            , onChange = ChangedTimer
                            }
                            5000
                            model.timer
                in
                ( { model | status = Checking username, timer = timer }
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

        TimerExpired ->
            ( model
            , Random.generate GotResult <|
                Random.weighted
                    ( 1, False )
                    [ ( 3, True )
                    ]
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update ChangedTimer timerMsg model.timer
            )

        GotResult isFree ->
            ( case model.status of
                Checking _ ->
                    { model
                        | status =
                            if isFree then
                                Success

                            else
                                Error
                    }

                _ ->
                    model
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { username, status } =
    H.form []
        [ H.label [] [ H.text "Username: " ]
        , H.input
            [ HA.type_ "text"
            , HA.autofocus True
            , HA.value username
            , HE.onInput EnteredUsername
            ]
            []
        , H.p []
            [ case status of
                Normal ->
                    H.text ""

                Checking s ->
                    H.em
                        []
                        [ H.text <| "Checking if " ++ s ++ " is free..."
                        ]

                Error ->
                    H.span
                        [ HA.style "color" "red" ]
                        [ H.text "The username is taken."
                        ]

                Success ->
                    H.span
                        [ HA.style "color" "green" ]
                        [ H.text "The username is available."
                        ]
            ]
        ]
