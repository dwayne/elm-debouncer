module RegistrationForm exposing (main)

-- This example is based on
-- https://github.com/Orasund/elm-cookbook/issues/1#issue-456089065

import Browser as B
import Html as H
import Html.Attributes as HA
import Lib.Timer as Timer exposing (Timer)
import Random
import Widget.TextInput as TextInput exposing (TextInput)


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
    { username : TextInput
    , status : Status
    , timer : Timer
    }


type Status
    = Normal
    | Checking String
    | Error
    | Success


init : () -> ( Model, Cmd msg )
init _ =
    ( { username = TextInput.init ""
      , status = Normal
      , timer = Timer.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EnteredUsername
    | ReadyToInvoke String
    | ChangedUsername TextInput.Msg
    | TimerExpired
    | ChangedTimer Timer.Msg
    | GotResult Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredUsername ->
            ( { model
                | status = Normal
                , timer =
                    case model.status of
                        Checking _ ->
                            Timer.cancel model.timer

                        _ ->
                            model.timer
              }
            , Cmd.none
            )

        ReadyToInvoke username ->
            let
                ( timer, cmd ) =
                    Timer.setTimeout timerConfig model.timer
            in
            ( { model | status = Checking username, timer = timer }
            , cmd
            )

        ChangedUsername usernameMsg ->
            let
                ( username, cmd ) =
                    TextInput.update textInputConfig usernameMsg model.username
            in
            ( { model | username = username }
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
            , Timer.update timerConfig timerMsg model.timer
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


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = 5000
        , onExpire = TimerExpired
        , onChange = ChangedTimer
        }



-- VIEW


view : Model -> H.Html Msg
view { username, status } =
    H.form []
        [ H.label [] [ H.text "Username: " ]
        , TextInput.view { autofocus = True } textInputConfig username
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


textInputConfig : TextInput.Config Msg
textInputConfig =
    TextInput.config
        { wait = 500
        , onInput = always EnteredUsername
        , onReady = ReadyToInvoke
        , onChange = ChangedUsername
        }
