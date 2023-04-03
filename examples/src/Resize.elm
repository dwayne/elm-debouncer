module Resize exposing (main)


import Browser as B
import Browser.Events as BE
import Throttler exposing (Throttler)
import Html as H
import Html.Attributes as HA


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { raw : List Event
    , throttled : List Event
    , throttler : Throttler Event
    }


type alias Event =
    { width : Int
    , height : Int
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model [] [] Throttler.init
    , Cmd.none
    )


-- UPDATE


type Msg
    = ResizedWindow Int Int
    | Ready Event
    | ChangedThrottler (Throttler.Msg Msg Event)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ResizedWindow width height ->
            let
                event =
                    Event width height

                ( throttler, cmd ) =
                    Throttler.throttle
                        { wait = 5000
                        , onReady = Ready
                        , onChange = ChangedThrottler
                        }
                        event
                        model.throttler
            in
            ( { model | raw = event :: model.raw, throttler = throttler }
            , cmd
            )

        Ready event ->
            --
            -- Here is where you do the work.
            --
            ( { model | throttled = event :: model.throttled }
            , Cmd.none
            )

        ChangedThrottler throttlerMsg ->
            let
                ( throttler, cmd ) =
                    Throttler.update throttlerMsg model.throttler
            in
            ( { model | throttler = throttler }
            , cmd
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    BE.onResize ResizedWindow


-- VIEW


view : Model -> H.Html msg
view { raw, throttled } =
    viewFrame { raw = raw, throttled = throttled }


viewFrame : { raw : List Event, throttled : List Event } -> H.Html msg
viewFrame { raw, throttled } =
    H.div [ HA.class "frame" ]
        [ H.div [ HA.class "frame__item" ]
            [ viewPanel "Raw resize events" raw
            ]
        , H.div [ HA.class "frame__item" ]
            [ viewPanel "Throttled resize events" throttled
            ]
        ]


viewPanel : String -> List Event -> H.Html msg
viewPanel title events =
    H.div [ HA.class "panel" ] <|
        H.h1 [] [ H.text title ] :: List.map viewEvent events


viewEvent : Event -> H.Html msg
viewEvent { width, height } =
    H.p [] [ H.text <| String.fromInt width ++ " x " ++ String.fromInt height ]
