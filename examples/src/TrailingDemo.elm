module TrailingDemo exposing (main)

import Browser as B
import Html as H
import Html.Attributes as HA
import Timer exposing (Timer)
import Widget.Demo as Demo


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- CONSTRANTS


frequency : Int
frequency =
    100



-- MODEL


type alias Model =
    { rawEvents : List Int
    , debouncedEvents : List Int
    , isRunning : Bool
    , timer : Timer
    , currentColor : Int
    , rawColor : Int
    , debouncedColor : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { rawEvents = []
      , debouncedEvents = []
      , isRunning = False
      , timer = Timer.init
      , currentColor = 2
      , rawColor = 0
      , debouncedColor = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Started
    | GotEvent
    | Stopped
    | TimerExpired
    | ChangedTimer (Timer.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Started ->
            let
                ( timer, cmd ) =
                    Timer.setInterval
                        { onExpire = TimerExpired
                        , onChange = ChangedTimer
                        }
                        frequency
                        model.timer
            in
            ( { model | isRunning = True, timer = timer }
            , cmd
            )

        GotEvent ->
            ( { model | rawColor = model.currentColor }
            , Cmd.none
            )

        Stopped ->
            ( { model
                | rawEvents = []
                , debouncedEvents = []
                , isRunning = False
                , timer = Timer.cancel model.timer
              }
            , Cmd.none
            )

        TimerExpired ->
            let
                rawEvents =
                    model.rawColor :: model.rawEvents

                debouncedEvents =
                    model.debouncedColor :: model.debouncedEvents
            in
            ( { model
                | rawEvents = rawEvents
                , debouncedEvents = debouncedEvents
                , timer =
                    if List.length rawEvents >= 90 then
                        Timer.cancel model.timer

                    else
                        model.timer
                , rawColor = 0
                , debouncedColor = 0
              }
            , Cmd.none
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update ChangedTimer timerMsg model.timer
            )



-- VIEW


view : Model -> H.Html Msg
view { rawEvents, debouncedEvents, isRunning } =
    H.div [ HA.class "content" ]
        [ Demo.view
            { section1 =
                { title = "Raw events over time"
                , subtitle = Nothing
                , events = List.reverse rawEvents
                }
            , section2 =
                { title = "Debounced events"
                , subtitle = Just "400ms, trailing"
                , events = List.reverse debouncedEvents
                }
            , isRunning = isRunning
            , onStart = Started
            , onEvent = GotEvent
            , onStop = Stopped
            }
        ]
