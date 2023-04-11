module ThrottleDemo exposing (main)

import Browser as B
import Debouncer2 as Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA
import Lib.Timer as Timer exposing (Timer)
import Widget.Demo as Demo


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
    { rawEvents : List Int
    , throttledEvents : List Int
    , isRunning : Bool
    , timer : Timer
    , debouncer : Debouncer ()
    , throttler : Debouncer ()
    , currentColor : Int
    , rawColor : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    let
        wait =
            4 * sampleRate
    in
    ( { rawEvents = []
      , throttledEvents = []
      , isRunning = False
      , timer = Timer.init
      , debouncer = Debouncer.init
      , throttler = Debouncer.init
      , currentColor = 2
      , rawColor = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Started
    | GotEvent
    | Stopped
    | TimerExpired
    | ChangedTimer Timer.Msg
    | ReadyToInvokeOnTrailing
    | ChangedDebouncer Debouncer.Msg
    | ReadyToInvoke
    | ChangedThrottler Debouncer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Started ->
            let
                ( timer, cmd ) =
                    Timer.setInterval timerConfig model.timer
            in
            ( { model | isRunning = True, timer = timer }
            , cmd
            )

        GotEvent ->
            let
                ( debouncer, debouncerCmd ) =
                    Debouncer.call debouncerConfig () model.debouncer

                ( throttler, throttlerCmd ) =
                    Debouncer.call throttlerConfig () model.throttler
            in
            ( { model
                | debouncer = debouncer
                , throttler = throttler
                , rawColor = model.currentColor
              }
            , Cmd.batch
                [ debouncerCmd
                , throttlerCmd
                ]
            )

        Stopped ->
            ( { model
                | rawEvents = []
                , throttledEvents = []
                , isRunning = False
                , timer = Timer.cancel model.timer
                , debouncer = Debouncer.cancel model.debouncer
                , throttler = Debouncer.cancel model.throttler
                , rawColor = 0
              }
            , Cmd.none
            )

        TimerExpired ->
            let
                rawEvents =
                    model.rawColor :: model.rawEvents

                throttledEvents =
                    0 :: model.throttledEvents

                { timer, debouncer, throttler } =
                    if isFull rawEvents then
                        { timer = Timer.cancel model.timer
                        , debouncer = Debouncer.cancel model.debouncer
                        , throttler = Debouncer.cancel model.throttler
                        }

                    else
                        { timer = model.timer
                        , debouncer = model.debouncer
                        , throttler = model.throttler
                        }
            in
            ( { model
                | rawEvents = rawEvents
                , throttledEvents = throttledEvents
                , timer = timer
                , debouncer = debouncer
                , throttler = throttler
                , rawColor = 0
              }
            , Cmd.none
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update timerConfig timerMsg model.timer
            )

        ReadyToInvokeOnTrailing ->
            let
                currentColor =
                    if model.currentColor == 10 then
                        2

                    else
                        model.currentColor + 1
            in
            ( { model
                | rawColor = 0
                , currentColor = currentColor
              }
            , Cmd.none
            )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update debouncerConfig debouncerMsg model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )

        ReadyToInvoke ->
            let
                rawEvents =
                    model.rawColor :: model.rawEvents

                throttledEvents =
                    model.currentColor :: model.throttledEvents
            in
            ( { model
                | rawEvents = rawEvents
                , throttledEvents = throttledEvents
                , rawColor = 0
              }
            , Cmd.none
            )

        ChangedThrottler throttlerMsg ->
            let
                ( throttler, cmd ) =
                    Debouncer.update throttlerConfig throttlerMsg model.throttler
            in
            ( { model | throttler = throttler }
            , cmd
            )


timerConfig : Timer.Config Msg
timerConfig =
    Timer.config
        { wait = sampleRate
        , onExpire = TimerExpired
        , onChange = ChangedTimer
        }


debouncerConfig : Debouncer.Config () Msg
debouncerConfig =
    Debouncer.trailing
        { wait = 4 * sampleRate + 5
        , onReady = always ReadyToInvokeOnTrailing
        , onChange = ChangedDebouncer
        }


throttlerConfig : Debouncer.Config () Msg
throttlerConfig =
    Debouncer.throttle
        { wait = 4 * sampleRate
        , onReady = always ReadyToInvoke
        , onChange = ChangedThrottler
        }



-- VIEW


view : Model -> H.Html Msg
view { rawEvents, throttledEvents, isRunning } =
    H.div [ HA.class "content" ]
        [ Demo.view
            { section1 =
                { title = "Raw events over time"
                , subtitle = Nothing
                , events = List.reverse rawEvents
                }
            , section2 =
                { title = "Throttled events"
                , subtitle = Just "400ms, throttle"
                , events = List.reverse throttledEvents
                }
            , isRunning = isRunning
            , isFull = isFull rawEvents
            , onStart = Started
            , onEvent = GotEvent
            , onStop = Stopped
            }
        ]



-- MISC


sampleRate : Int
sampleRate =
    100


isFull : List a -> Bool
isFull list =
    List.length list >= 90
