module TrailingDemo exposing (main)

import Browser as B
import Debouncer exposing (Debouncer)
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



-- MODEL


type alias Model =
    { rawEvents : List Int
    , debouncedEvents : List Int
    , isRunning : Bool
    , timer : Timer
    , debouncer : Debouncer ()
    , currentColor : Int
    , rawColor : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { rawEvents = []
      , debouncedEvents = []
      , isRunning = False
      , timer = Timer.init
      , debouncer = Debouncer.trailing <| 4 * sampleRate
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
    | ChangedTimer (Timer.Msg Msg)
    | ReadyToInvoke
    | ChangedDecouncer (Debouncer.Msg Msg ())


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
                        sampleRate
                        model.timer
            in
            ( { model | isRunning = True, timer = timer }
            , cmd
            )

        GotEvent ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call
                        { onReady = always ReadyToInvoke
                        , onChange = ChangedDecouncer
                        }
                        ()
                        model.debouncer
            in
            ( { model | debouncer = debouncer, rawColor = model.currentColor }
            , cmd
            )

        Stopped ->
            ( { model
                | rawEvents = []
                , debouncedEvents = []
                , isRunning = False
                , timer = Timer.cancel model.timer
                , debouncer = Debouncer.cancel model.debouncer
                , rawColor = 0
              }
            , Cmd.none
            )

        TimerExpired ->
            let
                rawEvents =
                    model.rawColor :: model.rawEvents

                debouncedEvents =
                    0 :: model.debouncedEvents

                ( timer, debouncer ) =
                    if isFull rawEvents then
                        ( Timer.cancel model.timer
                        , Debouncer.cancel model.debouncer
                        )

                    else
                        ( model.timer
                        , model.debouncer
                        )
            in
            ( { model
                | rawEvents = rawEvents
                , debouncedEvents = debouncedEvents
                , timer = timer
                , debouncer = debouncer
                , rawColor = 0
              }
            , Cmd.none
            )

        ChangedTimer timerMsg ->
            ( model
            , Timer.update ChangedTimer timerMsg model.timer
            )

        ReadyToInvoke ->
            let
                rawEvents =
                    0 :: model.rawEvents

                debouncedEvents =
                    model.currentColor :: model.debouncedEvents
            in
            ( { model
                | rawEvents = rawEvents
                , debouncedEvents = debouncedEvents
                , currentColor =
                    if model.currentColor == 10 then
                        2

                    else
                        model.currentColor + 1
              }
            , Cmd.none
            )

        ChangedDecouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update
                        ChangedDecouncer
                        debouncerMsg
                        model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
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
