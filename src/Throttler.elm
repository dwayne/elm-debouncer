module Throttler exposing
    ( Throttler
    , init
    , cancel

    , Options
    , throttle

    , Msg
    , update
    )


import Process
import Task


type Throttler
    = Throttler Int Bool


init : Throttler
init =
    Throttler 0 False


cancel : Throttler -> Throttler
cancel (Throttler id _) =
    Throttler (id + 1) False


type alias Options msg =
    { wait : Int
    , onReady : msg
    , onChange : Msg msg -> msg
    }


throttle : Options msg -> Throttler -> (Throttler, Cmd msg)
throttle { wait, onReady, onChange } (Throttler id isWaiting as throttler) =
    if isWaiting then
        ( throttler
        , Cmd.none
        )

    else
        ( Throttler id True
        , Process.sleep (toFloat wait)
            |> Task.perform (always (TimerExpired id onReady))
            |> Cmd.map onChange
        )


type Msg msg
    = TimerExpired Int msg


update : Msg msg -> Throttler -> (Throttler, Cmd msg)
update msg (Throttler id isWaiting as throttler) =
    case msg of
        TimerExpired incomingId onReady ->
            if incomingId == id && isWaiting then
                ( Throttler (id + 1) False
                , dispatch onReady
                )

            else
                ( throttler
                , Cmd.none
                )


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
