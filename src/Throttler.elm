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


type Throttler a
    = Throttler
        { id : Int
        , isWaiting : Bool
        , lastArg : Maybe a
        }


init : Throttler a
init =
    Throttler
        { id = 0
        , isWaiting = False
        , lastArg = Nothing
        }


cancel : Throttler a -> Throttler a
cancel (Throttler { id }) =
    Throttler
        { id = id + 1
        , isWaiting = False
        , lastArg = Nothing
        }


type alias Options msg a =
    { wait : Int
    , onReady : a -> msg
    , onChange : Msg msg a -> msg
    }


throttle : Options msg a -> a -> Throttler a -> (Throttler a, Cmd msg)
throttle { wait, onReady, onChange } args (Throttler { id, isWaiting, lastArg }) =
    if isWaiting then
        ( Throttler
            { id = id
            , isWaiting = isWaiting
            , lastArg = Just args
            }
        , Cmd.none
        )

    else
        ( Throttler
            { id = id
            , isWaiting = True
            , lastArg = Just args
            }
        , Process.sleep (toFloat wait)
            |> Task.perform (always (TimerExpired id onReady))
            |> Cmd.map onChange
        )


type Msg msg a
    = TimerExpired Int (a -> msg)


update : Msg msg a -> Throttler a -> (Throttler a, Cmd msg)
update msg (Throttler { id, isWaiting, lastArg } as throttler) =
    case msg of
        TimerExpired incomingId onReady ->
            if incomingId == id && isWaiting then
                ( Throttler
                    { id = id + 1
                    , isWaiting = False
                    , lastArg = Nothing
                    }
                , case lastArg of
                    Just arg ->
                        dispatch (onReady arg)

                    Nothing ->
                        Cmd.none
                )

            else
                ( throttler
                , Cmd.none
                )


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
