module Debouncer exposing
    ( Debouncer
    , init
    , cancel

    , Options
    , debounce

    , Msg
    , update
    )


-- NOTE: This is a trailing edge debouncer.


import Process
import Task


type Debouncer
    = Debouncer Int


init : Debouncer
init =
    Debouncer 0


cancel : Debouncer -> Debouncer
cancel (Debouncer id) =
    Debouncer (id + 1)


type alias Options msg =
    { wait : Int
    , onReady : msg
    , onChange : Msg msg -> msg
    }


debounce : Options msg -> Debouncer -> (Debouncer, Cmd msg)
debounce { wait, onReady, onChange } (Debouncer id) =
    let
        newId =
            id + 1
    in
    ( Debouncer newId
    , Process.sleep (toFloat wait)
        |> Task.perform (always (WakeUp newId onReady))
        |> Cmd.map onChange
    )


type Msg msg
    = WakeUp Int msg


update : Msg msg -> Debouncer -> Cmd msg
update msg (Debouncer id) =
    case msg of
        WakeUp incomingId onReady ->
            if incomingId == id then
                dispatch onReady

            else
                Cmd.none


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
