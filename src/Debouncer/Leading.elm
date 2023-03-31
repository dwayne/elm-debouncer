module Debouncer.Leading exposing
    ( Debouncer
    , init
    , cancel

    , Options
    , debounce

    , Msg
    , update
    )


import Process
import Task


type Debouncer
    = Debouncer Int Bool


init : Debouncer
init =
    Debouncer 0 False


cancel : Debouncer -> Debouncer
cancel (Debouncer id _) =
    Debouncer (id + 1) False


type alias Options msg =
    { wait : Int
    , onReady : msg
    , onChange : Msg -> msg
    }


debounce : Options msg -> Debouncer -> (Debouncer, Cmd msg)
debounce { wait, onReady, onChange } (Debouncer id isActive) =
    let
        newId =
            id + 1

        newDebouncer =
            Debouncer newId True
    in
    if isActive then
        ( newDebouncer
        , Process.sleep (toFloat wait)
            |> Task.perform (always (WakeUp newId))
            |> Cmd.map onChange
        )

    else
        ( newDebouncer
        , Cmd.batch
            [ dispatch onReady
            , Process.sleep (toFloat wait)
                |> Task.perform (always (WakeUp newId))
                |> Cmd.map onChange
            ]
        )


type Msg
    = WakeUp Int


update : Msg -> Debouncer -> Debouncer
update msg (Debouncer id _ as debouncer) =
    case msg of
        WakeUp incomingId ->
            if incomingId == id then
                Debouncer id False

            else
                debouncer


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
