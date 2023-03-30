module Debouncer exposing
    ( Debouncer
    , Id
    , Options
    , cancel
    , init
    , update
    , tryToApply
    )


import Debouncer.Internal as D
import Process
import Task


type Debouncer
    = Debouncer D.Debouncer


init : Debouncer
init =
    Debouncer D.init


type Id
    = Id Int


type alias Options msg =
    { millis : Int
    , onReady : Id -> msg
    }


update : Options msg -> Debouncer -> (Debouncer, Cmd msg)
update { millis, onReady } (Debouncer debouncer) =
    let
        newDebouncer =
            D.update debouncer
    in
    ( Debouncer newDebouncer
    , Process.sleep (toFloat millis)
        |> Task.perform (always (onReady (Id newDebouncer.id)))
    )


cancel : Debouncer -> Debouncer
cancel (Debouncer debouncer) =
    Debouncer (D.cancel debouncer)


tryToApply : Id -> (() -> a) -> Debouncer -> Maybe a
tryToApply (Id incomingId) f (Debouncer debouncer) =
    D.tryToApply incomingId f debouncer
