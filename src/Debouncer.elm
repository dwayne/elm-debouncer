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
    , toMsg : Id -> msg
    }


update : Options msg -> Debouncer -> (Debouncer, Cmd msg)
update { millis, toMsg } (Debouncer debouncer) =
    let
        newDebouncer =
            D.update debouncer
    in
    ( Debouncer newDebouncer
    , Process.sleep (toFloat millis)
        |> Task.perform (always (toMsg (Id newDebouncer.id)))
    )


cancel : Debouncer -> Debouncer
cancel (Debouncer debouncer) =
    Debouncer (D.cancel debouncer)


tryToApply : -> Id -> (() -> a) -> Debouncer -> ( Debouncer, Maybe a )
tryToApply (Id incomingId) f (Debouncer debouncer) =
    D.tryToApply incomingId f debouncer
        |> Tuple.mapFirst Debouncer
