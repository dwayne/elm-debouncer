module Debouncer exposing
    ( Debouncer
    , Id
    , cancel
    , init
    , toValue
    , tryToApply
    , update
    )


import Debouncer.Advanced.Internal as D
import Process
import Task


type Debouncer a
    = Debouncer (D.Debouncer a)


init : a -> Debouncer a
init =
    Debouncer << D.init


type Id
    = Id Int


type alias Options msg =
    { millis : Int
    , toMsg : Id -> msg
    }


update : Options msg -> a -> Debouncer a -> (Debouncer a, Cmd msg)
update { millis, toMsg } value (Debouncer debouncer) =
    let
        newDebouncer =
            D.update value debouncer
    in
    ( Debouncer newDebouncer
    , Process.sleep (toFloat millis)
        |> Task.perform (always (toMsg (Id newDebouncer.id)))
    )


cancel : Debouncer a -> Debouncer a
cancel (Debouncer debouncer) =
    Debouncer (D.cancel debouncer)


tryToApply : Bool -> Id -> (a -> b) -> Debouncer a -> ( Debouncer a, Maybe b )
tryToApply ignoreLastCommittedValue (Id incomingId) f (Debouncer debouncer) =
    D.tryToApply ignoreLastCommittedValue incomingId f debouncer
        |> Tuple.mapFirst Debouncer


toValue : Debouncer a -> a
toValue (Debouncer { value }) =
    value
