module Debouncer.Advanced.Internal exposing
    ( Debouncer
    , cancel
    , init
    , tryToApply
    , update
    )


type alias Debouncer a =
    { id : Int
    , lastCommittedValue : Maybe a
    , value : a
    }


init : a -> Debouncer a
init =
    Debouncer 0 Nothing


update : a -> Debouncer a -> Debouncer a
update value debouncer =
    { debouncer
        | id = debouncer.id + 1
        , value = value
    }


cancel : Debouncer a -> Debouncer a
cancel debouncer =
    { debouncer
        | id = debouncer.id + 1
        , lastCommittedValue = Nothing
    }


tryToApply : Bool -> Int -> (a -> b) -> Debouncer a -> ( Debouncer a, Maybe b )
tryToApply ignoreLastCommittedValue incomingId f debouncer =
    let
        isCommittable =
            if ignoreLastCommittedValue then
                incomingId == debouncer.id

            else
                incomingId == debouncer.id && Just debouncer.value /= debouncer.lastCommittedValue
    in
    if isCommittable then
        ( { debouncer | lastCommittedValue = Just debouncer.value }
        , Just (f debouncer.value)
        )

    else
        ( debouncer
        , Nothing
        )
