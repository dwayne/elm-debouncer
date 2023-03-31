module Debouncer.Internal exposing
    ( Debouncer
    , cancel
    , init
    , tryToApply
    , update
    )


type alias Debouncer =
    { id : Int
    }


init : Debouncer
init =
    Debouncer 0


update : Debouncer -> Debouncer
update { id } =
    Debouncer (id + 1)


cancel : Debouncer -> Debouncer
cancel =
    update


tryToApply : Int -> (a -> b) -> a -> Debouncer -> Maybe b
tryToApply incomingId f arg debouncer =
    if incomingId == debouncer.id then
        Just (f arg)

    else
        Nothing
