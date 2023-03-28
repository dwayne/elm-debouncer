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


tryToApply : Int -> (() -> a) -> Debouncer -> ( Debouncer, Maybe a )
tryToApply incomingId f debouncer =
    if incomingId == debouncer.id then
        ( debouncer
        , Just (f ())
        )

    else
        ( debouncer
        , Nothing
        )
