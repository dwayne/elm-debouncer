module Debouncer.Internal exposing
    ( Debouncer
    , cancel
    , init
    , isReady
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


isReady : Int -> Debouncer -> Bool
isReady incomingId { id } =
    incomingId == id
