module Debouncer exposing
  ( Debounced
  , wrap, unwrap

  , Id, Delay(..)
  , delay, performDelay
  , tryToApply
  )


import Process
import Task


type Debounced a
  = Debounced (State a)


type alias State a =
  { id : Int
  , lastCommittedValue : Maybe a
  , value : a
  }


wrap : a -> Debounced a
wrap =
  Debounced << State 0 Nothing


unwrap : Debounced a -> a
unwrap (Debounced { value }) =
  value


type Id =
  Id Int


type Delay =
  Delay Int Id


delay : Int -> a -> Debounced a -> (Debounced a, Delay)
delay millis value (Debounced state) =
  let
    newState =
      { state
      | id = state.id + 1
      , value = value
      }
  in
  ( Debounced newState
  , Delay millis (Id newState.id)
  )


performDelay : (Id -> msg) -> Delay -> Cmd msg
performDelay toMsg (Delay millis id) =
  Process.sleep (toFloat millis)
    |> Task.perform (always (toMsg id))


tryToApply : Id -> (a -> b) -> Debounced a -> (Debounced a, Maybe b)
tryToApply (Id incomingId) f (Debounced state as debounced) =
  let
    isCommittable =
      incomingId == state.id && Just state.value /= state.lastCommittedValue
  in
  if isCommittable then
    ( Debounced { state | lastCommittedValue = Just state.value }
    , Just (f state.value)
    )
  else
    ( debounced
    , Nothing
    )
