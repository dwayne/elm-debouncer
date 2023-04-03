module Debouncer exposing
    ( Debouncer
    , trailing, leading, throttle

    , DebounceOptions
    , Strategy(..)
    , debounce

    , cancel

    , CallOptions
    , call

    , Msg
    , update
    )


import Process
import Task


type Debouncer a
    = Debouncer Config (State a)


type alias Config =
    { invokeOnTrailing : Bool
    , invokeOnLeading : Bool
    , wait : Int
    , maxWait : Maybe Int
    }


type alias State a =
    { waitId : Int
    , maxWaitId : Int
    , numCalls : Int
    , lastArg : Maybe a
    }


initState : State a
initState =
    State 0 0 0 Nothing


trailing : Int -> Debouncer a
trailing wait =
    debounce
        { strategy = Trailing
        , wait = wait
        , maxWait = Nothing
        }


leading : Int -> Debouncer a
leading wait =
    debounce
        { strategy = Leading
        , wait = wait
        , maxWait = Nothing
        }


throttle : Int -> Debouncer a
throttle wait =
    debounce
        { strategy = LeadingAndTrailing
        , wait = wait
        , maxWait = Just wait
        }


type alias DebounceOptions =
    { strategy : Strategy
    , wait : Int
    , maxWait : Maybe Int
    }


type Strategy
    = Trailing
    | Leading
    | LeadingAndTrailing


debounce : DebounceOptions -> Debouncer a
debounce { strategy, wait, maxWait } =
    let
        nonNegativeWait =
            max wait 0

        nonNegativeMaxWait =
            Maybe.map (max nonNegativeWait) maxWait
    in
    Debouncer
        { invokeOnTrailing = strategy /= Leading
        , invokeOnLeading = strategy /= Trailing
        , wait = nonNegativeWait
        , maxWait = nonNegativeMaxWait
        }
        initState


cancel : Debouncer a -> Debouncer a
cancel (Debouncer config state) =
    Debouncer config (clearState state)


clearState : State a -> State a
clearState state =
    { waitId = state.waitId + 1
    , maxWaitId = state.maxWaitId + 1
    , numCalls = 0
    , lastArg = Nothing
    }


type alias CallOptions msg a =
    { onInvoke : a -> msg
    , onChange : Msg msg a -> msg
    }


call : CallOptions msg a -> a -> Debouncer a -> (Debouncer a, Cmd msg)
call { onInvoke, onChange } arg (Debouncer config state) =
    let
        newWaitId =
            state.waitId + 1

        numCalls =
            state.numCalls + 1
    in
    ( Debouncer config
        { state
        | waitId = newWaitId
        , numCalls = numCalls
        , lastArg = Just arg
        }
    , Cmd.batch
        [ if config.invokeOnLeading && numCalls == 1 then
            dispatch (onInvoke arg)
          else
            Cmd.none
        , sleep config.wait (WaitTimerExpired newWaitId onInvoke)
            |> Cmd.map onChange
        , case (config.maxWait, state.lastArg) of
            (Just maxWait, Nothing) ->
                sleep maxWait (MaxWaitTimerExpired state.maxWaitId onInvoke)
                    |> Cmd.map onChange

            _ ->
                Cmd.none
        ]
    )


type Msg msg a
    = WaitTimerExpired Int (a -> msg)
    | MaxWaitTimerExpired Int (a -> msg)


update : Msg msg a -> Debouncer a -> (Debouncer a, Cmd msg)
update msg (Debouncer config state as debouncer) =
    case msg of
        WaitTimerExpired incomingId onInvoke ->
            if incomingId == state.waitId then
                ( Debouncer config (clearState state)
                , if shouldInvoke config.invokeOnTrailing config.invokeOnLeading state.numCalls then
                      state.lastArg
                        |> Maybe.map (dispatch << onInvoke)
                        |> Maybe.withDefault Cmd.none
                  else
                      Cmd.none
                )

            else
                ( debouncer
                , Cmd.none
                )

        MaxWaitTimerExpired incomingId onInvoke ->
            if incomingId == state.maxWaitId then
                ( Debouncer config
                    { state
                    | waitId = state.waitId + 1
                    , maxWaitId = state.maxWaitId + 1
                    , lastArg = Nothing
                    }
                , if shouldInvoke config.invokeOnTrailing config.invokeOnLeading state.numCalls then
                      state.lastArg
                        |> Maybe.map (dispatch << onInvoke)
                        |> Maybe.withDefault Cmd.none
                  else
                      Cmd.none
                )

            else
                ( debouncer
                , Cmd.none
                )


shouldInvoke : Bool -> Bool -> Int -> Bool
shouldInvoke invokeOnTrailing invokeOnLeading numCalls =
    invokeOnTrailing &&
        ((invokeOnLeading && numCalls > 1) || not invokeOnLeading)


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
