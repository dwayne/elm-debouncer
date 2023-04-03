module Debouncer exposing
    ( Debouncer
    , Config
    , Strategy(..)
    , trailing, custom
    , cancel

    , Options
    , debounce

    , Msg
    , update
    )


import Process
import Task


type Debouncer a
    = Debouncer Config (State a)


type alias Config =
    { strategy : Strategy
    , wait : Int
    , maxWait : Maybe Int
    }


type Strategy
    = Trailing
    -- | Leading
    -- | LeadingAndTrailing


type alias State a =
    { waitId : Int
    , maxWaitId : Int
    , lastArg : Maybe a
    }


trailing : Int -> Debouncer a
trailing wait =
    Debouncer
        { strategy = Trailing
        , wait = max wait 0
        , maxWait = Nothing
        }
        initState


-- leading : Int -> Debouncer a
-- leading wait =
--     Debouncer
--         { strategy = Leading
--         , wait = max wait 0
--         , maxWait = Nothing
--         }
--         initState
--
--
-- throttle : Strategy -> Int -> Debouncer a
-- throttle strategy wait =
--     let
--         nonNegativeWait =
--             max wait 0
--     in
--     Debouncer
--         { strategy = strategy
--         , wait = nonNegativeWait
--         , maxWait = Just nonNegativeWait
--         }
--         initState
--
--
custom : Config -> Debouncer a
custom { strategy, wait, maxWait } =
    let
        nonNegativeWait =
            max wait 0

        nonNegativeMaxWait =
            Maybe.map (max nonNegativeWait) maxWait
    in
    Debouncer
        { strategy = strategy
        , wait = nonNegativeWait
        , maxWait = nonNegativeMaxWait
        }
        initState


initState : State a
initState =
    State 0 0 Nothing


cancel : Debouncer a -> Debouncer a
cancel (Debouncer config state) =
    Debouncer config (clearState state)


clearState : State a -> State a
clearState state =
    { waitId = state.waitId + 1
    , maxWaitId = state.maxWaitId + 1
    , lastArg = Nothing
    }


type alias Options msg a =
    { onReady : a -> msg
    , onChange : Msg msg a -> msg
    }


debounce : Options msg a -> a -> Debouncer a -> (Debouncer a, Cmd msg)
debounce { onReady, onChange } arg (Debouncer config state) =
    case config.strategy of
        Trailing ->
            let
                newWaitId =
                    state.waitId + 1
            in
            ( Debouncer config
                { state
                | waitId = newWaitId
                , lastArg = Just arg
                }
            , Cmd.batch
                [ sleep config.wait (TrailingTimerExpired newWaitId onReady)
                    |> Cmd.map onChange
                , case config.maxWait of
                    Nothing ->
                        Cmd.none

                    Just maxWait ->
                        if state.lastArg == Nothing then
                            sleep maxWait (MaxWaitTimerExpired state.maxWaitId onReady)
                                |> Cmd.map onChange

                        else
                            Cmd.none
                ]
            )


type Msg msg a
    = TrailingTimerExpired Int (a -> msg)
    | MaxWaitTimerExpired Int (a -> msg)


update : Msg msg a -> Debouncer a -> (Debouncer a, Cmd msg)
update msg (Debouncer config state as debouncer) =
    case msg of
        TrailingTimerExpired incomingId onReady ->
            if incomingId == state.waitId then
                ( Debouncer config (clearState state)
                , state.lastArg
                    |> Maybe.map (dispatch << onReady)
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( debouncer
                , Cmd.none
                )

        MaxWaitTimerExpired incomingId onReady ->
            if incomingId == state.maxWaitId then
                ( Debouncer config (clearState state)
                , state.lastArg
                    |> Maybe.map (dispatch << onReady)
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( debouncer
                , Cmd.none
                )


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
