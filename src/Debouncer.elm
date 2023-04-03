module Debouncer exposing
    ( Debouncer
    , trailing, leading, throttle

    , Config
    , debounce

    , cancel

    , Options
    , call

    , Msg
    , update
    )


import Process
import Task


type Debouncer a
    = Debouncer Config (State a)


type alias Config =
    { invokeOnLeading : Bool
    , invokeOnTrailing : Bool
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
        { invokeOnLeading = False
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Nothing
        }


leading : Int -> Debouncer a
leading wait =
    debounce
        { invokeOnLeading = True
        , invokeOnTrailing = False
        , wait = wait
        , maxWait = Nothing
        }


throttle : Int -> Debouncer a
throttle wait =
    debounce
        { invokeOnLeading = True
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Just wait
        }


debounce : Config -> Debouncer a
debounce { invokeOnTrailing, invokeOnLeading, wait, maxWait } =
    let
        nonNegativeWait =
            max wait 0

        nonNegativeMaxWait =
            Maybe.map (max nonNegativeWait) maxWait
    in
    Debouncer
        { invokeOnLeading = invokeOnLeading
        , invokeOnTrailing = invokeOnTrailing
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


type alias Options msg a =
    { onReady : a -> msg
    , onChange : Msg msg a -> msg
    }


call : Options msg a -> a -> Debouncer a -> (Debouncer a, Cmd msg)
call { onReady, onChange } arg (Debouncer config state) =
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
            Debug.log "dispatch onReady on leading edge" <|
                dispatch (onReady arg)
          else
            Cmd.none
        , sleep config.wait (WaitTimerExpired newWaitId onReady)
            |> Cmd.map onChange
        , case (config.maxWait, state.lastArg) of
            (Just maxWait, Nothing) ->
                sleep maxWait (MaxWaitTimerExpired state.maxWaitId onReady)
                    |> Cmd.map onChange

            _ ->
                Cmd.none
        ]
    )


type Msg msg a
    = WaitTimerExpired Int (a -> msg)
    | MaxWaitTimerExpired Int (a -> msg)


update : (Msg msg a -> msg) -> Msg msg a -> Debouncer a -> (Debouncer a, Cmd msg)
update onChange msg (Debouncer config state as debouncer) =
    case msg of
        WaitTimerExpired incomingId onReady ->
            if incomingId == state.waitId then
                Debug.log "clear state" <|
                    ( Debouncer config (clearState state)
                    , let
                        shouldInvoke =
                            config.invokeOnTrailing &&
                                ((config.invokeOnLeading && state.numCalls > 1) || not config.invokeOnLeading)
                      in
                      if shouldInvoke then
                          Debug.log "dispatch onReady on trailing edge"
                              (state.lastArg
                                |> Maybe.map (dispatch << onReady)
                                |> Maybe.withDefault Cmd.none)
                      else
                          Cmd.none
                    )

            else
                ( debouncer
                , Cmd.none
                )

        MaxWaitTimerExpired incomingId onReady ->
            if incomingId == state.maxWaitId then
                let
                    newMaxWaitId =
                        state.maxWaitId + 1
                in
                ( Debouncer config
                    { state
                    | maxWaitId = newMaxWaitId
                    , numCalls = 1
                    }
                , if state.numCalls > 1 then
                      Debug.log "dispatch onReady when maxWait timer expires" <|
                          Cmd.batch
                            [ state.lastArg
                                  |> Maybe.map (dispatch << onReady)
                                  |> Maybe.withDefault Cmd.none
                            , case config.maxWait of
                                Just maxWait ->
                                    sleep maxWait (MaxWaitTimerExpired newMaxWaitId onReady)
                                        |> Cmd.map onChange

                                Nothing ->
                                    Cmd.none
                            ]
                  else
                      Cmd.none
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
