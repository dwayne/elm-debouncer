module Debouncer exposing
    ( Config
    , DebounceOptions
    , Debouncer
    , LeadingOptions
    , Msg
    , ThrottleOptions
    , TrailingOptions
    , call
    , cancel
    , debounce
    , init
    , leading
    , throttle
    , trailing
    , update
    )

import Process
import Task


type Debouncer a
    = Debouncer (State a)


type alias State a =
    { waitId : Int
    , maxWaitId : Int
    , numCalls : Int
    , lastArg : Maybe a
    }


init : Debouncer a
init =
    Debouncer <| State 0 0 0 Nothing


type Config a msg
    = Config
        { invokeOnLeading : Bool
        , invokeOnTrailing : Bool
        , wait : Int
        , maxWait : Maybe Int
        , onReady : a -> msg
        , onChange : Msg -> msg
        }


type alias TrailingOptions a msg =
    { wait : Int
    , onReady : a -> msg
    , onChange : Msg -> msg
    }


trailing : TrailingOptions a msg -> Config a msg
trailing { wait, onReady, onChange } =
    debounce
        { invokeOnLeading = False
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Nothing
        , onReady = onReady
        , onChange = onChange
        }


type alias LeadingOptions a msg =
    TrailingOptions a msg


leading : LeadingOptions a msg -> Config a msg
leading { wait, onReady, onChange } =
    debounce
        { invokeOnLeading = True
        , invokeOnTrailing = False
        , wait = wait
        , maxWait = Nothing
        , onReady = onReady
        , onChange = onChange
        }


type alias ThrottleOptions a msg =
    TrailingOptions a msg


throttle : ThrottleOptions a msg -> Config a msg
throttle { wait, onReady, onChange } =
    debounce
        { invokeOnLeading = True
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Just wait
        , onReady = onReady
        , onChange = onChange
        }


type alias DebounceOptions a msg =
    { invokeOnLeading : Bool
    , invokeOnTrailing : Bool
    , wait : Int
    , maxWait : Maybe Int
    , onReady : a -> msg
    , onChange : Msg -> msg
    }


debounce : DebounceOptions a msg -> Config a msg
debounce { invokeOnTrailing, invokeOnLeading, wait, maxWait, onReady, onChange } =
    let
        nonNegativeWait =
            max wait 0

        nonNegativeMaxWait =
            Maybe.map (max nonNegativeWait) maxWait
    in
    Config
        { invokeOnLeading = invokeOnLeading
        , invokeOnTrailing = invokeOnTrailing
        , wait = nonNegativeWait
        , maxWait = nonNegativeMaxWait
        , onReady = onReady
        , onChange = onChange
        }


cancel : Debouncer a -> Debouncer a
cancel (Debouncer state) =
    Debouncer <| clearState state


clearState : State a -> State a
clearState state =
    { waitId = state.waitId + 1
    , maxWaitId = state.maxWaitId + 1
    , numCalls = 0
    , lastArg = Nothing
    }


call : Config a msg -> a -> Debouncer a -> ( Debouncer a, Cmd msg )
call (Config config) arg (Debouncer state) =
    let
        newWaitId =
            state.waitId + 1

        numCalls =
            state.numCalls + 1
    in
    ( Debouncer
        { state
            | waitId = newWaitId
            , numCalls = numCalls
            , lastArg = Just arg
        }
    , Cmd.batch
        [ if config.invokeOnLeading && numCalls == 1 then
            dispatch <| config.onReady arg

          else
            Cmd.none
        , WaitTimerExpired newWaitId
            |> sleep config.wait
            |> Cmd.map config.onChange
        , case ( config.maxWait, state.lastArg ) of
            ( Just maxWait, Nothing ) ->
                MaxWaitTimerExpired state.maxWaitId
                    |> sleep maxWait
                    |> Cmd.map config.onChange

            _ ->
                Cmd.none
        ]
    )


type Msg
    = WaitTimerExpired Int
    | MaxWaitTimerExpired Int


update : Config a msg -> Msg -> Debouncer a -> ( Debouncer a, Cmd msg )
update (Config config) msg ((Debouncer state) as debouncer) =
    case msg of
        WaitTimerExpired incomingId ->
            if incomingId == state.waitId then
                ( Debouncer <| clearState state
                , let
                    shouldInvoke =
                        config.invokeOnTrailing
                            && (not config.invokeOnLeading || state.numCalls > 1)
                  in
                  if shouldInvoke then
                    state.lastArg
                        |> Maybe.map (dispatch << config.onReady)
                        |> Maybe.withDefault Cmd.none

                  else
                    Cmd.none
                )

            else
                ( debouncer
                , Cmd.none
                )

        MaxWaitTimerExpired incomingId ->
            if incomingId == state.maxWaitId then
                let
                    newMaxWaitId =
                        state.maxWaitId + 1
                in
                ( Debouncer
                    { state
                        | maxWaitId = newMaxWaitId
                        , numCalls = 1
                    }
                , if state.numCalls > 1 then
                    Cmd.batch
                        [ state.lastArg
                            |> Maybe.map (dispatch << config.onReady)
                            |> Maybe.withDefault Cmd.none
                        , case config.maxWait of
                            Just maxWait ->
                                MaxWaitTimerExpired newMaxWaitId
                                    |> sleep maxWait
                                    |> Cmd.map config.onChange

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
