module Debouncer exposing
    ( Debouncer
    , init
    , Config, SimpleConfigOptions, trailing, leading, throttle, CustomConfigOptions, custom
    , call, Msg, update, cancel
    )

{-| Debounce or throttle your actions.


# Debouncer

@docs Debouncer


# Constructors

@docs init


# Configurations

@docs Config, SimpleConfigOptions, trailing, leading, throttle, CustomConfigOptions, custom


# Operations

@docs call, Msg, update, cancel

-}

import Process
import Task


{-| The type variable `a` represents the type of the argument the action depends
upon.

For e.g. if you're debouncing a search field, your ability to perform the search
will most likely depend upon a query string. So, in this case, `a` will be
`String`.

-}
type Debouncer a
    = Debouncer (State a)


type alias State a =
    { waitId : Int
    , maxWaitId : Int
    , numCalls : Int
    , lastArg : Maybe a
    }


{-| Create a debouncer.
-}
init : Debouncer a
init =
    Debouncer <| State 0 0 0 Nothing


{-| The configuration for a debouncer.
-}
type Config a msg
    = Config
        { invokeOnLeading : Bool
        , invokeOnTrailing : Bool
        , wait : Int
        , maxWait : Maybe Int
        , onReady : a -> msg
        , onChange : Msg -> msg
        }


{-| The configuration options used by the [`trailing`](#trailing),
[`leading`](#leading), and [`throttle`](#throttle) configuration constructors.

  - `wait`: A non-negative (`>= 0`) integer that represents the number of
    milliseconds to delay. Its exact meaning depends on the configuration
    constructor you've used.
  - `onReady`: The message constructor to use to create the message that will be
    sent to your update function when it's time to perform the action you've been
    delaying.
  - `onChange`: Convert [`Msg`](#Msg) to your message type. Basically, say
    how to `Cmd.map` the commands that will be returned from [`call`](#call) and
    [`update`](#update).

-}
type alias SimpleConfigOptions a msg =
    { wait : Int
    , onReady : a -> msg
    , onChange : Msg -> msg
    }


{-| Create the configuration for a trailing edge debouncer.

Suppose `wait = 400ms`, then you can expect the following behaviour:

```txt
--aaa--a-a--a-----b---b-bb--b-------c-------
-----------------a---------------b-------c--
```

**Note:** Each `-` represents `100ms`.

-}
trailing : SimpleConfigOptions a msg -> Config a msg
trailing { wait, onReady, onChange } =
    custom
        { invokeOnLeading = False
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Nothing
        , onReady = onReady
        , onChange = onChange
        }


{-| Create the configuration for a leading edge debouncer.

Suppose `wait = 400ms`, then you can expect the following behaviour:

```txt
--aaa--a-a--a-----b---b-bb--b-------c-------
--a---------------b-----------------c-------
```

**Note:** Each `-` represents `100ms`.

-}
leading : SimpleConfigOptions a msg -> Config a msg
leading { wait, onReady, onChange } =
    custom
        { invokeOnLeading = True
        , invokeOnTrailing = False
        , wait = wait
        , maxWait = Nothing
        , onReady = onReady
        , onChange = onChange
        }


{-| Create the configuration for a throttler.

Suppose `wait = 400ms`, then you can expect the following behaviour:

```txt
--aaa--a-a--a-----b---b-bb--b-------c-------
--a---a----a----a-b---b----b----b---c-------
```

**Note:** Each `-` represents `100ms`.

-}
throttle : SimpleConfigOptions a msg -> Config a msg
throttle { wait, onReady, onChange } =
    custom
        { invokeOnLeading = True
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Just wait
        , onReady = onReady
        , onChange = onChange
        }


{-| The configuration options used by the [custom](#custom) configuration
constructor.

  - `invokeOnLeading`: Whether or not to send the `onReady` constructed message
    immediately upon calling [`call`](#call).
  - `invokeOnTrailing`: Whether or not to send the `onReady` constructed message
    `wait` milliseconds after you last called [`call`](#call).
  - `maxWait`: If it's `Just n` then `n` is a non-negative integer (`>= wait`)
    that represents the maximum number of milliseconds to delay. Once that time is
    up the `onReady` constructed message is sent.

**Note:** `wait`, `onReady`, and `onChange` are the same as in
[`SimpleConfigOptions`](#SimpleConfigOptions).

-}
type alias CustomConfigOptions a msg =
    { invokeOnLeading : Bool
    , invokeOnTrailing : Bool
    , wait : Int
    , maxWait : Maybe Int
    , onReady : a -> msg
    , onChange : Msg -> msg
    }


{-| Create a fully customized configuration.
-}
custom : CustomConfigOptions a msg -> Config a msg
custom { invokeOnTrailing, invokeOnLeading, wait, maxWait, onReady, onChange } =
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


{-| Call this function instead of performing the action which you want to
delay. It will update the debouncer and determine when to send the `onReady arg`
message.

  - `onReady`: The message constructor you set when creating your configuration.
  - `arg`: The argument, of type `a`, you passed to [`call`](#call).

You handle the `onReady arg` message by performing the action which you've been
delaying.

-}
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


{-| The messages that are handled by the [`update`](#update) function.
-}
type Msg
    = WaitTimerExpired Int
    | MaxWaitTimerExpired Int


{-| Update the debouncer and determine when to send the `onReady lastArg`
message.

  - `onReady`: The message constructor you set when creating your configuration.
  - `lastArg`: The argument, of type `a`, that you passed the last time you
    invoked [`call`](#call).

You handle the `onReady lastArg` message by performing the action which you've
been delaying.

-}
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


{-| It will reset the debouncer and you will not receive any previously
scheduled `onReady` constructed messages.
-}
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


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
