module Debouncer2 exposing
    ( CallEffect
    , Config
    , CustomConfigOptions
    , Debouncer
    , Handlers
    , Msg
    , call
    , callEffect
    , custom
    , new
    , performCallEffect
    , trailing
    )

--
-- We won't want callEffect and performCallEffect exposed via the public API.
--
-- The approach would be to expose everything we need internally using an
-- internal module that isn't exposed in `elm.json` and then have a public
-- facing module that wraps everything up neatly into a bow.
--

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


new : Debouncer a
new =
    Debouncer (State 0 0 0 Nothing)


type Config
    = Config
        { invokeOnLeading : Bool
        , invokeOnTrailing : Bool
        , wait : Int
        , maxWait : Maybe Int
        }


trailing : Int -> Config
trailing wait =
    custom
        { invokeOnLeading = False
        , invokeOnTrailing = True
        , wait = wait
        , maxWait = Nothing
        }


type alias CustomConfigOptions =
    { invokeOnLeading : Bool
    , invokeOnTrailing : Bool
    , wait : Int
    , maxWait : Maybe Int
    }


custom : CustomConfigOptions -> Config
custom { invokeOnTrailing, invokeOnLeading, wait, maxWait } =
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
        }



--
-- The value of splitting call into call and callEffect is that it
-- improves unit testing. I can use callEffect in tests and I can
-- now actually test the implementation transparently.
--
-- The main question is: Is it worth it in this case?
--
-- Are there other benefits I'm just not seeing as yet?
--


type alias Handlers a msg =
    { onReady : a -> msg
    , onChange : Msg -> msg
    }


call : Handlers a msg -> Config -> a -> Debouncer a -> ( Debouncer a, Cmd msg )
call handlers config arg =
    callEffect config arg >> Tuple.mapSecond (performCallEffect handlers)


type alias CallEffect a =
    { onReady : Maybe a
    , waitTimerExpired : { waitId : Int, wait : Int }
    , maxWaitTimerExpired : Maybe { maxWaitId : Int, maxWait : Int }
    }


callEffect : Config -> a -> Debouncer a -> ( Debouncer a, CallEffect a )
callEffect (Config config) arg (Debouncer state) =
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
    , { onReady =
            if config.invokeOnLeading && numCalls == 1 then
                Just arg

            else
                Nothing
      , waitTimerExpired =
            { waitId = newWaitId
            , wait = config.wait
            }
      , maxWaitTimerExpired =
            case ( config.maxWait, state.lastArg ) of
                ( Just maxWait, Nothing ) ->
                    Just
                        { maxWaitId = state.maxWaitId
                        , maxWait = maxWait
                        }

                _ ->
                    Nothing
      }
    )


type Msg
    = WaitTimerExpired Int
    | MaxWaitTimerExpired Int


performCallEffect : Handlers a msg -> CallEffect a -> Cmd msg
performCallEffect { onReady, onChange } effect =
    Cmd.batch
        [ case effect.onReady of
            Just value ->
                dispatch (onReady value)

            Nothing ->
                Cmd.none
        , let
            { waitId, wait } =
                effect.waitTimerExpired
          in
          WaitTimerExpired waitId
            |> sleep wait
            |> Cmd.map onChange
        , case effect.maxWaitTimerExpired of
            Just { maxWaitId, maxWait } ->
                MaxWaitTimerExpired maxWaitId
                    |> sleep maxWait
                    |> Cmd.map onChange

            Nothing ->
                Cmd.none
        ]


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
