module Debouncer exposing
    ( Debouncer
    , debounce
    , cancel

    , ApplyOptions
    , apply

    , Msg
    , update
    )


import Debouncer.Internal as D
import Process
import Task


type Debouncer a b
    = Debouncer D.Debouncer (a -> b) Int


debounce : (a -> b) -> Int -> Debouncer a b
debounce =
    Debouncer D.init


cancel : Debouncer a b -> Debouncer a b
cancel (Debouncer debouncer f wait) =
    Debouncer (D.cancel debouncer) f wait


type alias ApplyOptions a b msg =
    { onResult : b -> msg
    , fromMsg : Msg a b msg -> msg
    }


apply : ApplyOptions a b msg -> a -> Debouncer a b -> (Debouncer a b, Cmd msg)
apply { onResult, fromMsg } arg (Debouncer debouncer f wait) =
    let
        newDebouncer =
            D.update debouncer
    in
    ( Debouncer newDebouncer f wait
    , Process.sleep (toFloat wait)
        |> Task.perform (always (Ready newDebouncer.id arg onResult))
        |> Cmd.map fromMsg
    )


type Msg a b msg
    = Ready Int a (b -> msg)


update : (Msg a b msg -> msg) -> Msg a b msg -> Debouncer a b -> Cmd msg
update fromMsg msg (Debouncer debouncer f wait) =
    case msg of
        Ready id arg onResult ->
            case D.tryToApply id f arg debouncer of
                Just result ->
                    dispatch (onResult result)

                Nothing ->
                    Cmd.none


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
