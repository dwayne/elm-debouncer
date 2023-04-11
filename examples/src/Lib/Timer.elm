module Lib.Timer exposing
    ( Config
    , Msg
    , Timer
    , cancel
    , config
    , init
    , setInterval
    , setTimeout
    , update
    )

import Process
import Task


type Timer
    = Timer Int


init : Timer
init =
    Timer 0


type Config msg =
    Config
        { wait : Int
        , onExpire : msg
        , onChange : Msg -> msg
        }


config
    : { wait : Int
      , onExpire : msg
      , onChange : Msg -> msg
      }
    -> Config msg
config { wait, onExpire, onChange } =
    Config
        { wait = max 0 wait
        , onExpire = onExpire
        , onChange = onChange
        }


setTimeout : Config msg -> Timer -> ( Timer, Cmd msg )
setTimeout (Config c) (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , Timeout newId
        |> sleep c.wait
        |> Cmd.map c.onChange
    )


setInterval : Config msg -> Timer -> ( Timer, Cmd msg )
setInterval (Config c) (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , Interval newId
        |> sleep c.wait
        |> Cmd.map c.onChange
    )


cancel : Timer -> Timer
cancel (Timer id) =
    Timer <| id + 1


type Msg
    = Timeout Int
    | Interval Int


update : Config msg -> Msg -> Timer -> Cmd msg
update (Config c) msg (Timer id) =
    case msg of
        Timeout incomingId ->
            if incomingId == id then
                dispatch c.onExpire

            else
                Cmd.none

        Interval incomingId ->
            if incomingId == id then
                Cmd.batch
                    [ dispatch c.onExpire
                    , Interval id
                        |> sleep c.wait
                        |> Cmd.map c.onChange
                    ]

            else
                Cmd.none


sleep : Int -> msg -> Cmd msg
sleep ms msg =
    Process.sleep (toFloat ms)
        |> Task.perform (always msg)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg
        |> Task.perform (always msg)
