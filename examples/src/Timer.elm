module Timer exposing
    ( Msg
    , Timer
    , cancel
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


type alias Options msg =
    { onExpired : msg
    , onChange : Msg msg -> msg
    }


setTimeout : Options msg -> Int -> Timer -> ( Timer, Cmd msg )
setTimeout { onExpired, onChange } delay (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , sleep delay (Timeout newId onExpired)
        |> Cmd.map onChange
    )


setInterval : Options msg -> Int -> Timer -> ( Timer, Cmd msg )
setInterval { onExpired, onChange } delay (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , sleep delay (Interval delay newId onExpired)
        |> Cmd.map onChange
    )


cancel : Timer -> Timer
cancel (Timer id) =
    Timer (id + 1)


type Msg msg
    = Timeout Int msg
    | Interval Int Int msg


update : (Msg msg -> msg) -> Msg msg -> Timer -> Cmd msg
update onChange msg (Timer id) =
    case msg of
        Timeout incomingId onExpired ->
            if incomingId == id then
                dispatch onExpired

            else
                Cmd.none

        Interval delay incomingId onExpired ->
            if incomingId == id then
                Cmd.batch
                    [ dispatch onExpired
                    , sleep delay (Interval delay id onExpired)
                        |> Cmd.map onChange
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
