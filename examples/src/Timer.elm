module Timer exposing
    ( Msg
    , Timer
    , cancel
    , init
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
    { onTimeout : msg
    , onChange : Msg msg -> msg
    }


setTimeout : Options msg -> Int -> Timer -> ( Timer, Cmd msg )
setTimeout { onTimeout, onChange } ms (Timer id) =
    let
        newId =
            id + 1
    in
    ( Timer newId
    , sleep ms (Timeout newId onTimeout)
        |> Cmd.map onChange
    )


cancel : Timer -> Timer
cancel (Timer id) =
    Timer (id + 1)


type Msg msg
    = Timeout Int msg


update : Msg msg -> Timer -> Cmd msg
update msg (Timer id) =
    case msg of
        Timeout incomingId onTimeout ->
            if incomingId == id then
                dispatch onTimeout

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
