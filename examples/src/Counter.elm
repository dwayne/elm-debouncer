module Counter exposing (main)

-- This example is based on
-- https://discourse.elm-lang.org/t/how-to-do-debouncing/8637.

import Browser as B
import Debouncer exposing (Debouncer)
import Html as H
import Html.Events as HE


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- CONSTANTS


incrDConfig : Debouncer.Config () Msg
incrDConfig =
    Debouncer.leading
        { wait = 1000
        , onReady = always ReadyToIncrement
        , onChange = ChangedIncrD
        }


decrDConfig : Debouncer.Config () Msg
decrDConfig =
    Debouncer.leading
        { wait = 5000
        , onReady = always ReadyToDecrement
        , onChange = ChangedDecrD
        }



-- MODEL


type alias Model =
    { count : Int
    , incrD : Debouncer ()
    , decrD : Debouncer ()
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { count = 0
      , incrD = Debouncer.init
      , decrD = Debouncer.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedIncrement
    | ReadyToIncrement
    | ChangedIncrD Debouncer.Msg
    | ClickedDecrement
    | ReadyToDecrement
    | ChangedDecrD Debouncer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedIncrement ->
            let
                ( incrD, cmd ) =
                    Debouncer.call incrDConfig () model.incrD
            in
            ( { model | incrD = incrD }
            , cmd
            )

        ReadyToIncrement ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        ChangedIncrD incrDMsg ->
            let
                ( incrD, cmd ) =
                    Debouncer.update incrDConfig incrDMsg model.incrD
            in
            ( { model | incrD = incrD }
            , cmd
            )

        ClickedDecrement ->
            let
                ( decrD, cmd ) =
                    Debouncer.call decrDConfig () model.decrD
            in
            ( { model | decrD = decrD }
            , cmd
            )

        ReadyToDecrement ->
            ( { model | count = model.count - 1 }
            , Cmd.none
            )

        ChangedDecrD decrDMsg ->
            let
                ( decrD, cmd ) =
                    Debouncer.update decrDConfig decrDMsg model.decrD
            in
            ( { model | decrD = decrD }
            , cmd
            )



-- VIEW


view : Model -> H.Html Msg
view { count } =
    H.div []
        [ H.button [ HE.onClick ClickedIncrement ] [ H.text "+1" ]
        , H.div [] [ H.text <| String.fromInt count ]
        , H.button [ HE.onClick ClickedDecrement ] [ H.text "-1" ]
        ]
