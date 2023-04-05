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



-- MODEL


type alias Model =
    { count : Int
    , incrD : Debouncer ()
    , decrD : Debouncer ()
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { count = 0
      , incrD = Debouncer.leading 1000
      , decrD = Debouncer.leading 5000
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedIncrement
    | ReadyToIncrement
    | ChangedIncrD (Debouncer.Msg Msg ())
    | ClickedDecrement
    | ReadyToDecrement
    | ChangedDecrD (Debouncer.Msg Msg ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedIncrement ->
            let
                ( incrD, cmd ) =
                    Debouncer.call
                        { onReady = always ReadyToIncrement
                        , onChange = ChangedIncrD
                        }
                        ()
                        model.incrD
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
                    Debouncer.update ChangedIncrD incrDMsg model.incrD
            in
            ( { model | incrD = incrD }
            , cmd
            )

        ClickedDecrement ->
            let
                ( decrD, cmd ) =
                    Debouncer.call
                        { onReady = always ReadyToDecrement
                        , onChange = ChangedDecrD
                        }
                        ()
                        model.decrD
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
                    Debouncer.update ChangedDecrD decrDMsg model.decrD
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
