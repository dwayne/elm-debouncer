module Resize exposing (main)


import Browser as B
import Browser.Events as BE
import Debouncer exposing (Debouncer)
import Html as H
import Html.Attributes as HA


main : Program () Model Msg
main =
    B.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { raw : List Event
    , debounced : List Event
    , debouncer : Debouncer
    }


type alias Event =
    { width : Int
    , height : Int
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model [] [] Debouncer.init
    , Cmd.none
    )


-- UPDATE


type Msg
    = ResizedWindow Int Int
    | Ready Event Debouncer.Id


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ResizedWindow width height ->
            let
                event =
                    Event width height

                ( debouncer, cmd ) =
                    Debouncer.update
                        { millis = 400
                        , onReady = Ready event
                        }
                        model.debouncer
            in
            ( { model | raw = event :: model.raw, debouncer = debouncer }
            , cmd
            )

        Ready event id ->
            let
                -- This is the function we want to debounce.
                f =
                    \_ -> event :: model.debounced
            in
            case Debouncer.tryToApply id f model.debouncer of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just debounced ->
                    ( { model | debounced = debounced }
                    , Cmd.none
                    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    BE.onResize ResizedWindow


-- VIEW


view : Model -> H.Html msg
view { raw, debounced } =
    viewFrame { raw = raw, debounced = debounced }


viewFrame : { raw : List Event, debounced : List Event } -> H.Html msg
viewFrame { raw, debounced } =
    H.div [ HA.class "frame" ]
        [ H.div [ HA.class "frame__item" ]
            [ viewPanel "Raw resize events" raw
            ]
        , H.div [ HA.class "frame__item" ]
            [ viewPanel "Debounced resize events" debounced
            ]
        ]


viewPanel : String -> List Event -> H.Html msg
viewPanel title events =
    H.div [ HA.class "panel" ] <|
        H.h1 [] [ H.text title ] :: List.map viewEvent events


viewEvent : Event -> H.Html msg
viewEvent { width, height } =
    H.p [] [ H.text <| String.fromInt width ++ " x " ++ String.fromInt height ]
