# elm-debouncer

Debounce or throttle your actions in Elm and take control of when and how often
they are performed.

## Examples

The best way to understand the utility of this package is to
[play with the examples](https://dwayne.github.io/elm-debouncer/).

## Usage

It takes 7 simple steps. See the annotated
[resize example](https://dwayne.github.io/elm-debouncer/resize.html) below.

```elm
module Resize exposing (main)

import Browser as B
import Browser.Events as BE
--
-- STEP 1:
--
-- Import the package.
--
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
--
-- STEP 2:
--
-- Add a debouncer.
--
    , debouncer : Debouncer Event
    }


type alias Event =
    { width : Int
    , height : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
--
-- STEP 3:
--
-- Initialize the debouncer.
--
    ( Model [] [] Debouncer.init
    , Cmd.none
    )


--
-- STEP 4:
--
-- Configure the debouncer.
--
debouncerConfig : Debouncer.Config Event Msg
debouncerConfig =
    Debouncer.trailing
        { wait = 500
        , onReady = ReadyToUseSize
        , onChange = ChangedDebouncer
        }


-- UPDATE


type Msg
    = ResizedWindow Int Int
    | ReadyToUseSize Event
    | ChangedDebouncer Debouncer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizedWindow w h ->
            let
                event =
                    Event w h
--
-- STEP 5:
--
-- Call the debouncer every time the raw event occurs.
--
                ( debouncer, cmd ) =
                    Debouncer.call debouncerConfig event model.debouncer
            in
            ( { model | raw = event :: model.raw, debouncer = debouncer }
            , cmd
            )
--
-- STEP 6:
--
-- Handle changes to the debouncer.
--
        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update debouncerConfig debouncerMsg model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )
--
-- STEP 7:
--
-- Perform the action you've been delaying.
--
        ReadyToUseSize event ->
            ( { model | debounced = event :: model.debounced }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    BE.onResize ResizedWindow



-- VIEW


view : Model -> H.Html msg
view { raw, debounced } =
    viewFrame raw debounced


viewFrame : List Event -> List Event -> H.Html msg
viewFrame raw debounced =
    H.div [ HA.class "frame" ]
        [ H.div [ HA.class "frame__item" ]
            [ viewPanel "Raw events" raw
            ]
        , H.div [ HA.class "frame__item" ]
            [ viewPanel "Debounced events" debounced
            ]
        ]


viewPanel : String -> List Event -> H.Html msg
viewPanel title events =
    H.div [ HA.class "panel" ] <|
        H.h2 [] [ H.text title ]
            :: List.map viewEvent events


viewEvent : Event -> H.Html msg
viewEvent { width, height } =
    H.p [] [ H.text <| String.fromInt width ++ " x " ++ String.fromInt height ]
```

## Resources

- [Debouncing and Throttling Explained Through Examples](https://css-tricks.com/debouncing-throttling-explained-examples/)
- Lodash's [debounce](https://lodash.com/docs/4.17.15#debounce) and
[throttle](https://lodash.com/docs/4.17.15#throttle)
