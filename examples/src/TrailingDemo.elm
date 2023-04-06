module TrailingDemo exposing (main)

import Browser as B
import Html as H
import Html.Attributes as HA
import Widget.Demo as Demo


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
    { rawEvents : List Int
    , debouncedEvents : List Int
    , isRunning : Bool
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { rawEvents = []
      , debouncedEvents = []
      , isRunning = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Started
    | Stopped


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Started ->
            ( { model | isRunning = True }
            , Cmd.none
            )

        Stopped ->
            ( { model | isRunning = False }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { rawEvents, debouncedEvents, isRunning } =
    H.div [ HA.class "content" ]
        [ Demo.view
            { section1 =
                { title = "Raw events over time"
                , subtitle = Nothing
                , events = rawEvents
                }
            , section2 =
                { title = "Debounced events"
                , subtitle = Just "400ms, trailing"
                , events = debouncedEvents
                }
            , isRunning = isRunning
            , onStart = Started
            , onStop = Stopped
            }
        ]
