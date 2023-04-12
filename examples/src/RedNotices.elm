module RedNotices exposing (main)

--
-- This example is based on https://youtu.be/PySFIsgXNZ0.
--
-- Search the Interpol database of red notices by a person's forename.
--

import Browser as B
import Html as H
import Html.Attributes as HA
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
    { query : String
    , searchResults : RemoteData (List Notice)
    }


type RemoteData a
    = Initial
    | Loading
    | Success a
    | Error


type alias Notice =
    { forename : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { query = ""
      , searchResults = Initial
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputQuery String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputQuery query ->
            ( { model | query = query }
            , Cmd.none
            )



-- VIEW


view : Model -> H.Html Msg
view { query, searchResults } =
    H.div []
        [ H.p []
            [ H.input
                [ HA.type_ "search"
                , HA.placeholder "Enter the forename to search"
                , HA.autofocus True
                , HA.value query
                , HE.onInput InputQuery
                ]
                []
            ]
        , case searchResults of
            Initial ->
                H.text ""

            Loading ->
                H.p [] [ H.text "Loading..." ]

            Success _ ->
                H.p [] [ H.text "Successfully got the notices." ]

            Error ->
                H.p [] [ H.text "Sorry, unable to retrieve the notices." ]
        ]
