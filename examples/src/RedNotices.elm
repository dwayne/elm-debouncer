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
import Http
import Json.Decode as JD
import Url.Builder as UB


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
    , searchResult : RemoteData (List Notice)
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
      , searchResult = Initial
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputQuery String
    | GotSearchResult (Result Http.Error (List Notice))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputQuery query ->
            if String.isEmpty query then
                ( { model | query = "", searchResult = Initial }
                , Cmd.none
                )

            else
                ( { model | query = query }
                , performSearch query
                )

        GotSearchResult (Ok notices) ->
            ( { model | searchResult = Success notices }
            , Cmd.none
            )

        GotSearchResult (Err _) ->
            ( { model | searchResult = Error }
            , Cmd.none
            )


performSearch : String -> Cmd Msg
performSearch query =
    Http.get
        { url = searchUrl query
        , expect = Http.expectJson GotSearchResult noticesDecoder
        }


searchUrl : String -> String
searchUrl query =
    UB.crossOrigin "https://ws-public.interpol.int"
        [ "notices", "v1", "red" ]
        [ UB.string "forename" query
        , UB.int "resultsPerPage" 200
        ]


noticesDecoder : JD.Decoder (List Notice)
noticesDecoder =
    JD.at [ "_embedded", "notices" ] (JD.list noticeDecoder)


noticeDecoder : JD.Decoder Notice
noticeDecoder =
    JD.map Notice
        (JD.field "forename" JD.string)



-- VIEW


view : Model -> H.Html Msg
view { query, searchResult } =
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
        , case searchResult of
            Initial ->
                H.text ""

            Loading ->
                H.p [] [ H.text "Loading..." ]

            Success notices ->
                notices
                    |> List.map (\notice -> H.li [] [ viewNotice notice ])
                    |> H.ul []

            Error ->
                H.p [] [ H.text "Sorry, unable to retrieve the notices." ]
        ]


viewNotice : Notice -> H.Html msg
viewNotice { forename } =
    H.text forename
