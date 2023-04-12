module RedNotices exposing (main)

--
-- This example is based on https://youtu.be/PySFIsgXNZ0.
--
-- Search the Interpol database of red notices by a person's forename.
--

import Browser as B
import Debouncer exposing (Debouncer)
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
    , debouncer : Debouncer String
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
      , debouncer = Debouncer.init
      , searchResult = Initial
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputQuery String
    | ReadyToSearch String
    | ChangedQueryDebouncer Debouncer.Msg
    | GotSearchResult (Result Http.Error (List Notice))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputQuery query ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call debouncerConfig query model.debouncer
            in
            ( { model | query = query, debouncer = debouncer }
            , cmd
            )

        ReadyToSearch rawQuery ->
            let
                query =
                    String.trim rawQuery
            in
            if String.isEmpty query then
                ( { model | searchResult = Initial }
                , Cmd.none
                )

            else
                ( { model | searchResult = Loading }
                , performSearch query
                )

        ChangedQueryDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update debouncerConfig debouncerMsg model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )

        GotSearchResult (Ok notices) ->
            ( { model | searchResult = Success notices }
            , Cmd.none
            )

        GotSearchResult (Err _) ->
            ( { model | searchResult = Error }
            , Cmd.none
            )


debouncerConfig : Debouncer.Config String Msg
debouncerConfig =
    Debouncer.trailing
        { wait = 500
        , onReady = ReadyToSearch
        , onChange = ChangedQueryDebouncer
        }


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
                viewText "Loading..."

            Success notices ->
                if List.isEmpty notices then
                    viewText "No results found."

                else
                    notices
                        |> List.map (\notice -> H.li [] [ viewNotice notice ])
                        |> H.ul []

            Error ->
                viewText "Sorry, unable to retrieve the notices."
        ]


viewText : String -> H.Html msg
viewText text =
    H.p [] [ H.text text ]


viewNotice : Notice -> H.Html msg
viewNotice { forename } =
    H.text forename
