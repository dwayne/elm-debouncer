module InfiniteScroll exposing (main)

import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD


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
    { blocks : List Int
    }


blocksPerPage : List Int
blocksPerPage =
    [ 8, 7, 6, 5, 4, 3, 2, 1 ]


init : () -> ( Model, Cmd msg )
init _ =
    ( { blocks = blocksPerPage
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Scrolled ScrollEvent


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Scrolled event ->
            Debug.log (Debug.toString event) <|
                ( model
                , Cmd.none
                )



-- VIEW


view : Model -> H.Html Msg
view model =
    let
        header =
            H.h1 [] [ H.text "Infinite scrolling throttled" ]

        blocks =
            List.reverse model.blocks
    in
    H.div
        [ HA.class "content"
        , onScroll Scrolled
        ]
        (header :: List.map viewBlock blocks)


viewBlock : Int -> H.Html msg
viewBlock n =
    let
        modifierName =
            String.fromInt <| modBy 4 (n - 1) + 1

        name =
            String.fromInt n
    in
    H.div
        [ HA.class <| "block block--" ++ modifierName
        ]
        [ H.text <| "Block " ++ name
        ]



-- HELPERS


type alias ScrollEvent =
    { sceneHeight : Float
    , viewportY : Float
    , viewportHeight : Float
    }


onScroll : (ScrollEvent -> msg) -> H.Attribute msg
onScroll toMsg =
    let
        scrollEventDecoder =
            JD.field "target" <|
                JD.map3 ScrollEvent
                    (JD.field "scrollHeight" JD.float)
                    (JD.field "scrollTop" JD.float)
                    (JD.field "clientHeight" JD.float)
    in
    HE.on "scroll" (JD.map toMsg scrollEventDecoder)
