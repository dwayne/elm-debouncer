module InfiniteScroll exposing (main)

import Browser as B
import Debouncer exposing (Debouncer)
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
    , debouncer : Debouncer ScrollEvent
    }


type alias ScrollEvent =
    { sceneHeight : Int
    , viewportY : Int
    , viewportHeight : Int
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { blocks = blocksPerPage
      , debouncer = Debouncer.init
      }
    , Cmd.none
    )


blocksPerPage : List Int
blocksPerPage =
    [ 8, 7, 6, 5, 4, 3, 2, 1 ]



-- UPDATE


type Msg
    = Scrolled ScrollEvent
    | ReadyToCheck ScrollEvent
    | ChangedDebouncer Debouncer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scrolled event ->
            let
                ( debouncer, cmd ) =
                    Debouncer.call debouncerConfig event model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )

        ReadyToCheck { sceneHeight, viewportY, viewportHeight } ->
            let
                distance =
                    sceneHeight - viewportY - viewportHeight
            in
            if distance < 200 then
                ( { model | blocks = List.append blocksPerPage model.blocks }
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        ChangedDebouncer debouncerMsg ->
            let
                ( debouncer, cmd ) =
                    Debouncer.update debouncerConfig debouncerMsg model.debouncer
            in
            ( { model | debouncer = debouncer }
            , cmd
            )


debouncerConfig : Debouncer.Config ScrollEvent Msg
debouncerConfig =
    Debouncer.throttle
        { wait = 300
        , onReady = ReadyToCheck
        , onChange = ChangedDebouncer
        }



-- VIEW


view : Model -> H.Html Msg
view model =
    let
        header =
            H.h1 [] [ H.text "Infinite Scroll Throttled" ]

        blocks =
            List.reverse model.blocks
    in
    H.div
        [ HA.class "content"
        , onScroll Scrolled
        ]
        (header :: List.map viewBlock blocks)


onScroll : (ScrollEvent -> msg) -> H.Attribute msg
onScroll toMsg =
    let
        scrollEventDecoder =
            JD.field "target" <|
                JD.map3 ScrollEvent
                    (JD.field "scrollHeight" JD.int)
                    (JD.field "scrollTop" JD.int)
                    (JD.field "clientHeight" JD.int)
    in
    HE.on "scroll" (JD.map toMsg scrollEventDecoder)


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
