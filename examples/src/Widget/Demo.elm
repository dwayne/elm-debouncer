module Widget.Demo exposing
    ( Demo
    , Section
    , view
    )

import Html as H
import Html.Attributes as HA


type alias Demo =
    { section1 : Section
    , section2 : Section
    , isActive : Bool
    }


view : Demo -> H.Html msg
view { section1, section2, isActive } =
    H.div [ HA.class "demo" ]
        [ H.div [ HA.class "demo__controls" ]
            [ viewControls isActive
            ]
        , H.div [ HA.class "demo__panel" ]
            [ viewPanel section1 section2
            ]
        ]


viewControls : Bool -> H.Html msg
viewControls isActive =
    H.div [ HA.class "controls" ]
        [ H.div [ HA.class "controls__control" ]
            [ viewButton "Trigger area" True isActive
            ]
        , H.div [ HA.class "controls__control" ]
            [ viewButton "Reset" False False
            ]
        ]


viewButton : String -> Bool -> Bool -> H.Html msg
viewButton text isPrimary isActive =
    H.button
        [ HA.class "button"
        , HA.classList
            [ ( "button--primary", isPrimary )
            , ( "button--active", isActive )
            ]
        ]
        [ H.text text ]


viewPanel : Section -> Section -> H.Html msg
viewPanel section1 section2 =
    H.div [ HA.class "panel" ]
        [ H.div [ HA.class "panel__item" ] [ viewSection section1 ]
        , H.div [ HA.class "panel__item" ] [ viewSection section2 ]
        ]


type alias Section =
    { title : String
    , subtitle : Maybe String
    , events : List Int
    }


viewSection : Section -> H.Html msg
viewSection { title, subtitle, events } =
    H.div [ HA.class "section" ]
        [ H.header [ HA.class "section__title" ]
            [ viewTitle title subtitle
            ]
        , viewEvents events
        ]


viewTitle : String -> Maybe String -> H.Html msg
viewTitle title subtitle =
    H.h2 [ HA.class "title" ] <|
        H.text title
            :: (case subtitle of
                    Nothing ->
                        []

                    Just s ->
                        [ H.text " "
                        , H.span [ HA.class "title__sub" ] [ H.text s ]
                        ]
               )


viewEvents : List Int -> H.Html msg
viewEvents =
    H.div [ HA.class "events" ] << List.map viewEvent


viewEvent : Int -> H.Html msg
viewEvent n =
    H.div [ HA.class <| "events__event events__event--color" ++ toColor n ] []


toColor : Int -> String
toColor =
    String.fromInt << modBy 11
