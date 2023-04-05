module Widget.Events exposing (view)


import Html as H
import Html.Attributes as HA


view : List Int -> H.Html msg
view =
    H.div [ HA.class "events" ] << List.map viewEvent


viewEvent : Int -> H.Html msg
viewEvent n =
    H.div [ HA.class <| "events__event events__event--color" ++ toColor n ] []


toColor : Int -> String
toColor =
    String.fromInt << modBy 11
