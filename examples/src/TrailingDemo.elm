module TrailingDemo exposing (main)

import Html as H
import Html.Attributes as HA
import Widget.Demo as Demo


main : H.Html msg
main =
    H.div [ HA.class "content" ]
        [ Demo.view
            { section1 =
                { title = "Raw events over time"
                , subtitle = Nothing
                , events = []
                }
            , section2 =
                { title = "Debounced events"
                , subtitle = Just "400ms, trailing"
                , events = []
                }
            , isActive = True
            }
        ]
