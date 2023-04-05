module TrailingDemo exposing (main)


import Html as H
import Widget.Events as WE


main : H.Html msg
main =
    WE.view [ 1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 0, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 12 ]
