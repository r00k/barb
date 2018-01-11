module Polygon exposing (..)

import Color exposing (Color)
import Random exposing (Generator)
import Random.Color


type alias Polygon =
    { vertices : List ( Float, Float )
    , color : Color
    }


maximumInitialEdgeLength : Float
maximumInitialEdgeLength =
    15


randomPolygon : Generator Polygon
randomPolygon =
    let
        min =
            -maximumInitialEdgeLength

        max =
            maximumInitialEdgeLength
    in
        Random.map2
            Polygon
            (Random.list
                3
                (Random.pair
                    (Random.float min max)
                    (Random.float min max)
                )
            )
            Random.Color.rgba
