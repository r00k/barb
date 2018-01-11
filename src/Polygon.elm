module Polygon exposing (mutatePolygons, randomInitialImage, Polygon, Image)

import Color exposing (Color)
import Random exposing (Generator)
import Random.Color
import Random.Extra


type alias Polygon =
    { vertices : List ( Float, Float )
    , color : Color
    }


type alias Image =
    List Polygon


maximumInitialEdgeLength : Float
maximumInitialEdgeLength =
    15


maximumVertexDisplacement : Float
maximumVertexDisplacement =
    15


maximumRGBChange : Int
maximumRGBChange =
    25


maximumAlphaChange : Float
maximumAlphaChange =
    0.1


numberOfPolygons : Int
numberOfPolygons =
    125


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


mutatePolygons : Image -> Generator Image
mutatePolygons image =
    let
        listOfGenerators =
            List.map sometimesMutate image
    in
        Random.Extra.combine listOfGenerators


sometimesMutate : Polygon -> Generator Polygon
sometimesMutate polygon =
    Random.Extra.frequency
        [ ( 92.0, Random.Extra.constant polygon )
        , ( 8.0, mutatePolygon polygon )
        ]


mutatePolygon : Polygon -> Generator Polygon
mutatePolygon polygon =
    Random.Extra.frequency
        [ ( 50.0
          , Random.map2
                Polygon
                (maybeMutateVertices polygon.vertices)
                (Random.Extra.constant polygon.color)
          )
        , ( 50.0
          , Random.map2
                Polygon
                (Random.Extra.constant polygon.vertices)
                (maybeMutateColor polygon.color)
          )
        ]


maybeMutateVertices : List ( Float, Float ) -> Generator (List ( Float, Float ))
maybeMutateVertices vertices =
    let
        listOfGenerators =
            List.map sometimesMutateVertex vertices
    in
        Random.Extra.combine listOfGenerators


maybeMutateColor : Color -> Generator Color
maybeMutateColor color =
    let
        floatGenerator =
            Random.float -maximumAlphaChange maximumAlphaChange

        intGenerator =
            Random.int -maximumRGBChange maximumRGBChange

        colorGenerator =
            Random.map4
                (adjustColor color)
                intGenerator
                intGenerator
                intGenerator
                floatGenerator
    in
        Random.Extra.frequency
            [ ( 50.0, Random.Extra.constant color )
            , ( 50.0, colorGenerator )
            ]


sometimesMutateVertex : ( Float, Float ) -> Generator ( Float, Float )
sometimesMutateVertex ( x, y ) =
    -- TODO: would be nice to also just pick x or y.
    let
        min =
            -maximumVertexDisplacement

        max =
            maximumVertexDisplacement

        vertexGenerator =
            Random.map2
                (\dx dy -> ( x + dx, y + dy ))
                (Random.float min max)
                (Random.float min max)
    in
        Random.Extra.frequency
            [ ( 50.0, Random.Extra.constant ( x, y ) )
            , ( 50.0, vertexGenerator )
            ]


adjustColor : Color -> Int -> Int -> Int -> Float -> Color
adjustColor color dr dg db da =
    let
        rgba =
            Color.toRgb color
    in
        Color.rgba
            (clamp 0 255 (rgba.red + dr))
            (clamp 0 255 (rgba.green + dg))
            (clamp 0 255 (rgba.blue + db))
            (clamp 0.0 1.0 (rgba.alpha + da))


randomInitialImage : Generator Image
randomInitialImage =
    (Random.list numberOfPolygons randomPolygon)
