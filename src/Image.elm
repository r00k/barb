module Image exposing (mutate, random, blank, Polygon, Image)

import Color exposing (Color)
import Random exposing (Generator)
import Random.Color
import Random.Extra


type alias Image =
    List Polygon


type alias Polygon =
    { vertices : List Vertex
    , color : Color
    }


type alias Vertex =
    ( Float, Float )


maximumInitialEdgeLength : Float
maximumInitialEdgeLength =
    25


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
    75


numberOfPolygonVertices : Int
numberOfPolygonVertices =
    4


mutationChance : Float
mutationChance =
    8.0


nonMutationChance : Float
nonMutationChance =
    100 - mutationChance


blank : Image
blank =
    []


randomPolygon : Generator Polygon
randomPolygon =
    Random.map2
        Polygon
        (Random.list
            numberOfPolygonVertices
            (Random.pair
                (Random.float -maximumInitialEdgeLength maximumInitialEdgeLength)
                (Random.float -maximumInitialEdgeLength maximumInitialEdgeLength)
            )
        )
        Random.Color.rgba


mutate : Image -> Generator Image
mutate image =
    List.map sometimesMutate image |> Random.Extra.combine


sometimesMutate : Polygon -> Generator Polygon
sometimesMutate polygon =
    Random.Extra.frequency
        [ ( nonMutationChance, Random.Extra.constant polygon )
        , ( mutationChance, mutatePolygon polygon )
        ]


mutatePolygon : Polygon -> Generator Polygon
mutatePolygon polygon =
    Random.map2
        Polygon
        (maybeMutateVertices polygon.vertices)
        (maybeMutateColor polygon.color)


maybeMutateVertices : List Vertex -> Generator (List Vertex)
maybeMutateVertices vertices =
    List.map sometimesMutateVertex vertices
        |> Random.Extra.combine


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


sometimesMutateVertex : Vertex -> Generator Vertex
sometimesMutateVertex ( x, y ) =
    let
        min =
            -maximumVertexDisplacement / 2

        max =
            maximumVertexDisplacement / 2

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


random : Generator Image
random =
    (Random.list numberOfPolygons randomPolygon)
