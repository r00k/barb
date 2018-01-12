module Image exposing (blank, random, mutate, Polygon, Image, Pixels)

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


type alias Pixels =
    List Int


maximumInitialEdgeLength : Float
maximumInitialEdgeLength =
    25


maximumVertexDisplacement : Float
maximumVertexDisplacement =
    15


maximumRGBChange : Int
maximumRGBChange =
    20


maximumAlphaChange : Float
maximumAlphaChange =
    0.2


numberOfPolygons : Int
numberOfPolygons =
    50


numberOfPolygonVertices : Int
numberOfPolygonVertices =
    4


mutationChance : Float
mutationChance =
    9.0


nonMutationChance : Float
nonMutationChance =
    100 - mutationChance


blank : Image
blank =
    []


random : Generator Image
random =
    (Random.list numberOfPolygons randomPolygon)


mutate : Image -> Generator Image
mutate image =
    List.map sometimesMutate image |> Random.Extra.combine


randomPolygon : Generator Polygon
randomPolygon =
    Random.map2 Polygon vertices Random.Color.rgba


vertices : Generator (List Vertex)
vertices =
    Random.list numberOfPolygonVertices vertex


vertex : Generator Vertex
vertex =
    Random.pair position position


position : Generator Float
position =
    Random.float -maximumInitialEdgeLength maximumInitialEdgeLength


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
    Random.Extra.frequency
        [ ( 50.0, Random.Extra.constant color )
        , ( 50.0, mutateColor color )
        ]


mutateColor : Color -> Generator Color
mutateColor color =
    Random.map4
        (adjustColor color)
        rgbChange
        rgbChange
        rgbChange
        alphaChange


alphaChange : Generator Float
alphaChange =
    Random.float -maximumAlphaChange maximumAlphaChange


rgbChange : Generator Int
rgbChange =
    Random.int -maximumRGBChange maximumRGBChange


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


sometimesMutateVertex : Vertex -> Generator Vertex
sometimesMutateVertex vertex =
    Random.Extra.frequency
        [ ( 50.0, Random.Extra.constant vertex )
        , ( 50.0, mutateVertex vertex )
        ]


mutateVertex : Vertex -> Generator Vertex
mutateVertex vertex =
    Random.map3
        displace
        displacement
        displacement
        (Random.Extra.constant vertex)


displace : Float -> Float -> Vertex -> Vertex
displace dx dy ( x, y ) =
    ( x + dx, y + dy )


displacement : Generator Float
displacement =
    Random.float (-maximumVertexDisplacement / 2) (maximumVertexDisplacement / 2)
