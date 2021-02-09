module Geometry exposing (Object, Vertex, chooseResultWithDefault, circle, colorToComponents, colorToVec3, colorToVec4, crossPoints, cylinder, filledCube, gl_orange, gl_red, gl_yellow, line, objects, objectsGen, randomCloud, randomColor, randomColorPick, randomVec3, randomVertex, sphere, square, trianglePoints, vec3ToVec4)

import Color exposing (..)
import List.Extra exposing (uniqueBy)
import Math.Vector3 exposing (Vec3, getX, getY, getZ, toRecord, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Random
import Random.List exposing (choose)
import Set
import Sphere exposing (..)


type alias Vertex =
    { color : Vec4
    , position : Vec3
    }


type alias Object =
    List Vertex


colorToComponents : Color -> ( Float, Float, Float )
colorToComponents rawColor =
    let
        c =
            toRgb rawColor
    in
    ( toFloat c.red / 255
    , toFloat c.green / 255
    , toFloat c.blue / 255
    )


colorToVec3 : Color -> Vec3
colorToVec3 rawColor =
    let
        ( r, g, b ) =
            colorToComponents rawColor
    in
    vec3 r g b


colorToVec4 : Float -> Color -> Vec4
colorToVec4 alpha rawColor =
    let
        ( r, g, b ) =
            colorToComponents rawColor
    in
    vec4 r g b alpha


vec3ToVec4 : Float -> Vec3 -> Vec4
vec3ToVec4 alpha input =
    let
        x =
            getX input

        y =
            getY input

        z =
            getZ input

        w =
            alpha
    in
    vec4 x y z w


gl_orange =
    colorToVec4 1 orange


gl_yellow =
    colorToVec4 1 yellow


gl_red =
    colorToVec4 1 red


line : Color -> Int -> Float -> Float -> Float -> Object
line color count spacing z y =
    List.range (negate (count // 2)) (count // 2)
        |> List.map (\x -> toFloat x * spacing)
        |> List.map
            (\x -> Vertex (colorToVec4 1 color) (vec3 x y z))


square : Color -> Int -> Float -> Float -> Object
square color count spacing z =
    List.range (negate (count // 2)) (count // 2)
        |> List.map (\y -> toFloat y * spacing)
        |> List.concatMap
            (line color count spacing z)


filledCube : Color -> Int -> Float -> Object
filledCube color count spacing =
    List.range (negate (count // 2)) (count // 2)
        |> List.map (\z -> toFloat z * spacing)
        |> List.concatMap
            (square color count spacing)


circle : Int -> Float -> Float -> Object
circle count radius z =
    List.range 0 count
        |> List.map (\c -> toFloat c * 2 * pi / toFloat count)
        |> List.map
            (\r -> Vertex gl_red (vec3 (sin r * radius) (cos r * radius) z))


cylinder : Int -> Int -> Float -> Float -> Object
cylinder count rows radius spacing =
    List.range 0 rows
        |> List.map (\z -> toFloat z * spacing - (spacing * toFloat rows) / 2.0)
        |> List.concatMap
            (circle count radius)



-- TODO spiral
-- TODO sphere using circles
-- TODO random cloud


chooseResultWithDefault : a -> ( Maybe a, List a ) -> a
chooseResultWithDefault default tuple =
    case tuple of
        ( Nothing, _ ) ->
            default

        ( Just c, _ ) ->
            c


randomColorPick : List Color -> Random.Generator Vec4
randomColorPick colors =
    let
        toVec =
            \c -> colorToVec4 1 c
    in
    Random.map toVec (Random.map (chooseResultWithDefault red) (Random.List.choose colors))


randomColor : Random.Generator Vec4
randomColor =
    Random.map (vec3ToVec4 1) (randomVec3 0 1)


randomVec3 : Float -> Float -> Random.Generator Vec3
randomVec3 min max =
    Random.map3 vec3 (Random.float min max) (Random.float min max) (Random.float min max)


randomVertex : Float -> Random.Generator Vec4 -> Random.Generator Vertex
randomVertex size colorPicker =
    Random.map2 Vertex colorPicker (randomVec3 -size size)


randomCloud : Int -> Float -> Random.Generator Vec4 -> Random.Generator Object
randomCloud count size colorPicker =
    Random.list count (randomVertex size colorPicker)


trianglePoints : Float -> Object
trianglePoints pos =
    [ Vertex (vec4 1 0.5 0 1) (vec3 pos 0 0)
    , Vertex (vec4 1 1 0 1) (vec3 0 pos 0)
    , Vertex (vec4 1 -1 0 1) (vec3 0 0 pos)
    ]


crossPoints : Object
crossPoints =
    List.range -30 30
        |> List.concatMap
            (\n -> trianglePoints (toFloat n / 10.0))


sphere : Int -> Object
sphere iterations =
    let
        faces =
            divideSphere iterations octahedron

        verticesLists =
            List.map
                (\f ->
                    case f of
                        ( a, b, c ) ->
                            [ a, b, c ]
                )
                faces

        vertices =
            List.concat verticesLists

        uniqueVertices =
            uniqueBy toTuple vertices
    in
    List.map (Vertex gl_yellow) uniqueVertices


objects : List Object
objects =
    [ crossPoints
    , filledCube orange 10 0.2

    --, circle 30 1 0
    , cylinder 30 10 1 0.2

    --, trianglePoints 1
    --, randomCloud 100
    , sphere 3

    --, []
    ]


objectsGen : Random.Generator (List Object)
objectsGen =
    Random.map (\g -> g :: objects) (randomCloud 1000 1 (randomColorPick [ red, orange, yellow ]))



-- used to be in the core Library
-- got it from https://github.com/johnpmayer/elm-linear-algebra/blob/c60cc6f411ff6ccf151ee5789ab826690382a1d5/src/Math/Vector3.elm


{-| Convert a vector to a tuple.
-}
toTuple : Vec3 -> ( Float, Float, Float )
toTuple v =
    let
        record =
            toRecord v
    in
    ( record.x, record.y, record.z )
