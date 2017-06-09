module Geometry exposing (..)

import Math.Vector3 exposing (vec3, Vec3, getX, getY, getZ)
import Math.Vector4 exposing (vec4, Vec4)
import Color exposing (..)
import Random
import Random.List


type alias Vertex =
    { color : Vec4
    , position : Vec3
    }


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


line : Int -> Float -> Float -> Float -> List Vertex
line count spacing z y =
    (List.range (negate (count // 2)) (count // 2))
        |> List.map (\x -> (toFloat x) * spacing)
        |> List.map
            (\x -> Vertex gl_red (vec3 x y z))


square : Int -> Float -> Float -> List Vertex
square count spacing z =
    (List.range (negate (count // 2)) (count // 2))
        |> List.map (\y -> (toFloat y) * spacing)
        |> List.concatMap
            (line count spacing z)


filledCube : Int -> Float -> List Vertex
filledCube count spacing =
    List.range (negate (count // 2)) (count // 2)
        |> List.map (\z -> (toFloat z) * spacing)
        |> List.concatMap
            (square count spacing)


circle : Int -> Float -> Float -> List Vertex
circle count radius z =
    List.range 0 count
        |> List.map (\c -> (toFloat c) * 2 * pi / (toFloat count))
        |> List.map
            (\r -> Vertex gl_red (vec3 ((sin r) * radius) ((cos r) * radius) z))


cylinder : Int -> Int -> Float -> Float -> List Vertex
cylinder count rows radius spacing =
    List.range 0 rows
        |> List.map (\z -> (toFloat z) * spacing - (spacing * toFloat rows) / 2.0)
        |> List.concatMap
            (circle count radius)



-- TODO recursive sphere
-- TODO spiral
-- TODO sphere using circles
-- TODO random cloud


ff : a -> ( Maybe a, List a ) -> a
ff default tuple =
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
        Random.map toVec (Random.map (ff red) (Random.List.choose colors))


randomColor : Random.Generator Vec4
randomColor =
    Random.map (vec3ToVec4 1) (randomVec3 0 1)


randomVec3 : Float -> Float -> Random.Generator Vec3
randomVec3 min max =
    Random.map3 vec3 (Random.float min max) (Random.float min max) (Random.float min max)


randomVertex : Float -> Random.Generator Vec4 -> Random.Generator Vertex
randomVertex size colorPicker =
    Random.map2 Vertex colorPicker (randomVec3 -size size)


randomCloud : Int -> Float -> Random.Generator Vec4 -> Random.Generator (List Vertex)
randomCloud count size colorPicker =
    Random.list count (randomVertex size colorPicker)


trianglePoints : Float -> List Vertex
trianglePoints pos =
    [ Vertex (vec4 1 0.5 0 1) (vec3 pos 0 0)
    , Vertex (vec4 1 1 0 1) (vec3 0 pos 0)
    , Vertex (vec4 1 -1 0 1) (vec3 0 0 pos)
    ]


crossPoints : List Vertex
crossPoints =
    (List.range -30 30)
        |> List.concatMap
            (\n -> trianglePoints (toFloat n / 10.0))


geometries : List (List Vertex)
geometries =
    [ crossPoints
    , filledCube 10 0.2

    --, circle 30 1 0
    , cylinder 30 10 1 0.2

    --, trianglePoints 1
    --, randomCloud 100
    ]


geometries2 : Random.Generator (List (List Vertex))
geometries2 =
    Random.map (\g -> g :: geometries) (randomCloud 1000 1 (randomColorPick [ red, orange, yellow ]))