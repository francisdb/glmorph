module Geometry exposing (..)

import Math.Vector3 exposing (vec3, Vec3)
import Color exposing (..)
import Random


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


colorToVec3 : Color -> Vec3
colorToVec3 rawColor =
    let
        c =
            toRgb rawColor
    in
        vec3
            (toFloat c.red / 255)
            (toFloat c.green / 255)
            (toFloat c.blue / 255)


gl_orange =
    colorToVec3 orange


gl_yellow =
    colorToVec3 yellow


gl_red =
    colorToVec3 red


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
--floatList : Random.Generator (List Float)
--floatList =
--    Random.list 10 (Random.float 0 1)
--
--randomCloud : Int -> List Vertex
--randomCloud count =
--    List.range 0 count
--        |> List.map (\z -> Vertex gl_orange (vec3 randomFloat.generate randomFloat.generate randomFloat.generate))


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            colorToVec3 rawColor

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]


trianglePoints : Float -> List Vertex
trianglePoints pos =
    [ Vertex (vec3 1 0.5 0) (vec3 pos 0 0)
    , Vertex (vec3 1 1 0) (vec3 0 pos 0)
    , Vertex (vec3 1 -1 0) (vec3 0 0 pos)
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
