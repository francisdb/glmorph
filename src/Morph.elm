module Morph exposing (..)

import General exposing (zipFill)
import Geometry exposing (Vertex, gl_red, Object)
import Math.Vector3 as V3 exposing (vec3)
import Math.Vector4 exposing (setW)
import MathExt exposing (mixColor, mix)


defaultColorInvisible =
    setW 0 gl_red


center =
    (vec3 0 0 0)


interpolateVertex : Float -> Vertex -> Vertex -> Vertex
interpolateVertex interpolation source destination =
    let
        position =
            mix interpolation source.position destination.position

        color =
            mixColor interpolation source.color destination.color
    in
        Vertex color position


sq a =
    a * a


morph : Float -> Object -> Object -> Object
morph interpolation source destination =
    let
        filler =
            \tuple ->
                case tuple of
                    ( Just src, Nothing ) ->
                        Vertex defaultColorInvisible (V3.scale ((sq interpolation) * 3) src.position)

                    ( Nothing, Just dest ) ->
                        Vertex defaultColorInvisible (V3.scale ((sqrt (1 - interpolation)) * 3) dest.position)

                    _ ->
                        Vertex defaultColorInvisible center

        zipped =
            -- we want this randomized
            zipFill filler source destination
    in
        List.map (\( a, b ) -> interpolateVertex interpolation a b) zipped
