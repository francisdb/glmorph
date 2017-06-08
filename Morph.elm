module Morph exposing (..)

import General exposing (zipFill)
import Geometry exposing (Vertex, gl_red)
import Math.Vector3 exposing (vec3)
import Math.Vector4 exposing (setW)
import MathExt exposing (mixColor, mix)


defaultColorInvisible =
    setW 0 gl_red


center =
    (vec3 0 0 0)


morphVertex : Float -> Vertex -> Vertex -> Vertex
morphVertex interpolation source destination =
    let
        position =
            mix interpolation source.position destination.position

        color =
            mixColor interpolation source.color destination.color
    in
        Vertex color position


morph : Float -> List Vertex -> List Vertex -> List Vertex
morph interpolation source destination =
    let
        filler =
            \tuple ->
                case tuple of
                    ( Just src, Nothing ) ->
                        Vertex defaultColorInvisible src.position

                    ( Nothing, Just dest ) ->
                        Vertex defaultColorInvisible dest.position

                    _ ->
                        Vertex defaultColorInvisible center

        zipped =
            -- we want this randomized
            zipFill filler source destination
    in
        List.map (\( a, b ) -> morphVertex interpolation a b) zipped
