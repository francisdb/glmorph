module MorphTests exposing (..)

import Geometry exposing (Vertex)
import Test exposing (..)
import Expect exposing (Expectation)
import MathExt exposing (mixColor)
import Math.Vector4 exposing (vec4, Vec4)
import Math.Vector3 exposing (vec3, Vec3)
import Morph exposing (morph)


suite : Test
suite =
    describe "The Morph module"
        [ describe "morph"
            -- Nest as many descriptions as you like.
            [ test "correctly interpolates" <|
                \_ ->
                    let
                        src =
                            Vertex (vec4 1 1 1 1) (vec3 0 0 0) :: []

                        dst =
                            Vertex (vec4 1 1 1 0) (vec3 2 2 2) :: []

                        morphed =
                            morph 0.5 src dst

                        expected =
                            Vertex (vec4 1 1 1 0.5) (vec3 1 1 1) :: []
                    in
                        Expect.equal expected morphed
            , test "correctly interpolates with empty dest" <|
                \_ ->
                    let
                        src =
                            Vertex (vec4 1 1 1 1) (vec3 0 0 0) :: []

                        dst =
                            []

                        morphed =
                            morph 0.5 src dst

                        expected =
                            Vertex (vec4 0.8999999761581421 0.5 0.5 0.5) (vec3 0 0 0) :: []
                    in
                        Expect.equal expected morphed
            , test "correctly interpolates with empty src" <|
                \_ ->
                    let
                        src =
                            []

                        dst =
                            Vertex (vec4 1 1 1 1) (vec3 0 0 0) :: []

                        morphed =
                            morph 0.5 src dst

                        expected =
                            Vertex (vec4 0.8999999761581421 0.5 0.5 0.5) (vec3 0 0 0) :: []
                    in
                        Expect.equal expected morphed
            ]
        ]
