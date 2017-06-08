module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import MathExt exposing (mixColor)
import Math.Vector4 exposing (vec4, Vec4)


suite : Test
suite =
    describe "The MathExt module"
        [ describe "mixColor"
            -- Nest as many descriptions as you like.
            [ test "correctly interpolates" <|
                \_ ->
                    let
                        src =
                            vec4 0 0 0 0

                        dst =
                            vec4 1 1 1 1
                    in
                        Expect.equal (vec4 0.5 0.5 0.5 0.5) (mixColor 0.5 src dst)
            ]
        ]
