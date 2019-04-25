module MathExt exposing (mix, mixColor)

import Math.Vector3 as V3
import Math.Vector4 as V4



-- we could probably do the morphing inside the shader to get better performance
-- On vertex shader you can use mix function. It does linear interpolation between two values (or vectors) based on third value between 0-1. mix(vec1,vec2,mixValue).
-- https://math.stackexchange.com/questions/105400/linear-interpolation-in-3-dimensions


mix : Float -> V3.Vec3 -> V3.Vec3 -> V3.Vec3
mix interpolation v1 v2 =
    let
        direction =
            V3.sub v2 v1
    in
    V3.add v1 (V3.scale interpolation direction)


mixColor : Float -> V4.Vec4 -> V4.Vec4 -> V4.Vec4
mixColor interpolation v1 v2 =
    let
        direction =
            V4.sub v2 v1
    in
    V4.add v1 (V4.scale interpolation direction)
