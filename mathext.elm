module MathExt exposing (mix)

import Math.Vector3 exposing (Vec3, vec3, sub, scale, add)


-- we could probably do the morphing inside the shader to get better performance
-- On vertex shader you can use mix function. It does linear interpolation between two values (or vectors) based on third value between 0-1. mix(vec1,vec2,mixValue).
-- https://math.stackexchange.com/questions/105400/linear-interpolation-in-3-dimensions


mix : Float -> Vec3 -> Vec3 -> Vec3
mix interpolation v1 v2 =
    let
        direction =
            sub v2 v1
    in
        add v1 (scale interpolation direction)
