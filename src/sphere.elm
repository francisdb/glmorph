module Sphere exposing (..)

import Math.Vector3 exposing (Vec3, vec3, add, normalize)


-- From https://github.com/w0rm/elm-webgl-playground/blob/master/Planet3D.elm
{- Recursively divide an octahedron to turn it into a sphere -}


divideSphere : Int -> List ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divideSphere step triangles =
    if step <= 0 then
        triangles
    else
        divideSphere (step - 1) (List.concatMap divide triangles)



{- 1
       / \
    b /___\ c
     /\   /\
    /__\ /__\
   0    a    2
-}


divide : ( Vec3, Vec3, Vec3 ) -> List ( Vec3, Vec3, Vec3 )
divide ( v0, v1, v2 ) =
    let
        a =
            add v0 v2 |> normalize

        b =
            add v0 v1 |> normalize

        c =
            add v1 v2 |> normalize
    in
        [ ( v0, b, a ), ( b, v1, c ), ( a, b, c ), ( a, c, v2 ) ]



{- Octahedron mesh -}


octahedron : List ( Vec3, Vec3, Vec3 )
octahedron =
    [ ( vec3 1 0 0, vec3 0 0 1, vec3 0 1 0 )
    , ( vec3 0 1 0, vec3 0 0 1, vec3 -1 0 0 )
    , ( vec3 -1 0 0, vec3 0 0 1, vec3 0 -1 0 )
    , ( vec3 0 -1 0, vec3 0 0 1, vec3 1 0 0 )
    , ( vec3 1 0 0, vec3 0 1 0, vec3 0 0 -1 )
    , ( vec3 0 1 0, vec3 -1 0 0, vec3 0 0 -1 )
    , ( vec3 -1 0 0, vec3 0 -1 0, vec3 0 0 -1 )
    , ( vec3 0 -1 0, vec3 1 0 0, vec3 0 0 -1 )
    ]
