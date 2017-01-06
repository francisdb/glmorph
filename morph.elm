module Main exposing (..)

import Color exposing (..)
import Math.Vector3 exposing (vec3, Vec3)
import Math.Matrix4 exposing (..)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Html
import Html exposing (Html)
import AnimationFrame
import Html.Attributes exposing (width, height)
import Time exposing (Time)
import Window
import Task
import Basics exposing (negate)
import Geometry exposing (..)
import Array
import WebGL.Texture as Texture exposing (Error, Texture)
import WebGL exposing (Mesh, Shader, Entity, lineLoop, points, triangles)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture as Texture exposing (Error, Texture)


-- import Random <- totally borked

import Random.Pcg as Random


-- MODEL


type Action
    = TextureLoaded (Result Error Texture)
    | Animate Time
    | Resize Window.Size
    | PickedRandom Int


type alias Model =
    { size : Window.Size
    , theta : Float
    , geometry : Maybe (List Vertex)
    , texture : Maybe Texture
    }



-- INIT


randomGeometryIndex =
    Random.int 0 ((List.length geometries) - 1)


init : ( Model, Cmd Action )
init =
    ( Model (Window.Size 0 0) 0 Nothing Nothing
      -- does the batch order matter? used to be tho other way in the example
    , Cmd.batch
        [ Task.perform Resize Window.size
          --, Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
        , Random.generate PickedRandom randomGeometryIndex
        ]
    )



-- VIEW


view : Model -> Html Action
view model =
    WebGL.toHtml
        [ width model.size.width
        , height model.size.height
        , Html.Attributes.style [ ( "display", "block" ), ( "background-color", "#202" ) ]
        ]
        (scene model)


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }


subscriptions : Model -> Sub Action
subscriptions model =
    [ AnimationFrame.diffs Animate
      --        , Keyboard.downs (keyChange True)
      --        , Keyboard.ups (keyChange False)
    , Window.resizes Resize
    ]
        |> Sub.batch


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )

        Animate dt ->
            ( { model | theta = model.theta + dt / 1000 }, Cmd.none )

        Resize size ->
            ( { model | size = size }, Cmd.none )

        PickedRandom index ->
            ( { model | geometry = Array.get index (Array.fromList geometries) }, Cmd.none )


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- MESHES - create a cube in which each vertex has a position and color


wireFrame : Mesh Vertex
wireFrame =
    lineLoop
        [ Vertex (vec3 1 0.5 0) (vec3 2 0 0)
        , Vertex (vec3 2 2 0) (vec3 0 2 0)
        , Vertex (vec3 2 -2 0) (vec3 0 0 2)
        ]


cube : Mesh Vertex
cube =
    let
        rft =
            vec3 1 1 1

        -- right, front, top
        lft =
            vec3 -1 1 1

        -- left,  front, top
        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        WebGL.triangles
            << List.concat
        <|
            [ face green rft rfb rbb rbt
              -- right
            , face blue rft rfb lfb lft
              -- front
            , face yellow rft lft lbt rbt
              -- top
            , face red rfb lfb lbb rbb
              -- bottom
            , face purple lft lfb lbb lbt
              -- left
            , face orange rbt rbb lbb lbt
              -- back
            ]


triangle : Mesh Vertex
triangle =
    case (trianglePoints 1) of
        [ first, second, third ] ->
            WebGL.triangles
                [ ( first
                  , second
                  , third
                  )
                ]

        _ ->
            points []


allPoints : List Vertex
allPoints =
    List.concat
        geometries


drawable : Model -> Mesh Vertex
drawable model =
    points (Maybe.withDefault [] model.geometry)


scene : Model -> List Entity
scene model =
    [ WebGL.entity vertexShader fragmentShader (drawable model) (uniforms model)
      --, render vertexShader fragmentShader wireFrame (uniforms model)
      --, render vertexShader fragmentShader triangle (uniforms model)
    ]


uniforms : Model -> Uniforms
uniforms model =
    { rotation = mul (makeRotate (0.3 * model.theta) (vec3 0 1 0)) (makeRotate (0.3 * model.theta) (vec3 1 0 0))
    , perspective = makePerspective 45 (toFloat model.size.width / toFloat model.size.height) 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, color : Vec3 } Uniforms { vcolor : Vec3, zshade : Float }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
varying float zshade;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
    zshade = ((rotation * vec4(position, 1.0)).z + 1.5 ) * 0.3;
    gl_PointSize = zshade * 8.0;
}

|]


fragmentShader : Shader {} Uniforms { vcolor : Vec3, zshade : Float }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
varying float zshade;
void main () {
    gl_FragColor = shade * zshade * vec4(vcolor, 1.0);
}

|]
