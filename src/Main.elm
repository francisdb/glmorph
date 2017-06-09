module Main exposing (..)

import General exposing (zipFill)
import Color exposing (..)
import Math.Vector3 exposing (vec3, Vec3, sub, scale, add)
import Math.Vector4 exposing (vec4, Vec4, setW, getW)
import Math.Matrix4 exposing (mul, makeRotate, makePerspective, makeLookAt)
import Math.Matrix4 as Mat4 exposing (Mat4)
import MathExt exposing (mix, mixColor)
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
import Keyboard exposing (..)
import Char exposing (fromCode)
import Debug as Debug
import Morph exposing (morph)


-- import Random <- totally borked

import Random.Pcg as Random
import Random as NativeRandom
import Random.List exposing (shuffle)


-- MODEL


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Time
    | Resize Window.Size
    | GeneratedObjects (List Object)
    | PickedRandom Int
    | RandomizedDestination Object
    | Presses Char


type alias Model =
    { size : Window.Size
    , theta : Float
    , morphStage : Float
    , source : Object
    , destination : Object
    , texture : Maybe Texture
    , objects : List Object
    }



-- INIT
-- TODO dont pick an index but a geometry


pickRandomCmd : Model -> Cmd Msg
pickRandomCmd model =
    Random.generate PickedRandom (randomGeometryIndex (List.length model.objects))


randomGeometryIndex geometriesLength =
    Random.int 0 (geometriesLength - 1)


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model (Window.Size 0 0) 0 0 [] [] Nothing objects
    in
        ( model
          -- does the batch order matter? used to be tho other way in the example
        , Cmd.batch
            [ Task.perform Resize Window.size

            --, Task.attempt TextureLoaded (Texture.load "texture/wood-crate.jpg")
            , pickRandomCmd model
            , NativeRandom.generate GeneratedObjects objectsGen
            ]
        )



-- VIEW


view : Model -> Html Msg
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ AnimationFrame.diffs Animate

    --        , Keyboard.downs (keyChange True)
    --        , Keyboard.ups (keyChange False)
    , Window.resizes Resize
    , Keyboard.presses (\code -> Presses (fromCode code))
    ]
        |> Sub.batch


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TextureLoaded textureResult ->
            ( { model | texture = Result.toMaybe textureResult }, Cmd.none )

        Animate dt ->
            let
                theta =
                    model.theta + dt / 1000

                morphStage =
                    min 1 (model.morphStage + dt / 3000)

                cmd =
                    if morphStage == 1 then
                        pickRandomCmd model
                    else
                        Cmd.none
            in
                ( { model | theta = theta, morphStage = morphStage }, cmd )

        Resize size ->
            ( { model | size = size }, Cmd.none )

        PickedRandom index ->
            let
                -- TODO exclude current geometry
                -- TODO we might want to shuffle the points first
                destination =
                    Maybe.withDefault [] (Array.get index (Array.fromList model.objects))

                updatedMode =
                    { model | source = model.destination, destination = destination, morphStage = 0 }
            in
                ( updatedMode, NativeRandom.generate RandomizedDestination (shuffle destination) )

        RandomizedDestination destination ->
            ( { model | destination = destination }, Cmd.none )

        GeneratedObjects objects ->
            ( { model | objects = objects }, Cmd.none )

        Presses code ->
            case code of
                ' ' ->
                    ( model, pickRandomCmd model )

                _ ->
                    ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- from https://github.com/alexeisavca/keyframes.elm/blob/1.0.0/src/Keyframes/Easing.elm


{-| Ease in and out cubically
-}
easeInOutCubic : Float -> Float
easeInOutCubic currentTime =
    let
        totalTime =
            1

        value =
            0

        change =
            1

        t =
            currentTime / (totalTime / 2)

        t2 =
            t - 2
    in
        if t < 1 then
            change / 2 * t ^ 3 + value
        else
            change / 2 * (t2 ^ 3 + 2) + value


drawable : Model -> Mesh Vertex
drawable model =
    let
        tweened =
            easeInOutCubic model.morphStage
    in
        points (morph tweened model.source model.destination)


scene : Model -> List Entity
scene model =
    [ WebGL.entityWith
        [ Blend.add Blend.srcAlpha Blend.one ]
        vertexShader
        fragmentShader
        (drawable model)
        (uniforms model)

    --, render vertexShader fragmentShader wireFrame (uniforms model)
    ]


uniforms : Model -> Uniforms
uniforms model =
    { rotation = mul (makeRotate (0.3 * model.theta) (vec3 0 1 0)) (makeRotate (0.3 * model.theta) (vec3 1 0 0))
    , perspective = makePerspective 45 (toFloat model.size.width / toFloat model.size.height) 0.01 100
    , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.9
    }



-- SHADERS


vertexShader : Shader { attr | position : Vec3, color : Vec4 } Uniforms { vcolor : Vec4, zshade : Float }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec4 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec4 vcolor;
varying float zshade;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
    zshade = ((rotation * vec4(position, 1.0)).z + 1.5 ) * 0.3;
    gl_PointSize = zshade * 9.0;
}

|]


fragmentShader : Shader {} Uniforms { vcolor : Vec4, zshade : Float }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec4 vcolor;
varying float zshade;
void main () {
    gl_FragColor = shade * zshade * vcolor;
}

|]
