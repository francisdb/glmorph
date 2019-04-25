module Main exposing (Model, Msg(..), Uniforms, drawable, easeInOutCubic, fragmentShader, init, main, pickRandomCmd, randomGeometryIndex, scene, subscriptions, uniforms, update, vertexShader, view)

-- import AnimationFrame

import Array
import Basics exposing (negate)
import Browser exposing (Document)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyPress, onResize)
import Char exposing (fromCode)
import Color exposing (..)
import Css exposing (margin, px)
import Css.Global exposing (body, global)
import Debug as Debug
import General exposing (zipFill)
import Geometry exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Math.Matrix4 as Mat4 exposing (Mat4, makeLookAt, makePerspective, makeRotate, mul)
import Math.Vector3 exposing (Vec3, add, scale, sub, vec3)
import Math.Vector4 exposing (Vec4, getW, setW, vec4)
import MathExt exposing (mix, mixColor)
import Morph exposing (morph)
import Random
import Random.List exposing (shuffle)
import Task
import Time exposing (Posix)
import WebGL exposing (Entity, Mesh, Shader, lineLoop, points, triangles)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture as Texture exposing (Error, Texture)



-- MODEL


type alias Size =
    { width : Int
    , height : Int
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | Animate Float
    | Resize Size
    | GotViewport Viewport
    | GeneratedObjects (List Object)
    | PickedRandom Int
    | RandomizedDestination Object
    | Presses String


type alias Model =
    { size : Size
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


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model (Size 0 0) 0 0 [] [] Nothing objects
    in
    ( model
      -- does the batch order matter? used to be tho other way in the example
    , Cmd.batch
        [ Task.perform GotViewport Browser.Dom.getViewport
        , pickRandomCmd model
        , Random.generate GeneratedObjects objectsGen
        ]
    )



-- VIEW


globalStyleNode : Html.Html msg
globalStyleNode =
    global
        [ body
            [ margin (px 0)
            ]
        ]
        |> toUnstyled


view : Model -> Document Msg
view model =
    Document "morph"
        [ globalStyleNode
        , WebGL.toHtml
            [ width model.size.width
            , height model.size.height
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "background-color" "#202"
            ]
            (scene model)
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , shade : Float
    }



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map Presses (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    [ onAnimationFrameDelta Animate
    , onResize (\w h -> Resize (Size w h))
    , onKeyPress keyDecoder
    ]
        |> Sub.batch

viewPortToSize : Viewport -> Size
viewPortToSize vp = Size (round vp.viewport.width) (round vp.viewport.height)

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

        GotViewport vp ->
            ( { model | size = viewPortToSize vp }, Cmd.none )

        PickedRandom index ->
            let
                -- TODO exclude current geometry
                -- TODO we might want to shuffle the points first
                destination =
                    Maybe.withDefault [] (Array.get index (Array.fromList model.objects))

                updatedMode =
                    { model | source = model.destination, destination = destination, morphStage = 0 }
            in
            ( updatedMode, Random.generate RandomizedDestination (shuffle destination) )

        RandomizedDestination destination ->
            ( { model | destination = destination }, Cmd.none )

        GeneratedObjects objects ->
            ( { model | objects = objects }, Cmd.none )

        Presses code ->
            case code of
                " " ->
                    ( model, pickRandomCmd model )

                _ ->
                    ( model, Cmd.none )


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


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
    zshade = ((rotation * vec4(position, 1.0)).z + 1.5 ) * 0.4;
    gl_PointSize = zshade * 12.0;
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
