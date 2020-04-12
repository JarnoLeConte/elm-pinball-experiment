module Main exposing (main)

-- elm
import Browser
import Browser.Events
import Browser.Dom
import Json.Decode
import Html exposing (Html)
import Html.Attributes as Attr
import Task

-- elm-explorations/webgl
import WebGL exposing (Shader, Mesh, Entity)

-- elm-explorations/linear-algebra
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)

-- ianmackenzie/elm-geometry-linear-algebra-interop
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d

-- ianmackenzie/elm-geometry
import Axis3d
import Block3d
import Direction3d
import Frame3d
import Point3d

-- ianmackenzie/elm-units
import Acceleration
import Duration
import Length
import Angle

-- w0rm/elm-physics
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)

-- local imports
import Meshes exposing (Attributes)


type alias Canvas =
  { width : Float
  , height : Float
  }

type alias Camera =
  { from :
      { x : Float
      , y : Float
      , z : Float
      }
  , to :
      { x : Float
      , y : Float
      , z : Float
      }
  }

type alias Data =
  { mesh : Mesh Attributes
  , name : String
  }

type alias Model =
  { canvas : Canvas
  , camera : Camera
  , world : World Data
  , leftFlipper : Float
  , rightFlipper : Float
  }

type Command
  = LeftFlipper
  | RightFlipper
  | Launcher

keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
  Json.Decode.field "key" Json.Decode.string
    |> Json.Decode.andThen
      (\string ->
        case string of
          "ArrowLeft" ->
            Json.Decode.succeed (toMsg LeftFlipper)
          "ArrowRight" ->
            Json.Decode.succeed (toMsg RightFlipper)
          "ArrowDown" ->
            Json.Decode.succeed (toMsg Launcher)
          _ ->
            Json.Decode.fail ("Unrecognized key: " ++ string)
      )

type Msg
  = Tick Float
  | Resize Float Float
  | KeyDown Command
  | KeyUp Command


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = subscriptions
    , view = view
    }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { canvas = { width = 1, height = 1 }
    , camera =
        { from = { x = -60, y = 60, z = 40 }
        , to = { x = 0, y = -7, z = 0 }
        }
    , world = initialWorld
    , leftFlipper = 0
    , rightFlipper = 0
    }
  , Task.perform (\{ viewport } -> Resize viewport.width viewport.height) Browser.Dom.getViewport
  )

update : Msg -> Model -> Model
update msg model =
  case msg of
    Tick _ ->
      { model
        | world =
            model.world
              -- |> World.constrain (constrainCar model.steering)
              -- |> World.update (applySpeed model.speeding baseFrame)
              |> World.simulate (Duration.seconds (1 / 60))
      }

    Resize width height ->
      let { canvas } = model
      in { model |  canvas = { canvas | width = width, height = height } }

    KeyDown _ -> model

    KeyUp _ -> model

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
    , Browser.Events.onAnimationFrameDelta Tick
    , Browser.Events.onKeyDown (keyDecoder KeyDown)
    , Browser.Events.onKeyUp (keyDecoder KeyUp)
    ]

view : Model -> Html Msg
view { world, camera, canvas } =
  Html.div []
    [ WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.3 0.3 0.3 1
        ]
        [ Attr.width (round canvas.width)
        , Attr.height (round canvas.height)
        , Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        ]
        (List.map (bodyToEntity canvas camera) (World.bodies world))
    ]


initialWorld : World Data
initialWorld =
  World.empty
    |> World.withGravity (Acceleration.metersPerSecondSquared 9.80665) Direction3d.negativeZ
    |> World.add floor
    |> World.add slope

floor : Body Data
floor =
  Body.plane { name = "floor", mesh = WebGL.triangles [] }
    |> Body.moveTo (Point3d.fromMeters { x = 0, y = 0, z = -1 })

slope : Body Data
slope =
  let
    block3d =
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.meters 10
        , Length.meters 16
        , Length.meters 0.5
        )
  in
    Body.block block3d
      { name = "slope"
      , mesh = WebGL.triangles (Meshes.block block3d)
      }
      |> Body.rotateAround Axis3d.x (Angle.radians (pi / 16))
      |> Body.moveTo (Point3d.meters 0 -2 1)


-- WebGL rendering

bodyToEntity : Canvas -> Camera -> Body Data -> Entity
bodyToEntity canvas camera body =
  WebGL.entity
    vertexShader
    fragmentShader
    (Body.data body).mesh
    (uniforms canvas camera body)


type alias Uniforms =
  { camera : Mat4
  , perspective : Mat4
  , transform : Mat4
  , color : Vec3
  , lightDirection : Vec3
  }

uniforms : Canvas -> Camera -> Body data -> Uniforms
uniforms canvas camera body =
  { camera = Mat4.makeLookAt (Vec3.fromRecord camera.from) (Vec3.fromRecord camera.to) Vec3.k
  , perspective = Mat4.makePerspective 24 (canvas.width / canvas.height) 5 2000
  , transform = Frame3d.toMat4 (Body.frame body)
  , color = Vec3.vec3 0.9 0.9 0.9
  , lightDirection = Vec3.normalize (Vec3.vec3 -1 -1 -1)
  }

vertexShader : Shader Attributes Uniforms { vlighting : Float }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 normal;
    uniform mat4 camera;
    uniform mat4 perspective;
    uniform mat4 transform;
    uniform vec3 lightDirection;
    varying float vlighting;
    void main () {
      float ambientLight = 0.4;
      float directionalLight = 0.6;
      gl_Position = perspective * camera * transform * vec4(position, 1.0);
      vec4 transformedNormal = normalize(transform * vec4(normal, 0.0));
      float directional = max(dot(transformedNormal.xyz, lightDirection), 0.0);
      vlighting = ambientLight + directional * directionalLight;
    }
  |]

fragmentShader : Shader {} Uniforms { vlighting : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        varying float vlighting;
        void main () {
          gl_FragColor = vec4(vlighting * color, 1.0);
        }
    |]
