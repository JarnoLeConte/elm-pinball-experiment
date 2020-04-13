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
import Vector3d
import Sphere3d

-- ianmackenzie/elm-units
import Acceleration
import Angle
import Duration
import Length
import Mass
import Force
import Quantity

-- w0rm/elm-physics
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Material as Material
import Physics.Shape as Shape

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
  { name : String
  , mesh : Mesh Attributes
  , color : Vec3
  }

type alias Model =
  { canvas : Canvas
  , camera : Camera
  , world : World Data
  , leftFlipper : Bool
  , rightFlipper : Bool
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
        { from = { x = 0, y = -0.9, z = 1.0 }
        , to = { x = 0, y = -0.17, z = 0 }
        }
    , world = initialWorld
    , leftFlipper = False
    , rightFlipper = False
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
              |> World.constrain constrainFlipper
              |> World.update (updateWorld model)
              |> World.simulate (Duration.seconds (1 / 60)) -- Use fixed value to avoid missing collisions
      }

    Resize width height ->
      let { canvas } = model
      in { model |  canvas = { canvas | width = width, height = height } }

    KeyDown LeftFlipper -> { model | leftFlipper = True }
    KeyDown RightFlipper -> { model | rightFlipper = True }
    KeyDown _ -> model

    KeyUp LeftFlipper -> { model | leftFlipper = False }
    KeyUp RightFlipper -> { model | rightFlipper = False }
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
    |> World.add floor
    |> World.add bottomPlate
    |> addBodies borders
    |> addBodies flippers
    |> World.add ball

addBodies : List (Body Data) -> World Data -> World Data
addBodies bodies world = List.foldr World.add world bodies


defaultColor : Vec3
defaultColor = Vec3.vec3 0.9 0.9 0.9

floor : Body Data
floor =
  Body.plane { name = "floor", mesh = WebGL.triangles [], color = defaultColor }
    |> Body.moveTo (Point3d.centimeters 0 0 0)
    |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0 })

flippers : List (Body Data)
flippers =
  let
    blocks =
      [ Block3d.centeredOn
          Frame3d.atOrigin
          ( Length.centimeters 10
          , Length.centimeters 0.5
          , Length.centimeters 2
          )
          |> Block3d.translateBy
              (Vector3d.centimeters -5 0 0)
          |> Block3d.rotateAround Axis3d.z
              (Angle.degrees 5)
          |> Block3d.translateBy
              (Vector3d.centimeters 10 0 0)
      , Block3d.centeredOn
          Frame3d.atOrigin
          ( Length.centimeters 10
          , Length.centimeters 0.5
          , Length.centimeters 2
          )
          |> Block3d.translateBy
              (Vector3d.centimeters -5 0 0)
          |> Block3d.rotateAround Axis3d.z
              (Angle.degrees -5)
          |> Block3d.translateBy
              (Vector3d.centimeters 10 0 0)
      ]

    sphere =
      Sphere3d.atOrigin (Length.centimeters 1.2)
        |> Sphere3d.translateBy
              (Vector3d.centimeters 0 0 0)

    flipperShape =
      Shape.sphere sphere :: List.map Shape.block blocks

    flipperMesh =
      WebGL.triangles (List.concatMap Meshes.block blocks ++ Meshes.sphere 2 sphere)
  in
    [ Body.compound
        flipperShape
        { name = "flipper-left"
        , mesh = flipperMesh
        , color = Vec3.vec3 0 1 0
        }
        |> Body.withMaterial (Material.custom { friction = 0.3, bounciness = 0 })
        |> Body.withBehavior (Body.dynamic (Mass.grams 500))
        |> Body.moveTo (Point3d.centimeters -10 -53 3)
    , Body.compound
        flipperShape
        { name = "flipper-right"
        , mesh = flipperMesh
        , color = Vec3.vec3 0 1 0
        }
        |> Body.withMaterial (Material.custom { friction = 0.3, bounciness = 0 })
        |> Body.withBehavior (Body.dynamic (Mass.grams 500))
        |> Body.rotateAround Axis3d.z (Angle.radians pi)
        |> Body.moveTo (Point3d.centimeters 10 -53 3)
    ]

bottomPlate : Body Data
bottomPlate =
  let
    block3d =
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.centimeters 60
        , Length.centimeters 120
        , Length.centimeters 1
        )
  in
    Body.block block3d
      { name = "bottom-plate"
      , mesh = WebGL.triangles (Meshes.block block3d)
      , color = defaultColor
      }
      |> Body.moveTo (Point3d.centimeters 0 0 -0.5)
      |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0 })

borders : List (Body Data)
borders =
  let
    color = Vec3.vec3 0 0 1
    block3d =
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.centimeters 25
        , Length.centimeters 2
        , Length.centimeters 5
        )
    sideBlock =
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.centimeters 2
        , Length.centimeters 120
        , Length.centimeters 5
        )
    topBlock =
      Block3d.centeredOn
        Frame3d.atOrigin
        ( Length.centimeters 60
        , Length.centimeters 2
        , Length.centimeters 5
        )
  in
    [ Body.block block3d
        { name = "border-bottom1"
        , mesh = WebGL.triangles (Meshes.block block3d)
        , color = color
        }
        |> Body.moveTo (Point3d.centimeters -15.5 -59 2.5)
        |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0.2 })
    , Body.block block3d
        { name = "border-bottom2"
        , mesh = WebGL.triangles (Meshes.block block3d)
        , color = color
        }
        |> Body.moveTo (Point3d.centimeters 15.5 -59 2.5)
        |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0.2 })
    , Body.block sideBlock
        { name = "border-left"
        , mesh = WebGL.triangles (Meshes.block sideBlock)
        , color = color
        }
        |> Body.moveTo (Point3d.centimeters -29 0 2.5)
        |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0.2 })
    , Body.block sideBlock
        { name = "border-left"
        , mesh = WebGL.triangles (Meshes.block sideBlock)
        , color = color
        }
        |> Body.moveTo (Point3d.centimeters 29 0 2.5)
        |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0.2 })
    , Body.block topBlock
        { name = "border-top"
        , mesh = WebGL.triangles (Meshes.block topBlock)
        , color = color
        }
        |> Body.moveTo (Point3d.centimeters 0 59 2.5)
        |> Body.withMaterial (Material.custom { friction = 0, bounciness = 0.2 })
    ]

ball : Body Data
ball =
  let
    sphere =
      Sphere3d.atOrigin (Length.centimeters 1.35)
  in
    Body.sphere sphere
      { name = "ball"
      , mesh = WebGL.triangles (Meshes.sphere 2 sphere)
      , color = Vec3.vec3 1 0 0
      }
      |> Body.withBehavior (Body.dynamic (Mass.grams 80))
      |> Body.moveTo (Point3d.centimeters -5 50 1.35)
      |> Body.withMaterial (Material.custom { friction = 0.3, bounciness = 0.9 })


-- constraints

constrainFlipper : Body Data -> Body Data -> List Constraint
constrainFlipper b1 b2 =
  let
    hingeLeftFlipper =
      Constraint.hinge
        (Axis3d.through
          (Point3d.centimeters -10.2 -53 0)
          (Direction3d.unsafe { x = 0, y = 0, z = 1 })
        )
        (Axis3d.through
          (Point3d.centimeters 0 0 0)
          (Direction3d.unsafe { x = 0, y = 0, z = -1 })
        )

    hingeRightFlipper =
      Constraint.hinge
        (Axis3d.through
          (Point3d.centimeters 10.2 -53 0)
          (Direction3d.unsafe { x = 0, y = 0, z = 1 })
        )
        (Axis3d.through
          (Point3d.centimeters 0 0 0)
          (Direction3d.unsafe { x = 0, y = 0, z = -1 })
        )
  in
    case ( (Body.data b1).name, (Body.data b2).name ) of
      ( "bottom-plate", "flipper-left" ) -> [ hingeLeftFlipper ]
      ( "bottom-plate", "flipper-right" ) -> [ hingeRightFlipper ]
      _ -> []

-- Model update

updateWorld : Model -> Body Data -> Body Data
updateWorld model body =
  case (Body.data body).name of
    "flipper-left" ->
      let
        direction =
          if model.leftFlipper then
            Direction3d.positiveY
          else
            Direction3d.negativeY

        point =
          Frame3d.originPoint (Body.frame body)
            |> Point3d.translateBy (Vector3d.centimeters 1 0 0)
      in
        body
          |> Body.applyForce (Force.newtons 10.3) direction point

    "flipper-right" ->
      let
        direction =
          if model.rightFlipper then
            Direction3d.positiveY
          else
            Direction3d.negativeY

        point =
          Frame3d.originPoint (Body.frame body)
            |> Point3d.translateBy (Vector3d.centimeters -1 0 0)
      in
        body
          |> Body.applyForce (Force.newtons 10.3) direction point

    "ball" ->
      body
        |> Body.applyForce
            (Quantity.times (Acceleration.metersPerSecondSquared 9.80665) (Mass.grams 80))
            (Direction3d.zy (Angle.degrees -175))
            (Frame3d.originPoint (Body.frame body))
    _ ->
      body


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

uniforms : Canvas -> Camera -> Body Data -> Uniforms
uniforms canvas camera body =
  { camera = Mat4.makeLookAt (Vec3.fromRecord camera.from) (Vec3.fromRecord camera.to) Vec3.j
  , perspective = Mat4.makePerspective 45 (canvas.width / canvas.height) 0.1 100
  , transform = Frame3d.toMat4 (Body.frame body)
  , color = (Body.data body).color
  , lightDirection = Vec3.normalize (Vec3.vec3 0 -1 -1)
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
