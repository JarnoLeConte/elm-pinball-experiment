module Meshes exposing
  ( Attributes
  , block
  )

import Block3d exposing (Block3d)
import Length exposing (Meters)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Coordinates exposing (BodyCoordinates)
import Point3d


type alias Attributes =
  { position : Vec3
  , normal : Vec3
  }

block : Block3d Meters BodyCoordinates -> List ( Attributes, Attributes, Attributes )
block block3d =
  let
    ( sizeX, sizeY, sizeZ ) = Block3d.dimensions block3d
    blockFrame3d = Block3d.axes block3d

    x = Length.inMeters sizeX * 0.5
    y = Length.inMeters sizeY * 0.5
    z = Length.inMeters sizeZ * 0.5

    transform px py pz =
      Point3d.placeIn blockFrame3d (Point3d.meters px py pz)
        |> Point3d.toMeters
        |> Vec3.fromRecord

    v0 = transform -x -y -z
    v1 = transform x -y -z
    v2 = transform x y -z
    v3 = transform -x y -z
    v4 = transform -x -y z
    v5 = transform x -y z
    v6 = transform x y z
    v7 = transform -x y z
  in
    [ facet v3 v2 v1
    , facet v1 v0 v3
    , facet v4 v5 v6
    , facet v6 v7 v4
    , facet v5 v4 v0
    , facet v0 v1 v5
    , facet v2 v3 v7
    , facet v7 v6 v2
    , facet v0 v4 v7
    , facet v7 v3 v0
    , facet v1 v2 v6
    , facet v6 v5 v1
    ]

facet : Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
facet a b c =
  let
    n =
      Vec3.cross (Vec3.sub b a) (Vec3.sub b c)
  in
    ( Attributes a n
    , Attributes b n
    , Attributes c n
    )
