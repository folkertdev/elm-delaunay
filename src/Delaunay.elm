module Delaunay exposing (..)

import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import Polygon2d exposing (Polygon2d)
import SweepHull


{-| Get the delaunay triangulation for a set of points
-}
delaunayTriangulation : List Point2d -> List Triangle2d
delaunayTriangulation points =
    SweepHull.sweepHull points
        |> Tuple.first


{-| Get the delaunay triangulation and the convex hull for a set of points
-}
delaunayTriangulationAndHull : List Point2d -> ( List Triangle2d, Polygon2d )
delaunayTriangulationAndHull points =
    SweepHull.sweepHull points
