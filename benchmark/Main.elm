module Main exposing (..)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import SweepHull
import SweepHullOld
import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import Polygon2d exposing (Polygon2d)
import Set


main : BenchmarkProgram
main =
    program sharesEdge


sweepHull : Benchmark
sweepHull =
    describe "SweepHull"
        [ -- nest as many descriptions as you like
          describe "sweepHull"
            [ benchmark "10 random points" <|
                \_ -> SweepHull.sweepHull (List.take 10 randomPoints100)
            , benchmark "20 random points" <|
                \_ -> SweepHull.sweepHull (List.take 15 randomPoints100)
            ]
        ]



{-
   sharesEdge =
       let
           tri1 =
               Triangle2d.fromVertices
                   ( (Point2d.fromCoordinates ( 1, 1 ))
                   , (Point2d.fromCoordinates ( 0, 0 ))
                   , (Point2d.fromCoordinates ( 1, 0 ))
                   )

           tri2 =
               Triangle2d.fromVertices
                   ( (Point2d.fromCoordinates ( 1, 1 ))
                   , (Point2d.fromCoordinates ( 0, 0 ))
                   , (Point2d.fromCoordinates ( 0, 1 ))
                   )

           tri3 =
               Triangle2d.fromVertices
                   ( (Point2d.fromCoordinates ( 10, 10 ))
                   , (Point2d.fromCoordinates ( 42, 52 ))
                   , (Point2d.fromCoordinates ( -1, 0 ))
                   )
       in
           describe "SweepHull"
               [ -- nest as many descriptions as you like
                 Benchmark.compare "sharesEdge shared"
                   "old"
                   (\_ -> SweepHull.sharesEdge tri1 tri2)
                   "new"
                   (\_ -> SweepHull.sharesEdge2 tri1 tri2)
               , Benchmark.compare "sharesEdge not shared"
                   "old"
                   (\_ -> SweepHull.sharesEdge tri1 tri3)
                   "new"
                   (\_ -> SweepHull.sharesEdge2 tri1 tri3)
               ]
-}


tesselation10 =
    randomPoints100
        |> List.take 10
        |> sweepHullHelper


x =
    SweepHull.flipTrianglesUntilFixpoint tesselation10
        |> List.sortBy SweepHull.hashTriangle
        |> Debug.log "new"


y =
    SweepHullOld.flipTrianglesUntilFixpointOld tesselation10
        |> List.sortBy SweepHull.hashTriangle
        |> Debug.log "old"


flipTriangles : Benchmark
flipTriangles =
    let
        tesselation15 =
            randomPoints100
                |> List.take 15
                |> sweepHullHelper
    in
        describe "SweepHull"
            [ -- nest as many descriptions as you like
              describe "sweepHull"
                [ Benchmark.compare "flipping"
                    "old"
                    (\_ -> SweepHullOld.flipTrianglesUntilFixpointOld tesselation10)
                    "new"
                    (\_ -> SweepHull.flipTrianglesUntilFixpoint tesselation10)
                ]
            ]


flipTrianglesSpeed =
    describe "SweepHull"
        [ benchmark "flipTrianglesUntilFixpoint" <|
            (\_ -> SweepHull.flipTrianglesUntilFixpoint tesselation10)
        ]


sweepHullHelper : List Point2d -> List Triangle2d
sweepHullHelper points =
    case SweepHull.initialTriangle points of
        Err _ ->
            []

        Ok initialTriangle ->
            case SweepHull.circumCircleCenter initialTriangle of
                Nothing ->
                    []

                Just center ->
                    let
                        -- Sort points by distance from the initial tri's circum-circle centre
                        insertionOrder =
                            SweepHull.sortByDistanceFromInitialTriangle points center initialTriangle

                        -- Form triangles by sequentially adding points
                        ( hull, newTriangles ) =
                            SweepHull.formTriangles initialTriangle insertionOrder
                    in
                        newTriangles


randomPoints100 =
    List.map (\( x, y ) -> Point2d.fromCoordinates ( 10 * x, 10 * y )) <|
        Set.toList <|
            Set.fromList <|
                [ ( 60, 83 )
                , ( 19, 3 )
                , ( 69, 2 )
                , ( 43, 61 )
                , ( 51, 96 )
                , ( 73, 99 )
                , ( 78, 42 )
                , ( 3, 75 )
                , ( 89, 19 )
                , ( 31, 30 )
                , ( 37, 73 )
                , ( 76, 45 )
                , ( 47, 28 )
                , ( 70, 23 )
                , ( 7, 37 )
                , ( 99, 6 )
                , ( 15, 73 )
                , ( 41, 61 )
                , ( 74, 81 )
                , ( 4, 34 )
                , ( 33, 24 )
                , ( 0, 39 )
                , ( 72, 70 )
                , ( 52, 86 )
                , ( 52, 78 )
                , ( 87, 69 )
                , ( 8, 5 )
                , ( 61, 54 )
                , ( 100, 89 )
                , ( 9, 69 )
                , ( 47, 41 )
                , ( 64, 67 )
                , ( 99, 36 )
                , ( 15, 54 )
                , ( 13, 44 )
                , ( 62, 70 )
                , ( 80, 8 )
                , ( 57, 67 )
                , ( 39, 34 )
                , ( 12, 53 )
                , ( 20, 82 )
                , ( 9, 10 )
                , ( 48, 4 )
                , ( 58, 10 )
                , ( 66, 97 )
                , ( 37, 54 )
                , ( 83, 45 )
                , ( 80, 90 )
                , ( 67, 45 )
                , ( 19, 65 )
                , ( 92, 21 )
                , ( 82, 5 )
                , ( 62, 7 )
                , ( 58, 85 )
                , ( 58, 7 )
                , ( 85, 1 )
                , ( 92, 87 )
                , ( 30, 28 )
                , ( 63, 42 )
                , ( 85, 21 )
                , ( 91, 41 )
                , ( 42, 75 )
                , ( 40, 28 )
                , ( 1, 35 )
                , ( 51, 14 )
                , ( 96, 53 )
                , ( 77, 4 )
                , ( 33, 96 )
                , ( 3, 68 )
                , ( 20, 11 )
                , ( 84, 18 )
                , ( 16, 52 )
                , ( 54, 4 )
                , ( 11, 20 )
                , ( 60, 92 )
                , ( 63, 28 )
                , ( 54, 17 )
                , ( 22, 90 )
                , ( 51, 61 )
                , ( 52, 81 )
                , ( 65, 46 )
                , ( 26, 41 )
                , ( 38, 76 )
                , ( 71, 63 )
                , ( 23, 0 )
                , ( 48, 19 )
                , ( 5, 16 )
                , ( 6, 75 )
                , ( 16, 61 )
                , ( 51, 47 )
                , ( 20, 85 )
                , ( 76, 23 )
                , ( 17, 3 )
                , ( 10, 19 )
                , ( 35, 84 )
                , ( 19, 38 )
                , ( 36, 99 )
                , ( 9, 70 )
                , ( 47, 97 )
                , ( 68, 88 )
                ]
