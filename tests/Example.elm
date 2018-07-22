module Example exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Test exposing (..)
import SweepHull exposing (..)
import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)
import LineSegment2d exposing (LineSegment2d)
import AllDict
import Set
import AdjacencyList
import Fuzz


epsilon =
    1.0e-6


mapBoth f ( x, y ) =
    ( f x, f y )


sortTriangle : Triangle2d -> Triangle2d
sortTriangle triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        case List.map Point2d.fromCoordinates <| List.sort <| List.map Point2d.coordinates [ p1, p2, p3 ] of
            [ q1, q2, q3 ] ->
                Triangle2d.fromVertices ( q1, q2, q3 )

            _ ->
                -- wrong, and impossible
                triangle


square =
    [ ( 1, 1 ), ( 0, 0 ), ( 1, 0 ), ( 0, 1 ) ]
        |> List.map Point2d.fromCoordinates


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


outlier =
    ( 0, 5 )


tri3 =
    Triangle2d.fromVertices
        ( (Point2d.fromCoordinates ( 0, 0 ))
        , (Point2d.fromCoordinates outlier)
        , (Point2d.fromCoordinates ( 1, 0 ))
        )


tri4 =
    Triangle2d.fromVertices
        ( (Point2d.fromCoordinates ( 1, 1 ))
        , (Point2d.fromCoordinates ( 1, 0 ))
        , (Point2d.fromCoordinates outlier)
        )


tri5 =
    Triangle2d.fromVertices
        ( (Point2d.fromCoordinates ( 0, 0 ))
        , (Point2d.fromCoordinates outlier)
        , (Point2d.fromCoordinates ( 1, 1 ))
        )


tri6 =
    Triangle2d.fromVertices
        ( (Point2d.fromCoordinates ( 0, 0 ))
        , (Point2d.fromCoordinates ( 1, 1 ))
        , (Point2d.fromCoordinates ( 1, 0 ))
        )



{-
   pointList =
       List.map Point2d.fromCoordinates


   radialDistanceTest =
       Test.describe "radialDistanceTest"
           [ Test.test "example 1" <|
               \_ ->
                   radialDistance (List.map Point2d.fromCoordinates [ ( 1, 0 ), ( 0, 1 ) ])
                       (Point2d.fromCoordinates ( 1, 0 ))
                       |> Expect.equal
                           [ ( 0, Point2d.fromCoordinates ( 1, 0 ) )
                           , ( 1.4142135623730951, Point2d.fromCoordinates ( 0, 1 ) )
                           ]
           , Test.test "example 2" <|
               \_ ->
                   let
                       points =
                           List.map Point2d.fromCoordinates [ ( 1, 0 ), ( 0, 42 ), ( -1, 0 ), ( 1, 0 ) ]
                   in
                       radialDistance points (Point2d.fromCoordinates ( 1, 0 ))
                           |> Expect.equal
                               [ ( 0, Point2d.fromCoordinates ( 1, 0 ) )
                               , ( 0, Point2d.fromCoordinates ( 1, 0 ) )
                               , ( 2, Point2d.fromCoordinates ( -1, 0 ) )
                               , ( 42.01190307520001, Point2d.fromCoordinates ( 0, 42 ) )
                               ]
           ]


   findSmallestCircumCircleTest =
       Test.describe "findSmallestCircumCircleTest"
           [ Test.test "example 1" <|
               \_ ->
                   findSmallestCircumCircle square
                       (Point2d.fromCoordinates ( 1, 1 ))
                       (Point2d.fromCoordinates ( 0, 0 ))
                       |> Expect.equal
                           ([ { diameter = 0.3535533905932738
                              , point = Point2d.fromCoordinates ( 1, 0 )
                              }
                            , { diameter = 0.3535533905932738
                              , point = Point2d.fromCoordinates ( 0, 1 )
                              }
                            ]
                           )
           ]


   circumCircleCenterTest =
       Test.describe "circumCircleCenterTest"
           [ Test.test "example 1" <|
               \_ ->
                   circumCircleCenter
                       (Point2d.fromCoordinates ( 0, 0 ))
                       (Point2d.fromCoordinates ( 0.5, 2 ))
                       (Point2d.fromCoordinates ( 1, 0 ))
                       |> Expect.equal (Just <| Point2d.fromCoordinates ( 0.5, 0.9375 ))
           ]



   suite =
       Test.describe "suite"
           [ Test.test "rightHandedCheck 1" <|
               \_ ->
                   rightHandedCheck
                       (Point2d.fromCoordinates ( 1, 1 ))
                       (Point2d.fromCoordinates ( 0, 0 ))
                       (Point2d.fromCoordinates ( 0, 1 ))
                       |> Expect.equal -1
           , Test.test "rightHandedCheck 2" <|
               \_ ->
                   rightHandedCheck
                       (Point2d.fromCoordinates ( 1, 1 ))
                       (Point2d.fromCoordinates ( 0, 0 ))
                       (Point2d.fromCoordinates ( 1, 0 ))
                       |> Expect.equal 1
           , Test.test "hasCommonEdge" <|
               \_ ->
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
                               , (Point2d.fromCoordinates ( 1, 0 ))
                               )

                       common =
                           LineSegment2d.fromEndpoints
                               ( (Point2d.fromCoordinates ( 1, 1 ))
                               , (Point2d.fromCoordinates ( 0, 0 ))
                               )
                   in
                       hasCommonEdge tri1 tri2
                           |> Expect.equal (Just common)
           , Test.test "calcTriangleAngle 1" <|
               \_ ->
                   calcTriangleAngle tri1
                       |> Expect.equal (Ok <| 1.5 * pi)
           , Test.test "calcTriangleAngle 2" <|
               \_ ->
                   calcTriangleAngle tri2
                       |> Expect.equal (Ok <| 0.5 * pi)
           , Test.test "checkAndFlipTrianglePair 1" <|
               \_ ->
                   checkAndFlipTrianglePair tri1 tri2
                       |> Expect.equal ( False, tri1, tri2 )
           , Test.test "checkAndFlipTrianglePair 2" <|
               \_ ->
                   checkAndFlipTrianglePair tri3 tri4
                       |> Expect.equal ( False, tri3, tri4 )
           , Test.test "checkAndFlipTrianglePair 3" <|
               \_ ->
                   checkAndFlipTrianglePair tri5 tri6
                       |> Expect.equal ( True, tri3, tri4 )
-}


randomPoints1 =
    [ ( 37, 6 )
    , ( 89, 96 )
    , ( 96, 41 )
    , ( 82, 27 )
    , ( 95, 68 )
    , ( 100, 57 )
    , ( 83, 16 )
    , ( 96, 46 )
    , ( 97, 35 )
    , ( 69, 10 )
    , ( 29, 41 )
    , ( 50, 59 )
    , ( 53, 71 )
    , ( 41, 1 )
    , ( 86, 85 )
    , ( 28, 21 )
    , ( 84, 37 )
    , ( 9, 56 )
    , ( 20, 92 )
    , ( 84, 77 )
    ]
        |> Set.fromList
        |> Set.toList
        |> List.map Point2d.fromCoordinates


fromTuplesUnordered =
    (\( p1, p2, p3 ) ->
        Triangle2d.fromVertices
            ( Point2d.fromCoordinates p1
            , Point2d.fromCoordinates p2
            , Point2d.fromCoordinates p3
            )
    )


fromTuples =
    (\( p1, p2, p3 ) ->
        sortTriangle <|
            Triangle2d.fromVertices
                ( Point2d.fromCoordinates p1
                , Point2d.fromCoordinates p2
                , Point2d.fromCoordinates p3
                )
    )


toTuples triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        ( Point2d.coordinates p1
        , Point2d.coordinates p2
        , Point2d.coordinates p3
        )


expected1 =
    List.map fromTuples
        [ ( ( 37, 6 ), ( 69, 10 ), ( 41, 1 ) )
        , ( ( 37, 6 ), ( 28, 21 ), ( 69, 10 ) )
        , ( ( 29, 41 ), ( 69, 10 ), ( 28, 21 ) )
        , ( ( 69, 10 ), ( 82, 27 ), ( 83, 16 ) )
        , ( ( 50, 59 ), ( 69, 10 ), ( 29, 41 ) )
        , ( ( 69, 10 ), ( 50, 59 ), ( 82, 27 ) )
        , ( ( 97, 35 ), ( 82, 27 ), ( 84, 37 ) )
        , ( ( 82, 27 ), ( 50, 59 ), ( 84, 37 ) )
        , ( ( 84, 77 ), ( 84, 37 ), ( 50, 59 ) )
        , ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
        , ( ( 84, 37 ), ( 96, 41 ), ( 97, 35 ) )
        , ( ( 84, 37 ), ( 96, 46 ), ( 96, 41 ) )
        , ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
        , ( ( 9, 56 ), ( 50, 59 ), ( 29, 41 ) )
        , ( ( 84, 77 ), ( 50, 59 ), ( 53, 71 ) )
        , ( ( 28, 21 ), ( 9, 56 ), ( 29, 41 ) )
        , ( ( 20, 92 ), ( 50, 59 ), ( 9, 56 ) )
        , ( ( 84, 37 ), ( 95, 68 ), ( 96, 46 ) )
        , ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
        , ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
        , ( ( 84, 37 ), ( 84, 77 ), ( 95, 68 ) )
        , ( ( 50, 59 ), ( 20, 92 ), ( 53, 71 ) )
        , ( ( 53, 71 ), ( 86, 85 ), ( 84, 77 ) )
        , ( ( 84, 77 ), ( 86, 85 ), ( 95, 68 ) )
        , ( ( 89, 96 ), ( 53, 71 ), ( 20, 92 ) )
        , ( ( 53, 71 ), ( 89, 96 ), ( 86, 85 ) )
        , ( ( 86, 85 ), ( 89, 96 ), ( 95, 68 ) )
        , ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
        ]



{-


   shullTest =
       Test.describe "sweep hull"
           [ Test.test "shull " <|
               \_ ->
                   shull square
                       |> List.map orderTriangle
                       |> Expect.equal (List.map orderTriangle [ tri1, tri2 ])
             {-
                , Test.test "randomPoints 1" <|
                    \_ ->
                        shull randomPoints1
                            |> List.map orderTriangle
                            |> Expect.equal (List.map orderTriangle expected1)
             -}
           , Test.test "randomPoints 2" <|
               \_ ->
                   let
                       expected =
                           List.map fromTuples
                               [ ( ( 37, 6 ), ( 89, 96 ), ( 82, 27 ) )
                               , ( ( 89, 96 ), ( 96, 41 ), ( 82, 27 ) )
                               ]
                   in
                       shull (List.take 4 randomPoints1)
                           |> List.map orderTriangle
                           |> Expect.equal expected
           , Test.test "randomPoints 3" <|
               \_ ->
                   let
                       expected =
                           List.map fromTuples
                               [ ( ( 37, 6 ), ( 95, 68 ), ( 82, 27 ) )
                               , ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                               , ( ( 95, 68 ), ( 96, 41 ), ( 82, 27 ) )
                               ]
                   in
                       shull (List.take 5 randomPoints1)
                           |> List.map orderTriangle
                           |> Expect.equal expected
           ]
-}


expectJust maybe =
    case maybe of
        Just _ ->
            Expect.pass

        Nothing ->
            Expect.fail ("I expected Nothing but got " ++ toString maybe)



{-
   flips =
       let
           p1 =
               ( fromTuplesUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
               , fromTuplesUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 41 ) )
               )

           p2 =
               ( fromTuplesUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
               , fromTuplesUnordered ( ( 100, 57 ), ( 82, 27 ), ( 37, 6 ) )
               )

           p3 =
               ( fromTuplesUnordered ( ( 82, 27 ), ( 100, 57 ), ( 96, 41 ) )
               , fromTuplesUnordered ( ( 82, 27 ), ( 100, 57 ), ( 95, 68 ) )
               )
       in
           Test.describe "flips"
               [ Test.test "flip 3" <|
                   \_ ->
                       uncurry checkAndFlipTrianglePair p3
                           |> expectJust
               , Test.test "flip 1" <|
                   \_ ->
                       uncurry checkAndFlipTrianglePair p1
                           |> expectJust
               , Test.test "flip 2" <|
                   \_ ->
                       uncurry checkAndFlipTrianglePair p2
                           |> expectJust
               , Test.test "hasCommonEdge 1" <|
                   \_ ->
                       uncurry hasCommonEdge p1
                           |> expectJust
               , Test.test "hasCommonEdge 2" <|
                   \_ ->
                       uncurry hasCommonEdge p2
                           |> expectJust
               , Test.test "hasCommonEdge 3" <|
                   \_ ->
                       uncurry hasCommonEdge p3
                           |> expectJust
               ]


   isJust m =
       case m of
           Just _ ->
               True

           Nothing ->
               False


   phase1 : Triangle2d -> ( Vector2d, Vector2d )
   phase1 triangle =
       let
           ( p1, p2, p3 ) =
               Triangle2d.vertices triangle

           v1 =
               Vector2d.from p3 p1

           v2 =
               Vector2d.from p3 p2
       in
           ( v1, v2 )


   phase2 : Triangle2d -> ( Float, Float )
   phase2 triangle =
       let
           ( v1, v2 ) =
               mapBoth Vector2d.normalize (phase1 triangle)
       in
           ( Vector2d.crossProduct v2 v1, Vector2d.dotProduct v1 v2 )


   phase3 : Triangle2d -> Float
   phase3 triangle =
       let
           ( crossProd, dotProd ) =
               phase2 triangle
                   |> Tuple.mapSecond (clamp -1 1)
       in
           if crossProd < 0 then
               2 * pi - acos dotProd
           else
               acos dotProd


   triangleAngles =
       let
           example1 =
               ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) )
       in
           Test.describe "triangle angles"
               [ Test.test "triangle angle v1" <|
                   \_ ->
                       example1
                           |> fromTuplesUnordered
                           |> phase1
                           |> Expect.equal ( Vector2d.fromComponents ( -1, 11 ), Vector2d.fromComponents ( 13, 25 ) )
               , Test.test "triangle angle v2" <|
                   \_ ->
                       example1
                           |> fromTuplesUnordered
                           |> phase2
                           |> Expect.equal ( 0.5397828912082718, 0.8418042708129002 )
               , Test.test "triangle angle v3" <|
                   \_ ->
                       example1
                           |> fromTuplesUnordered
                           |> phase3
                           |> Expect.equal 0.5701791791933415
               , Test.test "triangle angle random exp" <|
                   \_ ->
                       ( ( 95, 68 ), ( 96, 41 ), ( 96, 41 ) )
                           |> fromTuplesUnordered
                           |> phase1
                           |> mapBoth Vector2d.length
                           |> Expect.equal ( 27.018512172212592, 0.0 )
               , Test.test "triangle angle random 2" <|
                   \_ ->
                       ( ( 82, 27 ), ( 100, 57 ), ( 100, 57 ) )
                           |> fromTuplesUnordered
                           |> calcTriangleAngle
                           |> Expect.equal (Err DegenerateTriangle)
               , Test.test "triangle angle 1" <| \_ -> ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.5701791791933413)
               , Test.test "triangle angle 2" <| \_ -> ( ( 82, 27 ), ( 83, 16 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.22256647624971965)
               , Test.test "triangle angle 3" <| \_ -> ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (1.8246900583352348)
               , Test.test "triangle angle 4" <| \_ -> ( ( 100, 57 ), ( 37, 6 ), ( 95, 68 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (1.1787046606460772)
               , Test.test "triangle angle 5" <| \_ -> ( ( 37, 6 ), ( 82, 27 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (1.694197123157973)
               , Test.test "triangle angle 6" <| \_ -> ( ( 100, 57 ), ( 95, 68 ), ( 89, 96 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.06381650825792903)
               , Test.test "triangle angle 7" <| \_ -> ( ( 89, 96 ), ( 95, 68 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.2281578198340012)
               , Test.test "triangle angle 8" <| \_ -> ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.5701791791933413)
               , Test.test "triangle angle 9" <| \_ -> ( ( 82, 27 ), ( 83, 16 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.22256647624971965)
               , Test.test "triangle angle 10" <| \_ -> ( ( 95, 68 ), ( 37, 6 ), ( 89, 96 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.7350126809079457)
               , Test.test "triangle angle 11" <| \_ -> ( ( 82, 27 ), ( 95, 68 ), ( 100, 57 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (2.174545660192333)
               , Test.test "triangle angle 12" <| \_ -> ( ( 37, 6 ), ( 82, 27 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (1.694197123157973)
               , Test.test "triangle angle 13" <| \_ -> ( ( 82, 27 ), ( 100, 57 ), ( 96, 41 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (2.601173153319209)
               , Test.test "triangle angle 14" <| \_ -> ( ( 95, 68 ), ( 96, 41 ), ( 82, 27 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.47835243137865735)
               , Test.test "triangle angle 15" <| \_ -> ( ( 100, 57 ), ( 95, 68 ), ( 89, 96 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.06381650825792903)
               , Test.test "triangle angle 16" <| \_ -> ( ( 89, 96 ), ( 95, 68 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.2281578198340012)
               , Test.test "triange angle 17" <| \_ -> ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.5701791791933413)
               , Test.test "triange angle 18" <| \_ -> ( ( 82, 27 ), ( 83, 16 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.22256647624971965)
               , Test.test "triange angle 19" <| \_ -> ( ( 95, 68 ), ( 37, 6 ), ( 89, 96 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.7350126809079457)
               , Test.test "triange angle 20" <| \_ -> ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (0.38209199946215405)
               , Test.test "triangle angle 21" <| \_ -> ( ( 37, 6 ), ( 82, 27 ), ( 83, 16 ) ) |> fromTuplesUnordered |> calcTriangleAngle |> unsafeOk |> Expect.within (Absolute epsilon) (1.694197123157973)
               ]


   unsafeOk result =
       case result of
           Ok v ->
               v

           Err e ->
               Debug.crash "unsafeOk failed"


   initialTriangles =
       Test.describe "initial triangles"
           [ Test.test "initial triangle 6" <|
               \_ ->
                   List.take 6 randomPoints1
                       |> initialTriangle
                       |> Expect.equal
                           (Ok <|
                               Triangle2d.fromVertices
                                   ( Point2d.fromCoordinates ( 37, 6 )
                                   , Point2d.fromCoordinates ( 95, 68 )
                                   , Point2d.fromCoordinates ( 82, 27 )
                                   )
                           )
           , Test.test "insertion order" <|
               \_ ->
                   let
                       expected =
                           List.map Point2d.fromCoordinates [ ( 89, 96 ), ( 96, 41 ), ( 100, 57 ) ]

                       points =
                           List.take 6 randomPoints1
                   in
                       points
                           |> initialTriangle
                           |> Result.toMaybe
                           |> Maybe.andThen
                               (\initial ->
                                   Maybe.map (\x -> sortByDistanceFromInitialTriangle points x initial) (circumCircleCenter initial)
                               )
                           |> Expect.equal (Just expected)
           , Test.test "insertion order 2" <|
               \_ ->
                   let
                       expected =
                           List.map Point2d.fromCoordinates [ ( 83, 16 ), ( 96, 41 ), ( 96, 46 ), ( 97, 35 ), ( 100, 57 ), ( 95, 68 ), ( 89, 96 ) ]

                       points =
                           List.take 10 randomPoints1
                   in
                       points
                           |> initialTriangle
                           |> Result.toMaybe
                           |> Maybe.andThen
                               (\initial ->
                                   Maybe.map (\x -> sortByDistanceFromInitialTriangle points x initial) (circumCircleCenter initial)
                               )
                           |> Expect.equal (Just expected)
           , Test.test "new triangles" <|
               \_ ->
                   let
                       expected =
                           List.map fromTuplesUnordered
                               [ ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                               , ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                               , ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                               , ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) )
                               , ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                               , ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                               , ( ( 96, 41 ), ( 97, 35 ), ( 83, 16 ) )
                               , ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                               , ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) )
                               , ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                               , ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                               , ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                               ]

                       points =
                           List.take 10 randomPoints1
                   in
                       points
                           |> initialTriangle
                           |> Result.toMaybe
                           |> Maybe.andThen
                               (\initial ->
                                   Maybe.map
                                       (\x ->
                                           let
                                               order =
                                                   sortByDistanceFromInitialTriangle points x initial
                                           in
                                               formTriangles initial order
                                                   |> Tuple.second
                                       )
                                       (circumCircleCenter initial)
                               )
                           |> Maybe.map (List.sortBy Triangle2d.area)
                           |> Expect.equal (Just (List.sortBy Triangle2d.area expected))
           , Test.test "calcTriangleAngle should fail 2" <|
               \_ ->
                   let
                       one =
                           ( ( 95, 68 ), ( 96, 41 ), ( 96, 41 ) )
                               |> fromTuplesUnordered
                               |> calcTriangleAngle

                       two =
                           ( ( 96, 41 ), ( 95, 68 ), ( 100, 57 ) )
                               |> fromTuplesUnordered
                               |> calcTriangleAngle
                   in
                       Result.map2 (,) one two
                           |> Expect.equal (Err DegenerateTriangle)
           , Test.test "check and flip 2" <|
               \_ ->
                   ( ( ( 82, 27 ), ( 95, 68 ), ( 100, 57 ) ), ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) )
                       |> mapBoth fromTuplesUnordered
                       |> uncurry checkAndFlipTrianglePair
                       |> Expect.equal Nothing
           ]


   fromTupleUnordered =
       fromTuplesUnordered


   checkAndFlipTrianglePairTest : Test.Test
   checkAndFlipTrianglePairTest =
       let
           examples =
               [ ( False, fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) ), fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) ) )
               , ( True, fromTupleUnordered ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) ), fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) ), fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) ), fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) ), fromTupleUnordered ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) ), fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) ), fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) ), fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ) )
               , ( True, fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) ), fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) ) )
               , ( True, fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) ), fromTupleUnordered ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( True, fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) ), fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ), fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( False, fromTupleUnordered ( ( 95, 68 ), ( 96, 46 ), ( 37, 6 ) ), fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ) )
               , ( True, fromTupleUnordered ( ( 95, 68 ), ( 96, 46 ), ( 37, 6 ) ), fromTupleUnordered ( ( 96, 46 ), ( 82, 27 ), ( 37, 6 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) )
               , ( False, fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ), fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( False, fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ), fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ), fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) )
               , ( False, fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ), fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( False, fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ), fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) )
               , ( False, fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ), fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) ) )
               , ( False, fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ), fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) ), fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) )
               , ( False, fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ), fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ) )
               , ( False, fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ), fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ) )
               , ( False, fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ) )
               ]
       in
           Test.describe "checkAndFlipTrianglePairTest" <|
               List.indexedMap
                   (\index ( expected, tri1, tri2 ) ->
                       Test.test ("checkAndFlipTrianglePairTest #" ++ toString index) <|
                           \_ ->
                               checkAndFlipTrianglePair tri1 tri2
                                   |> (if expected then
                                           expectJust
                                       else
                                           Expect.equal Nothing
                                      )
                   )
                   examples


   flippedTests =
       let
           examples =
               [ ( fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 96, 41 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 96, 41 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 83, 16 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 83, 16 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 100, 57 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 )
               , ( fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 46 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 97, 35 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 69, 10 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 95, 68 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 100, 57 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 100, 57 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 100, 57 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 97, 35 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 100, 57 ), ( 97, 35 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 41 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 96, 46 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 95, 68 ), ( 89, 96 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 46 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 )
               , ( fromTupleUnordered ( ( 95, 68 ), ( 96, 46 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 96, 46 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 100, 57 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 95, 68 ), ( 96, 46 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 37, 6 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 82, 27 ), ( 37, 6 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 69, 10 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 41 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 95, 68 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 100, 57 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 97, 35 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 96, 46 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 82, 27 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 46 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 83, 16 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 37, 6 ), ( 89, 96 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 )
               , ( fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 95, 68 ), ( 89, 96 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 46 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 97, 35 ), ( 82, 27 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 )
               , ( fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 96, 46 ), ( 82, 27 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 69, 10 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 41 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 95, 68 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 89, 96 ), ( 100, 57 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 37, 6 ), ( 95, 68 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 97, 35 ), ( 96, 41 ) )
                 )
               , ( fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 96, 46 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 96, 41 ), ( 82, 27 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 37, 6 ), ( 95, 68 ) )
                 )
               , ( fromTupleUnordered ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 46 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) )
                 )
               , ( fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) )
                 , fromTupleUnordered ( ( 97, 35 ), ( 83, 16 ), ( 82, 27 ) )
                 )
               , ( fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) )
                 , fromTupleUnordered ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) )
                 )
               , ( fromTupleUnordered ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 37, 6 ), ( 89, 96 ) )
                 , fromTupleUnordered ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) )
                 )
               ]
       in
           Test.describe "flipped tests" <|
               List.indexedMap
                   (\index ( tri1, tri2, flipped1, flipped2 ) ->
                       Test.test ("flipped test #" ++ toString index) <|
                           \_ ->
                               flippedSharedEdge tri1 tri2
                                   |> Expect.equal (Just ( flipped1, flipped2 ))
                   )
                   examples
-}


points =
    List.map (\( x, y ) -> Point2d.fromCoordinates ( 10 * x, 10 * y ))
        [ ( -1, -1 )
        , ( 1, 1 )
        , ( -1, 1 )
        , ( 1, -1 )
        ]


triangulator : List Point2d -> List Triangle2d
triangulator points =
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



{-
   background =
       let
           triangles =
               triangulator points

           shared : AdjacencyList.AdjacencyList
           shared =
               AdjacencyList.fromTriangles triangles
                   |> SweepHull.unwrapAdjacencyList "tests background shared"

           edge1 =
               LineSegment2d.fromEndpoints
                   ( Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( 10, 10 )
                   )

           edge2 =
               LineSegment2d.fromEndpoints
                   ( Point2d.fromCoordinates ( -10, 10 )
                   , Point2d.fromCoordinates ( 10, -10 )
                   )

           tri1 =
               Triangle2d.fromVertices
                   ( Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( 10, 10 )
                   , Point2d.fromCoordinates ( 10, -10 )
                   )

           tri2 =
               Triangle2d.fromVertices
                   ( Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( -10, 10 )
                   , Point2d.fromCoordinates ( 10, 10 )
                   )

           tri3 =
               Triangle2d.fromVertices
                   ( Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( -10, 10 )
                   , Point2d.fromCoordinates ( 10, -10 )
                   )

           tri4 =
               Triangle2d.fromVertices
                   ( Point2d.fromCoordinates ( -10, 10 )
                   , Point2d.fromCoordinates ( 10, 10 )
                   , Point2d.fromCoordinates ( 10, -10 )
                   )

           edge3 =
               LineSegment2d.fromEndpoints
                   ( Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( -10, 10 )
                   )

           tri5 =
               Triangle2d.fromVertices
                   ( Point2d.fromCoordinates ( -20, -10 )
                   , Point2d.fromCoordinates ( -10, -10 )
                   , Point2d.fromCoordinates ( -10, 10 )
                   )
       in
           Test.describe "flipping edges"
               [ Test.test "3" <|
                   \_ ->
                       let
                           shared =
                               AdjacencyList.fromList
                                   [ ( edge1, ( tri1, tri2 ) )
                                   , ( edge3, ( tri2, tri5 ) )
                                   ]

                           expected =
                               AdjacencyList.fromList
                                   [ ( edge2, ( tri3, tri4 ) )
                                   , ( edge3, ( tri3, tri5 ) )
                                   ]
                       in
                           Result.map (flipEdge 2 edge1 edge2 tri1 tri2 tri3 tri4) shared
                               |> Expect.equal expected
               , Test.test "1" <|
                   \_ ->
                       flipEdge 0 edge1 edge2 tri1 tri2 tri3 tri4 (AllDict.singleton hashEdge edge1 ( tri1, tri2 ))
                           |> Expect.equal (AllDict.singleton hashEdge edge2 ( tri3, tri4 ))
               , Test.test "2" <|
                   \_ ->
                       flipEdge 0 edge2 edge1 tri3 tri4 tri1 tri2 (AllDict.singleton hashEdge edge2 ( tri3, tri4 ))
                           |> Expect.equal (AllDict.singleton hashEdge edge0 ( tri1, tri2 ))
               ]
-}
{-

   c =
       Test.test "randomPoints 7" <|
           \_ ->
               let
                   expected =
                       List.map fromTuples [ ( ( 37, 6 ), ( 82, 27 ), ( 69, 10 ) ), ( ( 82, 27 ), ( 83, 16 ), ( 69, 10 ) ), ( ( 95, 68 ), ( 82, 27 ), ( 37, 6 ) ), ( ( 97, 35 ), ( 82, 27 ), ( 96, 41 ) ), ( ( 82, 27 ), ( 96, 46 ), ( 96, 41 ) ), ( ( 100, 57 ), ( 96, 41 ), ( 96, 46 ) ), ( ( 82, 27 ), ( 97, 35 ), ( 83, 16 ) ), ( ( 82, 27 ), ( 95, 68 ), ( 96, 46 ) ), ( ( 96, 41 ), ( 100, 57 ), ( 97, 35 ) ), ( ( 96, 46 ), ( 95, 68 ), ( 100, 57 ) ), ( ( 37, 6 ), ( 89, 96 ), ( 95, 68 ) ), ( ( 95, 68 ), ( 89, 96 ), ( 100, 57 ) ) ]
               in
                   sweepHull (List.take 10 randomPoints1)
                       |> Tuple.first
                       |> List.map (sortTriangle)
                       |> List.sortBy Triangle2d.area
                       |> Expect.equal (List.sortBy Triangle2d.area expected)


   d =
       Test.test "randomPoints old == new" <|
           \_ ->
               let
                   old =
                       sweepHullOld (List.take 10 randomPoints1)
                           |> Tuple.first
                           |> List.map SweepHull.hashTriangle
                           |> List.sort

                   new =
                       sweepHull (List.take 10 randomPoints1)
                           |> Tuple.first
                           |> SweepHull.flipTrianglesUntilFixpoint
                           |> List.map SweepHull.hashTriangle
                           |> List.sort
               in
                   new
                       |> Expect.equal old
-}
{-
   checkAndFlipTrianglePairTest =
       let
           proof1 : Triangle2d -> Triangle2d -> AdjacencyList.AdjacencyProof
           proof1 tri1 tri2 =
               case AdjacencyList.proofAdjacency tri1 tri2 of
                   Nothing ->
                       Debug.crash ""

                   Just v ->
                       v

           verifyEqual name tri1 tri2 =
               Test.test name <|
                   \_ ->
                       (checkAndFlipTrianglePairNew (proof1 tri1 tri2) |> Maybe.map unwrap)
                           |> Expect.equal (checkAndFlipTrianglePair tri1 tri2)

           verifyEqualStart name tri1 tri2 =
               Test.test ("equal start " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> AdjacencyList.sortOnShared
                                   |> AdjacencyList.readProof
                                   |> (\( edge, tris ) -> ( tris.tri1, tris.tri2, edge ))

                           oldWay =
                               sortOnShared tri1 tri2
                                   |> (\v ->
                                           case v of
                                               Nothing ->
                                                   Debug.crash ""

                                               Just ( triOrdered1, triOrdered2, shared ) ->
                                                   ( triOrdered1, triOrdered2, shared )
                                      )
                       in
                           newWay
                               |> Expect.equal oldWay

           equalTriangulation1 name tri1 tri2 =
               Test.test ("equal triangulation 1 for " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> AdjacencyList.sortOnShared
                                   |> AdjacencyList.triangulation1

                           oldWay =
                               sortOnShared tri1 tri2
                                   |> Maybe.map (\( a, b, c ) -> ( a, b ))
                                   |> Maybe.map (uncurry SweepHull.triangulation1)
                                   |> Maybe.map
                                       (\v ->
                                           case v of
                                               Err _ ->
                                                   Debug.crash ""

                                               Ok e ->
                                                   e
                                       )
                       in
                           newWay
                               |> Expect.equal oldWay

           equalTriangulation2 name tri1 tri2 =
               Test.test ("equal triangulation 2 for " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> AdjacencyList.sortOnShared
                                   |> AdjacencyList.triangulation2
                                   |> Maybe.map
                                       (\( a, b, newProof ) ->
                                           let
                                               ( newShared, tris ) =
                                                   AdjacencyList.readProof newProof
                                           in
                                               ( a, b, tris.tri1, tris.tri2, newShared )
                                       )

                           oldWay =
                               sortOnShared tri1 tri2
                                   |> Maybe.map (\( a, b, c ) -> ( a, b ))
                                   |> Maybe.map (uncurry SweepHull.triangulation2)
                                   |> Maybe.map
                                       (\v ->
                                           case v of
                                               Err _ ->
                                                   Debug.crash ""

                                               Ok ( ( a, t1 ), ( b, t2 ), newShared ) ->
                                                   ( a, b, t1, t2, newShared )
                                       )
                       in
                           newWay
                               |> Expect.equal oldWay

           verifyEqualSharedEdge name tri1 tri2 =
               Test.test ("equal shared edge for " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> AdjacencyList.readProof
                                   |> (\( edge, tris ) -> edge)

                           oldWay =
                               hasCommonEdge tri1 tri2
                                   |> (\v ->
                                           case v of
                                               Nothing ->
                                                   Debug.crash ""

                                               Just e ->
                                                   e
                                      )
                       in
                           newWay
                               |> Expect.equal oldWay

           verifyEqualSplit name tri1 tri2 =
               Test.test ("equal split for " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> AdjacencyList.readProof
                                   |> (\( edge, tris ) -> ( edge, tris.otherVertex1, tris.otherVertex2 ))

                           oldWay =
                               splitOnShared tri1 tri2
                                   |> (\v ->
                                           case v of
                                               Nothing ->
                                                   Debug.crash ""

                                               Just e ->
                                                   e
                                      )
                       in
                           newWay
                               |> Expect.equal ((\{ shared, other1, other2 } -> ( shared, other1, other2 )) oldWay)

           equalFlip name tri1 tri2 =
               Test.test ("equal flip for " ++ name) <|
                   \_ ->
                       let
                           proof =
                               proof1 tri1 tri2

                           newWay =
                               proof
                                   |> SweepHull.checkAndFlipTrianglePairNew
                                   |> Maybe.map (AdjacencyList.readProof >> \( edge, tris ) -> ( tris.tri1, tris.tri2, edge ))

                           oldWay =
                               SweepHull.checkAndFlipTrianglePair tri1 tri2
                       in
                           newWay
                               |> Expect.equal oldWay

           unwrap proof =
               AdjacencyList.readProof proof
                   |> (\( sharedEdge, { tri1, tri2 } ) ->
                           ( tri1, tri2, sharedEdge )
                      )

           example1 =
               ( tri1, tri2 )

           example2 =
               ( fromTuplesUnordered ( ( 670, 450 ), ( 520, 780 ), ( 610, 540 ) )
               , fromTuplesUnordered ( ( 520, 780 ), ( 670, 450 ), ( 520, 810 ) )
               )

           example3 =
               ( fromTuplesUnordered ( ( 610, 540 ), ( 520, 810 ), ( 670, 450 ) )
               , fromTuplesUnordered ( ( 520, 810 ), ( 610, 540 ), ( 520, 780 ) )
               )

           example4 =
               ( fromTuplesUnordered ( ( 0, 390 ), ( 70, 370 ), ( 10, 350 ) )
               , fromTuplesUnordered ( ( 10, 350 ), ( 50, 160 ), ( 0, 390 ) )
               )
       in
           Test.describe "checkAndFlipTrianglePair"
               [ uncurry (verifyEqual "example 1") example1
               , uncurry (verifyEqual "example 2") example2
               , uncurry (verifyEqual "example 3") example3
               , uncurry (verifyEqualStart "example 1") example1
               , uncurry (verifyEqualStart "example 2") example2
               , uncurry (verifyEqualStart "example 3") example3
               , uncurry (verifyEqualSharedEdge "example 1") example1
               , uncurry (verifyEqualSharedEdge "example 2") example2
               , uncurry (verifyEqualSharedEdge "example 3") example3
               , uncurry (equalTriangulation1 "example 1") example1
               , uncurry (equalTriangulation1 "example 2") example2
               , uncurry (equalTriangulation1 "example 3") example3
               , uncurry (equalTriangulation1 "example 4") example4
               , uncurry (equalTriangulation2 "example 1") example1
               , uncurry (equalTriangulation2 "example 2") example2
               , uncurry (equalTriangulation2 "example 3") example3
               , uncurry (equalTriangulation2 "example 4") example4
               , uncurry (verifyEqualSplit "example 1") example1
               , uncurry (verifyEqualSplit "example 2") example2
               , uncurry (verifyEqualSplit "example 3") example3
               , uncurry (verifyEqualSplit "example 4") example4
               , uncurry (equalFlip "example 1") example1
               , uncurry (equalFlip "example 2") example2
               , uncurry (equalFlip "example 3") example3
               , uncurry (equalFlip "example 4") example4
               , Test.test "example 4 old " <|
                   \_ ->
                       let
                           ( tri1, tri2 ) =
                               example4
                       in
                           SweepHull.checkAndFlipTrianglePair tri1 tri2
                               |> Expect.equal Nothing
               , Test.test "example 4 new " <|
                   \_ ->
                       proof1 tri1 tri2
                           |> SweepHull.checkAndFlipTrianglePairNew
                           |> Maybe.map (AdjacencyList.readProof >> \( edge, tris ) -> ( tris.tri1, tris.tri2, edge ))
                           |> Expect.equal Nothing
               ]
-}


hashEdgeTest =
    let
        hashTriangleOld : Triangle2d -> List ( Float, Float )
        hashTriangleOld triangle =
            let
                ( q1, q2, q3 ) =
                    Triangle2d.vertices triangle
            in
                List.sort <| List.map Point2d.coordinates [ q1, q2, q3 ]

        fuzzPoint =
            Fuzz.map2 (,) Fuzz.float Fuzz.float
                |> Fuzz.map Point2d.fromCoordinates

        fuzzTriangle =
            Fuzz.map3 (,,) fuzzPoint fuzzPoint fuzzPoint
                |> Fuzz.map Triangle2d.fromVertices

        toList ( a, b, c ) =
            [ a, b, c ]
    in
        Test.fuzz fuzzTriangle "hash triangle behaves as before" <|
            \triangle ->
                AdjacencyList.hashTriangle triangle
                    |> toList
                    |> Expect.equal (hashTriangleOld triangle)


findOtherEdgeTest =
    let
        data =
            [ ( ( 974, 396 ), ( 992, 456 ), Set.fromList [ ( ( 974, 354 ), ( 974, 396 ) ), ( ( 974, 354 ), ( 992, 456 ) ), ( ( 974, 396 ), ( 992, 456 ) ) ], Just (( 974, 354 )) )
            , ( ( 992, 456 ), ( 998, 248 ), Set.fromList [ ( ( 974, 354 ), ( 992, 456 ) ), ( ( 974, 354 ), ( 998, 248 ) ), ( ( 992, 456 ), ( 998, 248 ) ) ], Just (( 974, 354 )) )
            , ( ( 974, 354 ), ( 998, 248 ), Set.fromList [ ( ( 974, 354 ), ( 980, 304 ) ), ( ( 974, 354 ), ( 998, 248 ) ), ( ( 980, 304 ), ( 998, 248 ) ) ], Just (( 980, 304 )) )
            , ( ( 974, 396 ), ( 974, 354 ), Set.fromList [ ( ( 950, 396 ), ( 974, 354 ) ), ( ( 950, 396 ), ( 974, 396 ) ), ( ( 974, 354 ), ( 974, 396 ) ) ], Just (( 950, 396 )) )
            , ( ( 998, 248 ), ( 980, 304 ), Set.fromList [ ( ( 980, 304 ), ( 992, 456 ) ), ( ( 980, 304 ), ( 998, 248 ) ), ( ( 992, 456 ), ( 998, 248 ) ) ], Just (( 992, 456 )) )
            , ( ( 980, 304 ), ( 974, 354 ), Set.fromList [ ( ( 974, 354 ), ( 980, 304 ) ), ( ( 974, 354 ), ( 992, 456 ) ), ( ( 980, 304 ), ( 992, 456 ) ) ], Just (( 992, 456 )) )
            , ( ( 992, 456 ), ( 974, 354 ), Set.fromList [ ( ( 974, 354 ), ( 974, 396 ) ), ( ( 974, 354 ), ( 992, 456 ) ), ( ( 974, 396 ), ( 992, 456 ) ) ], Just (( 974, 396 )) )
            , ( ( 992, 456 ), ( 998, 248 ), Set.fromList [ ( ( 980, 304 ), ( 992, 456 ) ), ( ( 980, 304 ), ( 998, 248 ) ), ( ( 992, 456 ), ( 998, 248 ) ) ], Just (( 980, 304 )) )
            , ( ( 950, 738 ), ( 994, 782 ), Set.fromList [ ( ( 950, 738 ), ( 994, 782 ) ), ( ( 950, 738 ), ( 998, 778 ) ), ( ( 994, 782 ), ( 998, 778 ) ) ], Just (( 998, 778 )) )
            , ( ( 998, 630 ), ( 950, 738 ), Set.fromList [ ( ( 950, 738 ), ( 998, 630 ) ), ( ( 950, 738 ), ( 998, 778 ) ), ( ( 998, 630 ), ( 998, 778 ) ) ], Just (( 998, 778 )) )
            , ( ( 998, 778 ), ( 994, 782 ), Set.fromList [ ( ( 980, 866 ), ( 994, 782 ) ), ( ( 980, 866 ), ( 998, 778 ) ), ( ( 994, 782 ), ( 998, 778 ) ) ], Just (( 980, 866 )) )
            , ( ( 998, 778 ), ( 998, 630 ), Set.fromList [ ( ( 950, 738 ), ( 998, 630 ) ), ( ( 950, 738 ), ( 998, 778 ) ), ( ( 998, 630 ), ( 998, 778 ) ) ], Just (( 950, 738 )) )
            , ( ( 950, 738 ), ( 998, 778 ), Set.fromList [ ( ( 950, 738 ), ( 998, 630 ) ), ( ( 950, 738 ), ( 998, 778 ) ), ( ( 998, 630 ), ( 998, 778 ) ) ], Just (( 998, 630 )) )
            , ( ( 998, 778 ), ( 994, 782 ), Set.fromList [ ( ( 980, 866 ), ( 994, 782 ) ), ( ( 980, 866 ), ( 998, 778 ) ), ( ( 994, 782 ), ( 998, 778 ) ) ], Just (( 980, 866 )) )
            , ( ( 960, 758 ), ( 950, 738 ), Set.fromList [ ( ( 930, 786 ), ( 950, 738 ) ), ( ( 930, 786 ), ( 960, 758 ) ), ( ( 950, 738 ), ( 960, 758 ) ) ], Just (( 930, 786 )) )
            , ( ( 960, 758 ), ( 994, 782 ), Set.fromList [ ( ( 960, 758 ), ( 994, 782 ) ), ( ( 960, 758 ), ( 998, 778 ) ), ( ( 994, 782 ), ( 998, 778 ) ) ], Just (( 998, 778 )) )
            ]

        makeTest i ( p1, p2, edges, expected ) =
            Test.test ("findOtherEdgeTest #" ++ toString i) <|
                \_ ->
                    AdjacencyList.findOtherEdge (Point2d.fromCoordinates p1) (Point2d.fromCoordinates p2) edges
                        |> Expect.equal (Maybe.map Point2d.fromCoordinates expected)
    in
        Test.describe "findOtherEdgeTest" (List.indexedMap makeTest data)
