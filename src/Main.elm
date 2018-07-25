module Main exposing (..)

import Delaunay
import SweepHull
import SweepHullOld
import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import SubPath exposing (SubPath)
import Curve
import Svg exposing (Svg)
import Svg.Attributes exposing (fill, strokeWidth, stroke, width, height, viewBox, cx, cy, r)
import Html exposing (text)
import Set
import Random500
import AdjacencyList exposing (hashTriangle)
import Vector2d
import Circle2d
import Array.Hamt as Array
import Geometry.Svg
import Polygon2d
import Circle2d
import Dict


drawTriangle : String -> Triangle2d -> Svg msg
drawTriangle color triangle =
    let
        ( p1, p2, p3 ) =
            toTuples triangle
    in
        Curve.linearClosed [ p1, p2, p3 ]
            |> (\c -> SubPath.element c [ fill color ])


fromTuples =
    (\( p1, p2, p3 ) ->
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


randomPoints1 =
    List.map Point2d.fromCoordinates
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


randomPoints =
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
        |> Set.fromList
        |> Set.toList
        |> List.map (\( x, y ) -> Point2d.fromCoordinates ( 10 * x, 10 * y ))


points =
    List.map (\( x, y ) -> Point2d.fromCoordinates ( 100 * x, 100 * y ))
        [ ( 0, 0 )
        , ( -1, -1 )
        , ( 1, 1 )
        , ( -1, 1 )
        , ( 1, -1 )
        ]


initialTriangulation : List Point2d -> List Triangle2d
initialTriangulation points =
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


main =
    main3 ()


main1 _ =
    let
        displacement =
            Vector2d.fromComponents ( 500, 250 )

        points =
            List.map Point2d.fromCoordinates
                [ ( 0, 0 )
                , ( 500, 500 )
                , ( 0, 500 )
                , ( 500, 0 )
                ]

        n =
            4

        _ =
            Debug.log "degenerate?" (List.any (\x -> Triangle2d.area x == 0) triangles_)

        triangles_ =
            SweepHull.sweepHull (List.take n randomPoints)
                |> Tuple.first
                |> List.sortBy AdjacencyList.hashTriangle

        oldTriangles_ =
            SweepHullOld.sweepHullOld (List.take n randomPoints)
                |> Tuple.first
                |> List.sortBy AdjacencyList.hashTriangle

        oldSet =
            List.map hashTriangle oldTriangles_ |> Set.fromList

        newSet =
            List.map hashTriangle triangles_ |> Set.fromList

        _ =
            Debug.log "difference" ( Set.diff newSet oldSet, Set.diff oldSet newSet )

        mapper old new =
            ( drawTriangle
                (if Set.member (hashTriangle old) newSet then
                    "white"
                 else
                    "red"
                )
                old
            , drawTriangle
                (if Set.member (hashTriangle new) oldSet then
                    "white"
                 else
                    "red"
                )
                new
            )

        ( oldTriangles, triangles ) =
            List.map2 mapper oldTriangles_ triangles_
                |> List.unzip

        _ =
            Debug.log "equal? " (triangles == oldTriangles)

        _ =
            Debug.log "equal lengths " (List.length triangles == List.length oldTriangles)
    in
        Html.div []
            [ Svg.svg
                [ fill "none", stroke "black", strokeWidth "2", width "1000", height "1000" ]
                triangles
            , Svg.svg
                [ fill "none", stroke "black", strokeWidth "2", width "1000", height "1000" ]
                oldTriangles
            ]


main2 _ =
    let
        triangles =
            SweepHull.sweepHull Random500.points
                |> Tuple.first
                |> List.sortBy AdjacencyList.hashTriangle
                |> List.map (drawTriangle "white")
    in
        Html.div []
            [ Svg.svg
                [ fill "none", stroke "grey", strokeWidth "2", width "1000", height "1000" ]
                triangles
            ]


drawCircle : Triangle2d -> Svg msg
drawCircle triangle =
    case Triangle2d.circumcircle triangle of
        Nothing ->
            text ""

        Just circle ->
            let
                ( x, y ) =
                    Circle2d.centerPoint circle |> Point2d.coordinates

                radius =
                    Circle2d.radius circle
            in
                Svg.circle [ cx (toString x), cy (toString y), r (toString radius), fill "none" ] []


drawPoint ( x, y ) =
    Svg.circle [ cx (toString x), cy (toString y), r (toString 5), fill "black" ] []


main3 _ =
    let
        displacement =
            Vector2d.fromComponents ( 350, 300 )

        points =
            List.map Point2d.fromCoordinates
                [ ( 0, 0 )
                , ( 500, 500 )
                , ( 0, 500 )
                , ( 520, 0 )
                ]

        makeVisible triangles =
            let
                mapper triangle_ =
                    let
                        triangle =
                            -- Triangle2d.scaleAbout (Point2d.fromCoordinates ( 500, 500 )) 0.9 triangle_
                            triangle_
                    in
                        {-
                           case Triangle2d.circumcircle triangle |> Maybe.map (Circle2d.centerPoint >> Point2d.coordinates) of

                               Just ( x, y ) ->
                                   [ Svg.circle [ cx (toString x), cy (toString y), r "10", fill "black" ] []
                                   , drawTriangle "none" triangle
                                   ]

                               Nothing ->
                        -}
                        [ drawTriangle "none" triangle ]
            in
                List.concatMap mapper triangles

        transformation =
            Triangle2d.scaleAbout (Point2d.fromCoordinates ( 500, 500 )) 0.5 << Triangle2d.translateBy displacement << Triangle2d.scaleAbout Point2d.origin 0.1

        transformation_ =
            Polygon2d.scaleAbout (Point2d.fromCoordinates ( 500, 500 )) 0.5 << Polygon2d.translateBy displacement << Polygon2d.scaleAbout Point2d.origin 0.1

        transformation__ =
            Point2d.scaleAbout (Point2d.fromCoordinates ( 500, 500 )) 0.2

        makeVisibleWithCircles triangles =
            triangles
                |> List.map transformation
                |> List.concatMap (\triangle -> [ drawTriangle "white" triangle, drawCircle triangle ])

        step5 =
            Delaunay.delaunayTriangulation (List.take 500 Random500.points |> Array.fromList)
                |> makeVisible

        n =
            500

        ps =
            (List.take n Random500.points |> Array.fromList)
                |> Array.map transformation__

        void _ =
            []

        centers =
            ps
                |> Delaunay.createGeometry
                |> Delaunay.centerPointsPerVertex
                |> Dict.values
                |> List.concat
                |> List.map Point2d.coordinates
                |> List.map
                    (\( x, y ) ->
                        Svg.circle [ cx (toString x), cy (toString y), r "5", fill "red", stroke "none" ] []
                    )

        voronoiTris =
            ps
                |> Delaunay.delaunayTriangulation
                |> makeVisible

        voronoiDiag =
            ps
                |> Delaunay.voronoiDiagram
                |> Debug.log "polygons"
                |> List.map (Geometry.Svg.polygon2d [ stroke "steelblue" ])
    in
        Html.div []
            [ text "We start with a triangle covering all of our points"
            , Svg.svg [ fill "none", stroke "black", strokeWidth "2", width "1000", height "1000" ] (step5)
            , Svg.svg [ fill "none", stroke "black", strokeWidth "2", width "1000", height "1000" ] (voronoiDiag ++ voronoiTris ++ centers)
            ]
