module SweepHull exposing (..)

import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import LineSegment2d exposing (LineSegment2d)
import Circle2d
import Polygon2d exposing (Polygon2d)
import Circle2d
import AdjacencyList exposing (AdjacencyList, AdjacencyProof, AdjacentTriangles)
import AllDict exposing (AllDict)


{-| S-hull: a fast sweep-hull routine for Delaunay triangulation by David Sinclair
http://www.s-hull.org/
-}
sweepHull : List Point2d -> ( List Triangle2d, Polygon2d )
sweepHull points =
    case initialTriangle points of
        Ok initial ->
            sweepHullHelper points initial

        Err _ ->
            ( [], emptyPolygon )


{-| Construct the actual delaunay triangulation.

On a high level:

* sort the input points (excluding the initial triangle) by their distance to the triangle
* construct a ugly/dirty triangluation from the sorted points:
    The triangulation doesn't have crossing/overlapping edges, but often does have many skinny triangles.
* flip edges in that triangulation based on the delaunay condition until it settles
* remove triangles with area=0
-}
sweepHullHelper : List Point2d -> Triangle2d -> ( List Triangle2d, Polygon2d )
sweepHullHelper points initialTriangle =
    case circumCircleCenter initialTriangle of
        Nothing ->
            ( [], emptyPolygon )

        Just center ->
            let
                -- Sort points by distance from the initial tri's circum-circle centre
                insertionOrder =
                    sortByDistanceFromInitialTriangle points center initialTriangle

                -- Form triangles by sequentially adding points
                ( hull, newTriangles ) =
                    formTriangles initialTriangle insertionOrder

                -- Flip adjacent pairs of triangles to meet Delaunay condition
                -- https://en.wikipedia.org/wiki/Delaunay_triangulation#Visual_Delaunay_definition:_Flipping
                delaunayTris =
                    flipTrianglesUntilFixpoint newTriangles

                -- Remove zero area triangles
                filteredTris =
                    removeZeroAreaTris delaunayTris
            in
                ( filteredTris, hull )


emptyPolygon : Polygon2d
emptyPolygon =
    Polygon2d.singleLoop []


circumCircleCenter : Triangle2d -> Maybe Point2d
circumCircleCenter triangle =
    triangle
        |> Triangle2d.circumcircle
        |> Maybe.map Circle2d.centerPoint


{-| Find a common edge between two triangles
-}
hasCommonEdge : Triangle2d -> Triangle2d -> Maybe LineSegment2d
hasCommonEdge tri1 tri2 =
    let
        edges1 =
            let
                ( e1, e2, e3 ) =
                    Triangle2d.edges tri1
            in
                [ e1, e2, e3 ]

        edges2 =
            let
                ( e1, e2, e3 ) =
                    Triangle2d.edges tri2
            in
                [ e1, e2, e3 ]

        options =
            edges2 ++ List.map LineSegment2d.reverse edges2

        folder edge accum =
            case accum of
                Just x ->
                    Just x

                Nothing ->
                    if List.member edge options then
                        Just edge
                    else
                        Nothing
    in
        List.foldl folder Nothing edges1


hashEdge : LineSegment2d -> ( ( Float, Float ), ( Float, Float ) )
hashEdge line =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints line

        ( q1, q2 ) =
            ( Point2d.coordinates p1, Point2d.coordinates p2 )
    in
        if q1 < q2 then
            ( q1, q2 )
        else
            ( q2, q1 )


hashTriangle : Triangle2d -> List ( Float, Float )
hashTriangle triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        List.sort <| List.map Point2d.coordinates [ p1, p2, p3 ]


type alias SharedEdgeSet =
    AllDict LineSegment2d () ( ( Float, Float ), ( Float, Float ) )


flipTriangles : AdjacencyList -> AdjacencyList
flipTriangles triangles =
    flipTrianglesHelper AdjacencyList.empty triangles


{-| Actually perform flipping

We get the "lowest" proof (constructs the whole list, sort of inefficient), and try to flip it.
If it doesn't need to be flipped the edge is added to the accumulator (of done edges).
Otherwise, the proof is removed, and the flipped proof is inserted into the accumulator.

The hard part is removing references to the now-removed triangles and replacing them with
the flipped triangles. We also store the triangles that now shared an edge with the flipped triangle as the
"dirty edges": they need to be checked for flipping in the next iteration.

-}
flipTrianglesHelper : AdjacencyList -> AdjacencyList -> AdjacencyList
flipTrianglesHelper accum remainingTriangles =
    case AdjacencyList.toProofList remainingTriangles of
        [] ->
            accum

        proof :: _ ->
            let
                ( edge, adjacentTriangles ) =
                    AdjacencyList.readProof proof
            in
                case checkAndFlipTrianglePair proof of
                    Just flippedProof ->
                        let
                            ( newSharedEdge, newTriangles ) =
                                AdjacencyList.readProof flippedProof

                            newFlippedTriangles =
                                [ newTriangles.tri1, newTriangles.tri2 ]

                            -- the other edges of the removed triangles. We have to check whether these edges are shared
                            -- so we can replace the now-removed triangle with its flipped version there
                            otherEdges : List LineSegment2d
                            otherEdges =
                                (triangleEdges adjacentTriangles.tri1 ++ triangleEdges adjacentTriangles.tri2)
                                    |> List.filter (\e -> hashEdge e /= hashEdge edge)

                            mapBoth f ( a, b ) =
                                ( f a, f b )

                            replacer : Triangle2d -> AdjacentTriangles -> ( Triangle2d, Triangle2d )
                            replacer possibleReplacement triangles =
                                mapBoth (replaceTriangle edge possibleReplacement) ( triangles.tri1, triangles.tri2 )

                            folder otherEdge ( removed, accum ) =
                                -- we know there is at most one replacement
                                case List.filter (triangleHasEdge otherEdge) newFlippedTriangles of
                                    possibleReplacement :: _ ->
                                        case AdjacencyList.getProof otherEdge accum of
                                            Nothing ->
                                                ( AdjacencyList.update otherEdge (Maybe.map (replacer possibleReplacement)) removed
                                                , accum
                                                )

                                            Just proof ->
                                                let
                                                    ( _, triangles ) =
                                                        AdjacencyList.readProof proof
                                                in
                                                    case ( replaceTriangleMaybe edge possibleReplacement triangles.tri1, replaceTriangleMaybe edge possibleReplacement triangles.tri2 ) of
                                                        ( Nothing, Nothing ) ->
                                                            ( removed, accum )

                                                        ( newTri1_, newTri2_ ) ->
                                                            let
                                                                newTri1 =
                                                                    newTri1_ |> Maybe.withDefault triangles.tri1

                                                                newTri2 =
                                                                    newTri2_ |> Maybe.withDefault triangles.tri2
                                                            in
                                                                ( AdjacencyList.insert otherEdge ( newTri1, newTri2 ) removed |> AdjacencyList.unsafeUnwrap "insert"
                                                                , AdjacencyList.remove otherEdge accum
                                                                )

                                    _ ->
                                        ( removed, accum )

                            ( newerRemainingTriangles, valid ) =
                                List.foldl folder ( remainingTriangles, accum ) otherEdges
                                    |> Tuple.mapFirst (AdjacencyList.remove edge)
                        in
                            flipTrianglesHelper
                                (valid
                                    |> AdjacencyList.insertProof flippedProof
                                )
                                newerRemainingTriangles

                    Nothing ->
                        flipTrianglesHelper
                            (AdjacencyList.insertProof proof accum)
                            (AdjacencyList.remove edge remainingTriangles)


sharesEdge : Triangle2d -> Triangle2d -> Bool
sharesEdge tri1 tri2 =
    case hasCommonEdge tri1 tri2 of
        Nothing ->
            False

        Just _ ->
            True


triangleEdges : Triangle2d -> List LineSegment2d
triangleEdges triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.edges triangle
    in
        [ p1, p2, p3 ]


triangleHasEdge : LineSegment2d -> Triangle2d -> Bool
triangleHasEdge edge triangle =
    List.member (hashEdge edge) (List.map hashEdge (triangleEdges triangle))


{-| Replace a triangle by its flipped version if it contains a removed edge
-}
replaceTriangle : LineSegment2d -> Triangle2d -> Triangle2d -> Triangle2d
replaceTriangle removedEdge newTriangle existingTriangle =
    if triangleHasEdge removedEdge existingTriangle then
        if sharesEdge newTriangle existingTriangle then
            newTriangle
        else
            existingTriangle
    else
        existingTriangle


{-| Replace a triangle by its flipped version if it contains a removed edge
-}
replaceTriangleMaybe : LineSegment2d -> Triangle2d -> Triangle2d -> Maybe Triangle2d
replaceTriangleMaybe removedEdge newTriangle existingTriangle =
    if triangleHasEdge removedEdge existingTriangle then
        if sharesEdge newTriangle existingTriangle then
            Just newTriangle
        else
            Nothing
    else
        Nothing


{-| Flip triangles until stability is reached (no new triangles are flipped)
-}
flipTrianglesUntilFixpoint : List Triangle2d -> List Triangle2d
flipTrianglesUntilFixpoint rawTriangles =
    case AdjacencyList.fromTriangles rawTriangles of
        Err _ ->
            rawTriangles

        Ok initial ->
            flipTriangles initial
                |> AdjacencyList.uniqueTriangles


{-| Given two triangles that share an edge, we obtain a quad. There are two ways the quad
can be tesselated (turned into triangles): the diagonal can run to from bottom left to top right
or from the top left to the bottom right.

Here, we try the first triangulation. We switch to the second triangulation if the first one:
* Has a degenerate triangle. A triangle is degenerate when its three points are on a straight line.
    A consequence is that the area is zero and one of the angles of the triangle is 180 degrees (pi radians).

* The delaunay condition is not met. That means that the outer angles (not touching the shared edge) are too large (larger than pi radians when combined).
    When these angles are large, the shared edge is relatively long (compared to the other sides) and flipping it will create two triangles with
    more equal sides.
-}
checkAndFlipTrianglePair : AdjacencyProof -> Maybe AdjacencyProof
checkAndFlipTrianglePair proof_ =
    let
        proof =
            AdjacencyList.sortOnShared proof_
    in
        case AdjacencyList.triangulation1 proof of
            Nothing ->
                Nothing

            Just ( t1, t3 ) ->
                let
                    flipDegenerateTri =
                        (t1 == pi || t3 == pi)

                    angTotal =
                        t1 + t3

                    flipForDelaunay =
                        angTotal > pi
                in
                    if flipDegenerateTri || flipForDelaunay then
                        -- print("Flip possibly required", angTotal, triOrdered1, triOrdered2)
                        case AdjacencyList.triangulation2 proof of
                            Nothing ->
                                Nothing

                            Just ( t2, t4, newProof ) ->
                                -- t1 + t2 + t3 + t4 == 2 * pi
                                if flipDegenerateTri && (t2 > pi || t4 > pi) then
                                    -- Flipping would create an overlap
                                    Nothing
                                else if t2 == pi || t4 == pi then
                                    -- Flipping would create triangle of zero size
                                    Nothing
                                else
                                    let
                                        flipAngTotal =
                                            t2 + t4
                                    in
                                        if flipForDelaunay && flipAngTotal >= angTotal then
                                            -- No improvement when flipped, so abort flip
                                            Nothing
                                        else
                                            Just newProof
                    else
                        Nothing


{-| Find the initial triangle. This can fail if

* there are fewer than 3 points in the input
* there are no three non-overlapping points
-}
initialTriangle : List Point2d -> Result String Triangle2d
initialTriangle points =
    case points of
        first :: ((_ :: _ :: _) as rest) ->
            -- Sort by radial distance
            case List.sortBy (Point2d.distanceFrom first) rest of
                [] ->
                    Err "cannot happen"

                second :: others ->
                    case completeInitialTriangle others first second of
                        Nothing ->
                            -- ruled out
                            Err "no valid triangles"

                        Just match ->
                            Ok match

        _ ->
            Err "not enough points"


completeInitialTriangle : List Point2d -> Point2d -> Point2d -> Maybe Triangle2d
completeInitialTriangle points firstPoint secondPoint =
    let
        folder point smallestSoFar =
            let
                new =
                    Triangle2d.fromVertices ( point, firstPoint, secondPoint )
                        |> Triangle2d.circumcircle
                        |> Maybe.map Circle2d.diameter
                        |> Maybe.map (\diameter -> { diameter = diameter, point = point })
            in
                case ( smallestSoFar, new ) of
                    ( Nothing, x ) ->
                        x

                    ( x, Nothing ) ->
                        x

                    ( Just x, Just y ) ->
                        Just (minBy .diameter x y)

        pointWithSmallestCircumCircle =
            List.foldl folder Nothing points
                |> Maybe.map .point

        constructTriangle thirdPoint =
            let
                triangle =
                    Triangle2d.fromVertices ( firstPoint, secondPoint, thirdPoint )
            in
                -- Order points to be right handed
                if Triangle2d.clockwiseArea triangle < 0 then
                    -- Swap points
                    Triangle2d.fromVertices ( firstPoint, thirdPoint, secondPoint )
                else
                    -- Already right handed
                    triangle
    in
        pointWithSmallestCircumCircle
            |> Maybe.map constructTriangle


compareBy : (a -> comparable) -> a -> a -> Order
compareBy tagger x y =
    compare (tagger x) (tagger y)


minBy : (a -> comparable) -> a -> a -> a
minBy tagger x y =
    case compareBy tagger x y of
        GT ->
            y

        EQ ->
            x

        LT ->
            x


maxBy : (a -> comparable) -> a -> a -> a
maxBy tagger x y =
    case compareBy tagger x y of
        GT ->
            x

        EQ ->
            x

        LT ->
            y


{-| Sort the points not in the initial triangle by distance to the center of
    the (circumcircle of) the initial triangle
-}
sortByDistanceFromInitialTriangle : List Point2d -> Point2d -> Triangle2d -> List Point2d
sortByDistanceFromInitialTriangle points center initialTriangle =
    let
        ( first, second, third ) =
            Triangle2d.vertices initialTriangle

        mapper point =
            if point == first || point == second || point == third then
                Nothing
            else
                Just
                    ( point
                    , LineSegment2d.fromEndpoints ( point, center )
                        |> LineSegment2d.length
                    )
    in
        points
            |> List.filterMap mapper
            |> List.sortBy Tuple.second
            |> List.map Tuple.first


{-| Give the visible hull edges from a point.
Assumes the point lies outside of the polygon (not sure that is important)

This works by taking every edge of the hull and checking whether a triangle with the new point forms
a clockwise triangle. If it does, the edge is visible, otherwise it isn't.
-}
visibleEdges : Point2d -> Polygon2d -> List LineSegment2d
visibleEdges point polygon =
    Polygon2d.edges polygon
        |> List.filterMap
            (\lineSegment ->
                let
                    ( p1, p2 ) =
                        LineSegment2d.endpoints lineSegment

                    area =
                        Triangle2d.fromVertices ( p1, p2, point )
                            |> Triangle2d.clockwiseArea
                in
                    if area >= 0 then
                        Just lineSegment
                    else
                        Nothing
            )


{-| It turns out that - because of the radial ordering of the points -
edges that are on the convex hull aren't a problem
-}
type VisibleEdge
    = Visible LineSegment2d
    | On LineSegment2d


{-| Form a triangulation

This triangulation is correct (no overlapping edges) but not as "good"
as delaunay: there can be large skinny triangles.

arguments:

* the initial triangle
* the ordered (by distance from the initial triangle) other points

process:

* construct the convex hull of the points seen so far (the initial triangle)
* go over the other points one by one. for each
    - check what edges of the hull it needs to connect to (i.e. are visible from that point)
    - and add triangles connecting the point with the visible edges
    - add the new point to the hull

-}
formTriangles : Triangle2d -> List Point2d -> ( Polygon2d, List Triangle2d )
formTriangles seedTriangle orderedPoints =
    let
        constructTriangle point lineSegment =
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints lineSegment
            in
                Triangle2d.fromVertices ( p2, point, p1 )

        initialTriangles =
            [ seedTriangle ]

        initialHull =
            let
                ( p1, p2, p3 ) =
                    Triangle2d.vertices seedTriangle
            in
                Polygon2d.convexHull [ p1, p2, p3 ]

        addTriangle point lineSegment triangles =
            constructTriangle point lineSegment :: triangles

        folder point ( hull, triangles ) =
            ( hull
                |> Polygon2d.vertices
                |> (\verts -> point :: verts)
                |> Polygon2d.convexHull
            , visibleEdges point hull
                |> List.foldl (addTriangle point) triangles
            )
    in
        List.foldl folder ( initialHull, initialTriangles ) orderedPoints


removeZeroAreaTris : List Triangle2d -> List Triangle2d
removeZeroAreaTris =
    List.filter (\triangle -> Triangle2d.area triangle > 0)
