module SweepHull exposing (..)

import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import LineSegment2d exposing (LineSegment2d)
import Circle2d
import Vector2d exposing (Vector2d)
import Polygon2d exposing (Polygon2d)
import Circle2d
import AdjacencyList exposing (AdjacencyList, AdjacencyProof)
import Set


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


{-| S-hull: a fast sweep-hull routine for Delaunay triangulation by David Sinclair
http://www.s-hull.org/
-}
sweepHullOld : List Point2d -> ( List Triangle2d, Polygon2d )
sweepHullOld points =
    case initialTriangle points of
        Ok initial ->
            sweepHullHelperOld points initial

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
sweepHullHelperOld : List Point2d -> Triangle2d -> ( List Triangle2d, Polygon2d )
sweepHullHelperOld points initialTriangle =
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
                    flipTrianglesUntilFixpointOld newTriangles

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


{-| Find a common edge between two triangles
-}
hasCommonEdgeSet : Triangle2d -> Triangle2d -> Maybe LineSegment2d
hasCommonEdgeSet tri1 tri2 =
    let
        ( e1, e2, e3 ) =
            Triangle2d.edges tri1

        ( e4, e5, e6 ) =
            Triangle2d.edges tri2

        edges1 =
            [ hashEdge e1, hashEdge e2, hashEdge e3 ]
                |> Set.fromList

        edges2 =
            [ hashEdge e4, hashEdge e5, hashEdge e6 ]
                |> Set.fromList

        common =
            Set.intersect edges1 edges2
    in
        if Set.isEmpty common then
            Nothing
        else
            Set.toList common
                |> List.head
                |> Maybe.map (mapBoth Point2d.fromCoordinates >> LineSegment2d.fromEndpoints)


hasCommonEdgeFast : Triangle2d -> Triangle2d -> Maybe LineSegment2d
hasCommonEdgeFast tri1 tri2 =
    let
        ( e1, e2, e3 ) =
            Triangle2d.edges tri1

        ( e4, e5, e6 ) =
            Triangle2d.edges tri2
    in
        if e1 == e4 || e1 == e5 || e1 == e6 then
            Just e1
        else if e2 == e4 || e2 == e5 || e2 == e6 then
            Just e2
        else if e3 == e4 || e3 == e5 || e3 == e6 then
            Just e3
        else
            Nothing


type DegenerateTriangle
    = DegenerateTriangle


{-| Calculate the internal angle between the third vertex and the other two
-}
calcTriangleAngle : Triangle2d -> Result DegenerateTriangle Float
calcTriangleAngle triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle

        v1 =
            Vector2d.from p3 p1
                |> Vector2d.normalize

        v2 =
            Vector2d.from p3 p2
                |> Vector2d.normalize
    in
        if Vector2d.length v1 == 0 || Vector2d.length v2 == 0 then
            Err DegenerateTriangle
        else
            ( Vector2d.crossProduct v2 v1, Vector2d.dotProduct v1 v2 )
                |> (Tuple.mapSecond (clamp -1 1))
                |> (\( crossProd, dotProd ) ->
                        if crossProd < 0 then
                            Ok (2 * pi - acos dotProd)
                        else
                            Ok (acos dotProd)
                   )


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


type alias EdgeHash =
    ( ( Float, Float ), ( Float, Float ) )


flipEdge :
    Int
    -> LineSegment2d
    -> LineSegment2d
    -> Triangle2d
    -> Triangle2d
    -> Triangle2d
    -> Triangle2d
    -> AdjacencyList
    -> AdjacencyList
flipEdge triCount removedEdge newEdge old1 old2 flipped1 flipped2 cache =
    let
        x =
            trianglesShareEdge newEdge flipped1 flipped2

        y =
            trianglesShareEdge removedEdge old1 old2

        z =
            if triangleHasEdge removedEdge old1 && triangleHasEdge removedEdge old2 then
                ()
            else
                Debug.crash "wut"

        otherEdges =
            (triangleEdges old1 ++ triangleEdges old2)
                |> List.filter (\e -> hashEdge e /= hashEdge removedEdge)
                |> List.map
                    (\otherEdge ->
                        if triangleHasEdge otherEdge flipped1 then
                            ( otherEdge, flipped1 )
                        else if triangleHasEdge otherEdge flipped2 then
                            ( otherEdge, flipped2 )
                        else
                            Debug.crash ("so wrong")
                    )

        mapper sharedEdge newTriangle ( t1, t2 ) =
            let
                _ =
                    trianglesShareEdge sharedEdge t1 t2

                x =
                    if triangleHasEdge sharedEdge newTriangle then
                        ()
                    else
                        Debug.crash "new triangle does not have shared edge"
            in
                if shareTwoEdges newTriangle t1 || hashTriangle t1 == hashTriangle old1 || hashTriangle t1 == hashTriangle old2 then
                    ( newTriangle, t2 )
                        |> uncurry (trianglesShareEdge sharedEdge)
                else if shareTwoEdges newTriangle t2 || hashTriangle t2 == hashTriangle old1 || hashTriangle t2 == hashTriangle old2 then
                    ( t1, newTriangle )
                        |> uncurry (trianglesShareEdge sharedEdge)
                else if hashTriangle t1 == hashTriangle old1 || hashTriangle t2 == hashTriangle old1 then
                    Debug.crash ("these triangles are WRONG" ++ toString ( sharedEdge, t1, t2 ) ++ " ====>              " ++ toString ( removedEdge, newTriangle ))
                else
                    ( t1, t2 )

        folder ( sharedEdge, newTriangle ) accum =
            accum

        -- |> AdjacencyList.update sharedEdge (Maybe.map (mapper sharedEdge newTriangle))
        f cache =
            List.foldl folder cache otherEdges
    in
        Debug.crash "unused"


shareTwoEdges t1 t2 =
    let
        edgeSet triangle =
            triangle
                |> triangleEdges
                |> List.map hashEdge
                |> Set.fromList
    in
        Set.intersect (edgeSet t1) (edgeSet t2)
            |> Set.toList
            |> (\inCommon ->
                    case inCommon of
                        [ _, _ ] ->
                            True

                        _ ->
                            False
               )


trianglesShareEdge edge t1 t2 =
    case splitOnShared t1 t2 of
        Just { shared } ->
            if hashEdge shared == hashEdge edge then
                ( t1, t2 )
            else
                Debug.crash "triangles share the wrong edge"

        Nothing ->
            Debug.crash "triangles don't  share an edge"


doesNotContainEdge : LineSegment2d -> AdjacencyList -> AdjacencyList
doesNotContainEdge edge cache =
    case AdjacencyList.get edge cache of
        Just _ ->
            Debug.crash "edge should not exist"

        Nothing ->
            cache


sharesEdge tri1 tri2 =
    case splitOnShared tri1 tri2 of
        Nothing ->
            False

        Just _ ->
            True


triangleHasEdge edge triangle =
    List.member (hashEdge edge) (List.map hashEdge (triangleEdges triangle))


triangleEdges : Triangle2d -> List LineSegment2d
triangleEdges triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.edges triangle
    in
        [ p1, p2, p3 ]


triangleCountIs n cache =
    let
        size =
            List.length (AdjacencyList.uniqueTriangles cache)
    in
        if size == n then
            cache
        else
            Debug.crash ("size is incorrect, expected " ++ toString n ++ " but got  " ++ toString size)


flipTriangles : AdjacencyList -> ( Int, AdjacencyList )
flipTriangles triangles =
    flipTrianglesHelper ( 0, AdjacencyList.empty ) triangles


unwrapAdjacencyList : String -> Result String AdjacencyList -> AdjacencyList
unwrapAdjacencyList location alist =
    case alist of
        Err e ->
            Debug.crash <| "invalid adjacency list at " ++ location ++ " with error " ++ e

        Ok v ->
            v


flipTrianglesHelperWorking : ( Int, AdjacencyList ) -> AdjacencyList -> ( Int, AdjacencyList )
flipTrianglesHelperWorking ( accumCount, accum ) remainingTriangles =
    case remainingTriangles |> AdjacencyList.toList of
        [] ->
            ( accumCount, accum )

        ( edge, tris ) :: _ ->
            let
                ( t1, t2 ) =
                    ( tris.tri1, tris.tri2 )
            in
                case checkAndFlipTrianglePair t1 t2 of
                    Just ( flipped1, flipped2, newSharedEdge ) ->
                        let
                            otherEdges =
                                (triangleEdges t1 ++ triangleEdges t2)
                                    |> List.filter (\e -> hashEdge e /= hashEdge edge)

                            replacer sharedEdge triangles =
                                mapBoth (replaceTriangle edge (List.filter (triangleHasEdge sharedEdge) [ flipped1, flipped2 ])) ( triangles.tri1, triangles.tri2 )

                            folder otherEdge =
                                AdjacencyList.update otherEdge (Maybe.map (replacer otherEdge))
                        in
                            flipTrianglesHelperWorking
                                ( accumCount + 1
                                , List.foldl folder accum otherEdges
                                    |> AdjacencyList.insert newSharedEdge ( flipped1, flipped2 )
                                    |> unwrapAdjacencyList "flipTrianglesHelper insert"
                                )
                                (List.foldl folder remainingTriangles otherEdges
                                    |> AdjacencyList.remove edge
                                )

                    Nothing ->
                        flipTrianglesHelperWorking
                            ( accumCount
                            , AdjacencyList.insert edge ( t1, t2 ) accum
                                |> unwrapAdjacencyList "flipTrianglesHelper insert 3"
                            )
                            (AdjacencyList.remove edge remainingTriangles)


flipTrianglesHelper : ( Int, AdjacencyList ) -> AdjacencyList -> ( Int, AdjacencyList )
flipTrianglesHelper ( accumCount, accum ) remainingTriangles =
    case remainingTriangles |> AdjacencyList.toProofList of
        [] ->
            ( accumCount, accum )

        proof :: _ ->
            let
                ( edge, tris ) =
                    AdjacencyList.readProof proof

                ( t1, t2 ) =
                    ( tris.tri1, tris.tri2 )
            in
                case checkAndFlipTrianglePairNew proof of
                    Just newProof ->
                        let
                            ( newSharedEdge, newTris ) =
                                AdjacencyList.readProof newProof

                            ( flipped1, flipped2 ) =
                                ( newTris.tri1, newTris.tri2 )

                            _ =
                                case checkAndFlipTrianglePair t1 t2 of
                                    Just ( x1, x2, otherSharedEdge ) ->
                                        if not (x1 == flipped1 && x2 == flipped2 && otherSharedEdge == newSharedEdge) then
                                            Debug.crash "not actually the same"
                                        else
                                            ()

                                    Nothing ->
                                        let
                                            newProof =
                                                AdjacencyList.proofAdjacency t1 t2

                                            new =
                                                newProof
                                                    |> Maybe.map checkAndFlipTrianglePairNew
                                        in
                                            Debug.crash ("actually all is wrong" ++ toString ( t1, t2, proof, checkAndFlipTrianglePairNew proof, checkAndFlipTrianglePair t1 t2, new, newProof ))

                            otherEdges =
                                (triangleEdges t1 ++ triangleEdges t2)
                                    |> List.filter (\e -> hashEdge e /= hashEdge edge)

                            replacer sharedEdge triangles =
                                mapBoth (replaceTriangle edge (List.filter (triangleHasEdge sharedEdge) [ flipped1, flipped2 ])) ( triangles.tri1, triangles.tri2 )

                            folder otherEdge =
                                AdjacencyList.update otherEdge (Maybe.map (replacer otherEdge))
                        in
                            flipTrianglesHelper
                                ( accumCount + 1
                                , List.foldl folder accum otherEdges
                                    |> AdjacencyList.insert newSharedEdge ( flipped1, flipped2 )
                                    |> unwrapAdjacencyList "flipTrianglesHelper insert"
                                )
                                (List.foldl folder remainingTriangles otherEdges
                                    |> AdjacencyList.remove edge
                                )

                    Nothing ->
                        flipTrianglesHelper
                            ( accumCount
                            , AdjacencyList.insert edge ( t1, t2 ) accum
                                |> unwrapAdjacencyList "flipTrianglesHelper insert 3"
                            )
                            (AdjacencyList.remove edge remainingTriangles)


mapBoth f ( a, b ) =
    ( f a, f b )


replaceTriangle removedEdge newTriangles existingTriangle =
    if triangleHasEdge removedEdge existingTriangle then
        List.foldl
            (\newTriangle existing ->
                if sharesEdge newTriangle existing then
                    newTriangle
                else
                    existing
            )
            existingTriangle
            newTriangles
    else
        existingTriangle


makeClockwise triangle =
    if Triangle2d.clockwiseArea triangle < 0 then
        let
            ( p1, p2, p3 ) =
                Triangle2d.vertices triangle
        in
            Triangle2d.fromVertices ( p3, p2, p1 )
    else
        triangle


{-| Flip triangles until stability is reached (no new triangles are flipped)
-}
flipTrianglesUntilFixpoint : List Triangle2d -> List Triangle2d
flipTrianglesUntilFixpoint =
    let
        go triangles =
            case flipTriangles triangles of
                ( 0, newTriangles ) ->
                    -- no progress was made
                    AdjacencyList.uniqueTriangles newTriangles

                ( n, newTriangles ) ->
                    go newTriangles
    in
        go << unwrapAdjacencyList "flipTrianglesUntilFixpoint" << AdjacencyList.fromTriangles


{-| Flip triangles until stability is reached (no new triangles are flipped)
-}
flipTrianglesUntilFixpointOld : List Triangle2d -> List Triangle2d
flipTrianglesUntilFixpointOld =
    let
        go triangles =
            case flipTrianglesOld triangles of
                ( 0, newTriangles ) ->
                    -- no progress was made
                    newTriangles

                ( _, newTriangles ) ->
                    go newTriangles
    in
        go


flipTrianglesOld : List Triangle2d -> ( Int, List Triangle2d )
flipTrianglesOld triangles =
    flipTrianglesHelperOld ( 0, [] ) triangles


flipTrianglesHelperOld : ( Int, List Triangle2d ) -> List Triangle2d -> ( Int, List Triangle2d )
flipTrianglesHelperOld ( accumCount, accum ) remainingTriangles =
    case remainingTriangles of
        [] ->
            ( accumCount, List.reverse accum )

        t :: ts ->
            let
                folder triangle ( numberFlipped, current, accum ) =
                    case checkAndFlipTrianglePair current triangle of
                        Just ( newCurrent, newTriangle, _ ) ->
                            ( 1 + accumCount, newCurrent, newTriangle :: accum )

                        Nothing ->
                            ( accumCount, current, triangle :: accum )

                ( numberFlippedFirst, done, rest ) =
                    List.foldr folder ( 0, t, [] ) ts
            in
                flipTrianglesHelperOld ( numberFlippedFirst + accumCount, done :: accum ) rest


{-| If the two input triangles share an edge, return that edge and the two remaining points
-}
splitOnShared : Triangle2d -> Triangle2d -> Maybe { shared : LineSegment2d, other1 : Point2d, other2 : Point2d }
splitOnShared t1 t2 =
    case hasCommonEdge t1 t2 of
        Nothing ->
            Nothing

        Just shared ->
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints shared

                other1 =
                    let
                        ( e1, e2, e3 ) =
                            Triangle2d.vertices t1
                    in
                        if e1 /= p1 && e1 /= p2 then
                            e1
                        else if e2 /= p1 && e2 /= p2 then
                            e2
                        else
                            e3

                other2 =
                    let
                        ( e1, e2, e3 ) =
                            Triangle2d.vertices t2
                    in
                        if e1 /= p1 && e1 /= p2 then
                            e1
                        else if e2 /= p1 && e2 /= p2 then
                            e2
                        else
                            e3
            in
                Just { shared = shared, other1 = other1, other2 = other2 }


{-| Put the shared edge in the expected position
-}
sortOnShared : Triangle2d -> Triangle2d -> Maybe ( Triangle2d, Triangle2d, LineSegment2d )
sortOnShared t1 t2 =
    Maybe.map
        (\{ shared, other1, other2 } ->
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints shared
            in
                ( Triangle2d.fromVertices ( p1, p2, other1 )
                , Triangle2d.fromVertices ( p1, other2, p2 )
                , shared
                )
        )
        (splitOnShared t1 t2)


{-| Flip the shared edge (if it exists) between two triangles
-}
flippedSharedEdge : Triangle2d -> Triangle2d -> Maybe ( Triangle2d, Triangle2d )
flippedSharedEdge t1 t2 =
    Maybe.map
        (\{ shared, other1, other2 } ->
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints shared
            in
                ( Triangle2d.fromVertices ( p1, p2, other1 )
                , Triangle2d.fromVertices ( p1, other2, p2 )
                )
        )
        (splitOnShared t1 t2)


{-| The first possible triangulation
-}
triangulation1 : Triangle2d -> Triangle2d -> Result DegenerateTriangle ( Float, Float )
triangulation1 triOrdered1 triOrdered2 =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triOrdered1

        ( q1, q2, q3 ) =
            Triangle2d.vertices triOrdered2

        quad =
            { zero = p1, one = p3, two = q3, three = q2 }

        triangle1 =
            Triangle2d.fromVertices ( quad.zero, quad.two, quad.one )

        triangle3 =
            Triangle2d.fromVertices ( quad.two, quad.zero, quad.three )
    in
        Result.map2 (,) (calcTriangleAngle triangle1) (calcTriangleAngle triangle3)


{-| The first possible triangulation
-}
triangulation2 : Triangle2d -> Triangle2d -> Result DegenerateTriangle ( ( Float, Triangle2d ), ( Float, Triangle2d ), LineSegment2d )
triangulation2 triOrdered1 triOrdered2 =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triOrdered1

        ( q1, q2, q3 ) =
            Triangle2d.vertices triOrdered2

        quad =
            { zero = p1, one = p3, two = q3, three = q2 }

        triangle2 =
            Triangle2d.fromVertices ( quad.one, quad.three, quad.two )

        triangle4 =
            Triangle2d.fromVertices ( quad.three, quad.one, quad.zero )
    in
        Result.map2 (,) (calcTriangleAngle triangle2) (calcTriangleAngle triangle4)
            |> Result.map (\( t2, t4 ) -> ( ( t2, triangle2 ), ( t4, triangle4 ), LineSegment2d.fromEndpoints ( quad.one, quad.three ) ))


checkAndFlipTrianglePairNew : AdjacencyProof -> Maybe AdjacencyProof
checkAndFlipTrianglePairNew proof_ =
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
checkAndFlipTrianglePair : Triangle2d -> Triangle2d -> Maybe ( Triangle2d, Triangle2d, LineSegment2d )
checkAndFlipTrianglePair tri1 tri2 =
    case sortOnShared tri1 tri2 of
        Nothing ->
            Nothing

        Just ( triOrdered1, triOrdered2, shared ) ->
            let
                _ =
                    Debug.log "old: triangles" ( triOrdered1, triOrdered2 )
            in
                case triangulation1 triOrdered1 triOrdered2 of
                    Err _ ->
                        Debug.log "triangulation 1" Nothing

                    Ok ( t1, t3 ) ->
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
                                case triangulation2 triOrdered1 triOrdered2 of
                                    Err _ ->
                                        Debug.log "triangulation 2" Nothing

                                    Ok ( ( t2, flipped1 ), ( t4, flipped2 ), newShared ) ->
                                        -- t1 + t2 + t3 + t4 == 2 * pi
                                        if flipDegenerateTri && (t2 > pi || t4 > pi) then
                                            -- Flipping would create an overlap
                                            Debug.log "flipDegenerateTri" Nothing
                                        else if t2 == pi || t4 == pi then
                                            -- Flipping would create triangle of zero size
                                            Debug.log "flipping would create triangle of zero size" Nothing
                                        else
                                            let
                                                flipAngTotal =
                                                    t2 + t4
                                            in
                                                if flipForDelaunay && flipAngTotal >= angTotal then
                                                    -- No improvement when flipped, so abort flip
                                                    Debug.log ("old: no improvement " ++ toString ( flipForDelaunay, flipAngTotal, angTotal )) Nothing
                                                else
                                                    Just ( flipped1, flipped2, newShared )
                            else
                                Debug.log "old: else" Nothing


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
                    if area == 0 then
                        Just lineSegment
                    else if area > 0 then
                        Just lineSegment
                    else
                        Nothing
            )


type VisibleEdge
    = Visible LineSegment2d
    | On LineSegment2d


isClockwise : Triangle2d -> Bool
isClockwise triangle =
    if Triangle2d.clockwiseArea triangle >= 0 then
        True
    else
        False


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



{-
   x =
       AdjacencyProof
           { tri1 = Triangle2d ( Point2d ( 0, 390 ), Point2d ( 70, 370 ), Point2d ( 10, 350 ) )
           , tri2 = Triangle2d ( Point2d ( 10, 350 ), Point2d ( 50, 160 ), Point2d ( 0, 390 ) )
           , otherVertex1 = Point2d ( 70, 370 )
           , otherVertex2 = Point2d ( 50, 160 )
           , sharedEdge = LineSegment2d ( Point2d ( 0, 390 ), Point2d ( 10, 350 ) )
           }


   y =
       AdjacencyProof
           { tri1 = Triangle2d ( Point2d ( 0, 390 ), Point2d ( 70, 370 ), Point2d ( 10, 350 ) )
           , tri2 = Triangle2d ( Point2d ( 10, 350 ), Point2d ( 50, 160 ), Point2d ( 0, 390 ) )
           , sharedEdge = LineSegment2d ( Point2d ( 10, 350 ), Point2d ( 0, 390 ) )
           , otherVertex1 = Point2d ( 70, 370 )
           , otherVertex2 = Point2d ( 50, 160 )
           }
-}
