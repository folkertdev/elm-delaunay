module Delaunay exposing (..)

import Point2d exposing (Point2d)
import Triangle2d exposing (Triangle2d)
import Circle2d
import Direction2d
import Axis2d
import Array.Hamt as Array exposing (Array)
import Set
import BoundingBox2d


{-| The state of the triangulation, using flat arrays for vertices, edges, and faces
-}
type alias GeometryState =
    { points : Array Point2d
    , edges : Array Edge
    , faces : Array Face
    }


{-| A face

contains edge indices. Center and radius are cached.
marked is used to mark faces for deletion (when true the face is considered deleted)
superTrianglePoints stores whether the face has points in common with the initial triangle
-}
type alias Face =
    { edge1 : Int
    , edge2 : Int
    , edge3 : Int
    , center : Point2d
    , radiusSquared : Float
    , marked : Bool
    , superTrianglePoints : SuperTrianglePoints
    }


{-| An Edge

start and end are vertex indices.
The coedge is an edge between the same two vertices, but part of another face.
next is the next edge of the triangle this edge is part of
-}
type alias Edge =
    { start : Int
    , end : Int
    , coedge : Maybe Int
    , face : Int
    , next : Int
    }


delaunayTriangulation : Array Point2d -> List Triangle2d
delaunayTriangulation rawPoints =
    let
        ( superTriangle, points ) =
            checkInput identity rawPoints

        initialGeometryState : GeometryState
        initialGeometryState =
            let
                initialEdges =
                    Array.empty
                        |> Array.push { start = 0, end = 1, face = 0, coedge = Nothing, next = 1 }
                        |> Array.push { start = 1, end = 2, face = 0, coedge = Nothing, next = 2 }
                        |> Array.push { start = 2, end = 0, face = 0, coedge = Nothing, next = 0 }

                ( center, radius ) =
                    circumcenterAndRadius superTriangle

                initialFace =
                    { edge1 = 0
                    , edge2 = 1
                    , edge3 = 2
                    , center = center
                    , radiusSquared = radius * radius
                    , marked = False
                    , superTrianglePoints = Three
                    }
            in
                { faces = Array.empty |> Array.push initialFace
                , edges = initialEdges
                , points = points
                }

        folder ( pointIndex, point ) geometryState =
            let
                ( closeFaces, { edges, faces } as newGeometryState ) =
                    findCloseFaces point geometryState
            in
                case firstBoundaryEdge closeFaces edges faces of
                    Nothing ->
                        newGeometryState

                    Just ( i, firstEdge ) ->
                        let
                            boundary =
                                boundaryLoop i firstEdge edges faces
                        in
                            newGeometryState
                                |> createFaces ( pointIndex, point ) boundary
    in
        points
            |> Array.slice 3 (Array.length points)
            |> Array.indexedMap (\i e -> ( i + 3, e ))
            |> Array.foldl folder initialGeometryState
            |> toTriangulation


checkInput : (vertex -> Point2d) -> Array vertex -> ( Triangle2d, Array Point2d )
checkInput toPoint vertices =
    let
        folder vertex ( seenPoints, boundingBoxAccum, vertexAccum ) =
            let
                point =
                    toPoint vertex

                coordinates =
                    Point2d.coordinates point
            in
                if Set.member coordinates seenPoints then
                    ( seenPoints, boundingBox, vertexAccum )
                else
                    ( Set.insert coordinates seenPoints
                    , BoundingBox2d.hull (BoundingBox2d.singleton point) boundingBoxAccum
                    , point :: vertexAccum
                    )

        ( points, boundingBox, uniqueVertices ) =
            Array.foldl folder ( Set.empty, BoundingBox2d.singleton Point2d.origin, [] ) vertices

        extrema =
            let
                e =
                    BoundingBox2d.extrema boundingBox
            in
                { e
                    | minX = e.minX - 10
                    , minY = e.minY - 10
                    , maxX = e.maxX + 10
                    , maxY = e.maxY + 10
                }

        superP1 =
            Point2d.fromCoordinates ( extrema.minX, extrema.minY )

        superP2 =
            Point2d.fromCoordinates ( extrema.maxX * 2, extrema.minY )

        superP3 =
            Point2d.fromCoordinates ( extrema.minX, extrema.maxY * 2 )

        superTriangle =
            Triangle2d.fromVertices ( superP1, superP2, superP3 )
    in
        ( superTriangle
        , uniqueVertices
            |> List.reverse
            |> (\list -> [ superP1, superP2, superP3 ] ++ list)
            |> Array.fromList
        )


newFace : ( Int, Point2d ) -> ( Int, Edge ) -> GeometryState -> ( Int, GeometryState )
newFace ( pointIndex, point ) ( edgeIndex, edge ) =
    let
        addNewFace ({ edges, faces, points } as state) =
            let
                ( center, radius ) =
                    (Maybe.map3 (,,) (Just point) (Array.get edge.start points) (Array.get edge.end points))
                        |> Maybe.map
                            (\( a, b, c ) ->
                                Triangle2d.fromVertices
                                    ( a
                                    , b
                                    , c
                                    )
                            )
                        |> Maybe.map circumcenterAndRadius
                        |> Maybe.withDefault ( point, 0 )

                faceIndex =
                    Array.length faces

                firstEdge : Edge
                firstEdge =
                    { start = pointIndex
                    , end = edge.start
                    , next = edgeIndex
                    , coedge = Nothing
                    , face = faceIndex
                    }

                firstEdgeIndex =
                    Array.length edges

                secondEdgeIndex =
                    Array.length edges + 1

                secondEdge : Edge
                secondEdge =
                    { start = edge.end
                    , end = pointIndex
                    , next = Array.length edges
                    , coedge = Nothing
                    , face = faceIndex
                    }

                result : Face
                result =
                    { edge1 = firstEdgeIndex
                    , edge2 = edgeIndex
                    , edge3 = secondEdgeIndex
                    , center = center
                    , radiusSquared = radius * radius
                    , marked = False
                    , superTrianglePoints = findSuperTrianglePoints ( firstEdge.start, firstEdge.end, secondEdge.start )
                    }

                newEdge =
                    { edge | next = secondEdgeIndex, face = faceIndex }

                newEdges =
                    edges
                        |> updateArray edgeIndex (\_ -> newEdge)
                        |> Array.push firstEdge
                        |> Array.push secondEdge

                newFaces =
                    faces
                        |> Array.push result

                newState =
                    { state
                        | edges = newEdges
                        , faces = newFaces
                    }
            in
                ( faceIndex
                , newState
                )
    in
        addNewFace


createFaces : ( Int, Point2d ) -> List ( Int, Edge ) -> GeometryState -> GeometryState
createFaces ( pointIndex, point ) boundary geometryState =
    let
        mapper : ( Bool, Int ) -> ( Bool, Int ) -> ( Bool, Int ) -> GeometryState -> GeometryState
        mapper ( _, first ) ( isLast, currentIndex ) ( _, previousIndex ) ({ edges, faces } as state) =
            case ( Array.get currentIndex faces, Array.get previousIndex faces ) of
                ( Just current, Just previous ) ->
                    let
                        connectToStart =
                            if not isLast then
                                identity
                            else
                                case Array.get first faces of
                                    Just firstFace ->
                                        updateArray firstFace.edge1 (\edge -> { edge | coedge = Just current.edge3 })
                                            >> updateArray current.edge3 (\edge -> { edge | coedge = Just firstFace.edge1 })

                                    _ ->
                                        identity
                    in
                        { state
                            | edges =
                                edges
                                    |> updateArray current.edge1 (\edge -> { edge | coedge = Just previous.edge3 })
                                    |> updateArray previous.edge3 (\edge -> { edge | coedge = Just current.edge1 })
                                    |> connectToStart
                        }

                _ ->
                    Debug.crash "something is wrong"

        traverse : (a -> s -> ( b, s )) -> List a -> s -> ( List b, s )
        traverse tagger list initialState =
            let
                folder item ( accum, state ) =
                    let
                        ( b, newState ) =
                            tagger item state
                    in
                        ( b :: accum, newState )
            in
                List.foldl folder ( [], initialState ) list
                    |> Tuple.mapFirst List.reverse

        ( newFaces, newState ) =
            traverse (newFace ( pointIndex, point )) boundary geometryState

        numberOfFaces =
            List.length newFaces - 1
    in
        case List.indexedMap (\i e -> ( i == numberOfFaces, e )) newFaces of
            [] ->
                newState

            (x :: xs) as indexed ->
                List.map2 (mapper x) (xs ++ [ x ]) indexed
                    |> List.foldl (>>) identity
                    |> (\f -> f newState)


toTriangulation : GeometryState -> List Triangle2d
toTriangulation state =
    let
        getEdges : Face -> Array a -> Maybe ( a, a, a )
        getEdges face edges =
            Maybe.map3 (,,)
                (Array.get face.edge1 edges)
                (Array.get face.edge2 edges)
                (Array.get face.edge3 edges)

        mapper : Face -> Maybe Triangle2d
        mapper face =
            if face.marked then
                Nothing
            else
                getEdges face state.edges
                    |> Maybe.andThen
                        (\( e1, e2, e3 ) ->
                            let
                                vertices =
                                    [ e1.start, e1.end, e3.start ]

                                i1 =
                                    e1.start

                                i2 =
                                    e1.end

                                i3 =
                                    e3.start
                            in
                                if List.member 0 vertices || List.member 1 vertices || List.member 2 vertices then
                                    Nothing
                                else
                                    Maybe.map3 (,,)
                                        (Array.get i1 state.points)
                                        (Array.get i2 state.points)
                                        (Array.get i3 state.points)
                                        |> Maybe.map Triangle2d.fromVertices
                        )
    in
        state.faces
            |> Array.toList
            |> List.filterMap mapper



-- REMOVING FACES


{-| Find the faces close enough to a new point that they need to be removed
-}
findCloseFaces : Point2d -> GeometryState -> ( List Face, GeometryState )
findCloseFaces point ({ faces, points } as geometryState) =
    let
        folder face ( i, accum ) =
            if face.marked then
                ( i + 1, accum )
            else
                let
                    remove =
                        removeFace point face face.superTrianglePoints geometryState.points
                in
                    if remove then
                        ( i + 1, ( i, { face | marked = True } ) :: accum )
                    else
                        ( i + 1, accum )

        ( _, closeFaces ) =
            Array.foldl folder ( 0, [] ) faces
    in
        ( List.map Tuple.second closeFaces
        , { geometryState | faces = List.foldl (\( i, _ ) -> updateArray i (\face -> { face | marked = True })) faces closeFaces }
        )


{-| Keep track of how many vertices are shared with the super triangle
-}
type SuperTrianglePoints
    = Zero
    | One { shared : Int, other1 : Int, other2 : Int }
    | Two { shared1 : Int, shared2 : Int, other : Int }
    | Three


{-| Given three vertices, check how many are shared with the super triangle
-}
findSuperTrianglePoints : ( Int, Int, Int ) -> SuperTrianglePoints
findSuperTrianglePoints ( i1, i2, i3 ) =
    let
        vertices =
            [ i1, i2, i3 ]
    in
        case List.filter (\e -> e == 0 || e == 1 || e == 2) vertices of
            [] ->
                Zero

            [ p ] ->
                -- shares 1 vertex
                case vertices |> List.filter (\v -> v /= 0 && v /= 1 && v /= 2) of
                    [ p1, p2 ] ->
                        One { shared = p, other1 = p1, other2 = p2 }

                    _ ->
                        -- impossible
                        Three

            [ p1, p2 ] ->
                -- shares 2 vertices
                case vertices |> List.filter (\v -> v /= 0 && v /= 1 && v /= 2) of
                    [ p ] ->
                        Two { shared1 = p1, shared2 = p2, other = p }

                    _ ->
                        -- impossible
                        Three

            [ _, _, _ ] ->
                -- shares 3 vertices, so remove it
                Three

            _ ->
                -- impossible
                Three


{-| Actually determine whether a face should be removed given a new point

Most sources only consider triangles that don't share vertices with the initial triangle.
They produce wrong results if the initial triangle is not large enough.

The solution is to treat points of the initial vertex as though they are at infinity.
based on https://stackoverflow.com/questions/30741459/bowyer-watson-algorithm-how-to-fill-holes-left-by-removing-triangles-with-sup
-}
removeFace : Point2d -> Face -> SuperTrianglePoints -> Array Point2d -> Bool
removeFace point face superTrianglePoints points =
    case superTrianglePoints of
        Zero ->
            -- no shared vertices, remove if within circumcircle
            Point2d.squaredDistanceFrom point face.center < face.radiusSquared

        One { shared, other1, other2 } ->
            {- One vertex is shared with the super-triangle

               We pretend that this shared point is at infinity,
               which means that the circumcircle going through the points other1 and other2 is a straight line.

               We can then check that the new point is on the same side of that line as the shared point.
               If so, the point is within the "circumcircle" and the triangle needs to be removed.
            -}
            let
                vertices =
                    Maybe.map3 (,,)
                        (Array.get shared points)
                        (Array.get other1 points)
                        (Array.get other2 points)

                rest ( sharedPoint, p1, p2 ) =
                    case Maybe.map (Axis2d.through p1) (Direction2d.from p1 p2) of
                        Just axis ->
                            case compare (Point2d.signedDistanceFrom axis point) 0 of
                                EQ ->
                                    False

                                LT ->
                                    case compare (Point2d.signedDistanceFrom axis sharedPoint) 0 of
                                        GT ->
                                            False

                                        EQ ->
                                            -- never happens
                                            True

                                        LT ->
                                            True

                                GT ->
                                    case compare (Point2d.signedDistanceFrom axis sharedPoint) 0 of
                                        LT ->
                                            False

                                        EQ ->
                                            -- never happens
                                            True

                                        GT ->
                                            True

                        Nothing ->
                            True
            in
                Maybe.map rest vertices
                    |> Maybe.withDefault True

        Two { shared1, shared2, other } ->
            {- Two vertices are shared with the super-triangle

               We pretend that both of these are at infinity,
               which means that the circumcircle going through the remaining point is a straight line parallel
               to a line between the points at infinity

               We can then check that the new point is on the same side of that line as the two shared point.
               If so, the point is within the "circumcircle" and the triangle needs to be removed.
            -}
            let
                vertices =
                    Maybe.map3 (,,)
                        (Array.get shared1 points)
                        (Array.get shared2 points)
                        (Array.get other points)

                rest ( inf1, inf2, oth ) =
                    case Maybe.map (Axis2d.through oth) (Direction2d.from inf1 inf2) of
                        Just axis ->
                            case compare (Point2d.signedDistanceFrom axis point) 0 of
                                EQ ->
                                    False

                                LT ->
                                    True

                                GT ->
                                    False

                        Nothing ->
                            True
            in
                Maybe.map rest vertices |> Maybe.withDefault True

        Three ->
            -- shares 3 vertices, so remove it
            True



-- BOUNDARIES


firstBoundaryEdge : List Face -> Array Edge -> Array Face -> Maybe ( Int, Edge )
firstBoundaryEdge closeFaces edges faces =
    let
        isBoundary : Edge -> Bool
        isBoundary edge =
            case edge.coedge of
                Nothing ->
                    True

                Just index ->
                    let
                        optionalFace =
                            Array.get edge.face faces

                        optionalCoface =
                            Array.get index edges
                                |> Maybe.andThen (\coedge -> Array.get coedge.face faces)
                    in
                        case ( optionalFace, optionalCoface ) of
                            ( Just face, Just coface ) ->
                                face.marked /= coface.marked

                            ( Nothing, Nothing ) ->
                                False

                            ( Just face, Nothing ) ->
                                face.marked /= True

                            ( Nothing, Just coface ) ->
                                True /= coface.marked

        boundaryEdge : Face -> Maybe ( Int, Edge )
        boundaryEdge face =
            let
                pipeline edgeIndex =
                    Array.get edgeIndex edges
                        |> Maybe.andThen
                            (\edge ->
                                if isBoundary edge then
                                    Just ( edgeIndex, edge )
                                else
                                    Nothing
                            )
            in
                case pipeline face.edge1 of
                    Just e1 ->
                        Just e1

                    Nothing ->
                        case pipeline face.edge2 of
                            Just e2 ->
                                Just e2

                            Nothing ->
                                pipeline face.edge3

        go facesToCheck =
            case facesToCheck of
                [] ->
                    Nothing

                f :: fs ->
                    case boundaryEdge f of
                        Nothing ->
                            go fs

                        Just e ->
                            Just e
    in
        go closeFaces


isBoundary : Edge -> Array Edge -> Array Face -> Bool
isBoundary edge edges faces =
    case edge.coedge of
        Nothing ->
            True

        Just index ->
            let
                optionalFace =
                    Array.get edge.face faces

                optionalCoface =
                    Array.get index edges
                        |> Maybe.andThen (\coedge -> Array.get coedge.face faces)
            in
                case ( optionalFace, optionalCoface ) of
                    ( Just face, Just coface ) ->
                        face.marked /= coface.marked

                    ( Nothing, Nothing ) ->
                        False

                    ( Just face, Nothing ) ->
                        face.marked /= True

                    ( Nothing, Just coface ) ->
                        True /= coface.marked


boundaryEdge : Face -> Array Edge -> Array Face -> Maybe ( Int, Edge )
boundaryEdge face edges faces =
    let
        pipeline edgeIndex =
            Array.get edgeIndex edges
                |> Maybe.andThen
                    (\edge ->
                        if isBoundary edge edges faces then
                            Just ( edgeIndex, edge )
                        else
                            Nothing
                    )
    in
        case pipeline face.edge1 of
            Just e1 ->
                Just e1

            Nothing ->
                case pipeline face.edge2 of
                    Just e2 ->
                        Just e2

                    Nothing ->
                        pipeline face.edge3


nextBoundary : Edge -> Array Edge -> Array Face -> Maybe ( Int, Edge )
nextBoundary firstEdge edges faces =
    let
        go : Int -> Edge -> Maybe ( Int, Edge )
        go index edge =
            if isBoundary edge edges faces then
                Just ( index, edge )
            else
                edge.coedge
                    |> Maybe.andThen
                        (\coedgeIndex ->
                            Array.get coedgeIndex edges
                                |> Maybe.andThen
                                    (\coedge ->
                                        Array.get coedge.next edges |> Maybe.andThen (go coedge.next)
                                    )
                        )
    in
        Array.get firstEdge.next edges
            |> Maybe.andThen (go firstEdge.next)


boundaryLoop : Int -> Edge -> Array Edge -> Array Face -> List ( Int, Edge )
boundaryLoop firstEdgeIndex firstEdge edges faces =
    let
        go accum optionalEdge =
            case optionalEdge of
                Nothing ->
                    List.reverse accum

                Just ( i, edge ) ->
                    if edge.start == firstEdge.start && edge.end == firstEdge.end then
                        (List.reverse accum)
                    else
                        go (( i, edge ) :: accum) (nextBoundary edge edges faces)
    in
        go [ ( firstEdgeIndex, firstEdge ) ] (nextBoundary firstEdge edges faces)



-- HELPERS


{-| center and radius of the circumcircle of a triangle.
Assumes the triangle is not degenerate
-}
circumcenterAndRadius : Triangle2d -> ( Point2d, Float )
circumcenterAndRadius triangle =
    triangle
        |> Triangle2d.circumcircle
        |> Maybe.map (\circle -> ( Circle2d.centerPoint circle, Circle2d.radius circle ))
        |> Maybe.withDefault ( Point2d.fromCoordinates ( 0, 0 ), 0 )


updateArray : Int -> (a -> a) -> Array a -> Array a
updateArray index tagger array =
    case Array.get index array of
        Nothing ->
            array

        Just item ->
            Array.set index (tagger item) array
