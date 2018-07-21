module AdjacencyList
    exposing
        ( AdjacencyList
        , AdjacencyProof
        , insertProof
        , empty
        , insert
        , remove
        , update
        , keys
        , fromList
        , toList
        , get
        , foldr
        , map
        , uniqueTriangles
        , fromTriangles
        , toAllDict
        , proofAdjacency
        , triangulation1
        , triangulation2
        , toProofList
        , readProof
        , sortOnShared
        , keySet
        , getProof
        )

import AllDict exposing (AllDict)
import LineSegment2d exposing (LineSegment2d)
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)
import Set exposing (Set)


type alias Edge =
    ( ( Float, Float ), ( Float, Float ) )


type AdjacencyList
    = AdjacencyList (AllDict LineSegment2d { tri1 : Triangle2d, tri2 : Triangle2d, otherVertex1 : Point2d, otherVertex2 : Point2d } Edge)


type alias AdjacentTriangles =
    { tri1 : Triangle2d, tri2 : Triangle2d, otherVertex1 : Point2d, otherVertex2 : Point2d }


hashTriangle : Triangle2d -> List ( Float, Float )
hashTriangle triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
        List.sort <| List.map Point2d.coordinates [ p1, p2, p3 ]


hashEdge : LineSegment2d -> Edge
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


empty : AdjacencyList
empty =
    AdjacencyList (AllDict.empty hashEdge)


singleton : LineSegment2d -> ( Triangle2d, Triangle2d ) -> Result String AdjacencyList
singleton key value =
    insert key value empty


insert : LineSegment2d -> ( Triangle2d, Triangle2d ) -> AdjacencyList -> Result String AdjacencyList
insert edge ( t1, t2 ) alist =
    case Maybe.map (\p -> insertProof p alist) (proofAdjacencyFromEdge edge t1 t2) of
        Just done ->
            Ok done

        Nothing ->
            Err <|
                toString
                    { error = "invalid insertion: triangles do not share edge"
                    , tri1 = t1
                    , tri2 = t2
                    , edge = edge
                    }


insertProof : AdjacencyProof -> AdjacencyList -> AdjacencyList
insertProof (AdjacencyProof proof) (AdjacencyList dict) =
    AllDict.insert proof.sharedEdge { tri1 = proof.tri1, tri2 = proof.tri2, otherVertex1 = proof.otherVertex1, otherVertex2 = proof.otherVertex2 } dict
        |> AdjacencyList


remove : LineSegment2d -> AdjacencyList -> AdjacencyList
remove edge (AdjacencyList dict) =
    AdjacencyList (AllDict.remove edge dict)


get : LineSegment2d -> AdjacencyList -> Maybe AdjacentTriangles
get edge (AdjacencyList dict) =
    AllDict.get edge dict


getProof : LineSegment2d -> AdjacencyList -> Maybe AdjacencyProof
getProof edge (AdjacencyList dict) =
    AllDict.get edge dict
        |> Maybe.map
            (\value ->
                AdjacencyProof
                    { tri1 = value.tri1
                    , tri2 = value.tri2
                    , otherVertex1 = value.otherVertex1
                    , otherVertex2 = value.otherVertex2
                    , sharedEdge = edge
                    }
            )


keys : AdjacencyList -> List LineSegment2d
keys (AdjacencyList dict) =
    AllDict.keys dict


fromList : List ( LineSegment2d, ( Triangle2d, Triangle2d ) ) -> Result String AdjacencyList
fromList list =
    case list of
        [] ->
            Ok empty

        ( e, ts ) :: xs ->
            (fromList xs)
                |> Result.andThen (\alist -> insert e ts alist)


toList : AdjacencyList -> List ( LineSegment2d, AdjacentTriangles )
toList (AdjacencyList dict) =
    AllDict.toList dict


keySet : AdjacencyList -> AllDict LineSegment2d () Edge
keySet (AdjacencyList dict) =
    AllDict.map (\_ _ -> ()) dict


toProofList : AdjacencyList -> List AdjacencyProof
toProofList (AdjacencyList dict) =
    let
        folder edge value accum =
            AdjacencyProof
                { tri1 = value.tri1
                , tri2 = value.tri2
                , otherVertex1 = value.otherVertex1
                , otherVertex2 = value.otherVertex2
                , sharedEdge = edge
                }
                :: accum
    in
        AllDict.foldr folder [] dict


readProof : AdjacencyProof -> ( LineSegment2d, AdjacentTriangles )
readProof (AdjacencyProof proof) =
    ( proof.sharedEdge
    , { tri1 = proof.tri1
      , tri2 = proof.tri2
      , otherVertex1 = proof.otherVertex1
      , otherVertex2 = proof.otherVertex2
      }
    )


foldr : (LineSegment2d -> AdjacentTriangles -> a -> a) -> a -> AdjacencyList -> a
foldr folder default (AdjacencyList dict) =
    AllDict.foldr folder default dict


map : (LineSegment2d -> AdjacentTriangles -> ( Triangle2d, Triangle2d )) -> AdjacencyList -> Result String AdjacencyList
map mapper =
    let
        folder key value accum =
            accum |> Result.andThen (\validAccum -> insert key (mapper key value) validAccum)
    in
        foldr folder (Ok empty)


update : LineSegment2d -> (Maybe AdjacentTriangles -> Maybe ( Triangle2d, Triangle2d )) -> AdjacencyList -> AdjacencyList
update key updater (AdjacencyList dict) =
    let
        actualUpdater item =
            case updater item |> Maybe.andThen (\( t1, t2 ) -> proofAdjacency t1 t2) of
                Nothing ->
                    Nothing

                Just (AdjacencyProof proof) ->
                    Just
                        { tri1 = proof.tri1
                        , tri2 = proof.tri2
                        , otherVertex1 = proof.otherVertex1
                        , otherVertex2 = proof.otherVertex2
                        }
    in
        AdjacencyList (AllDict.update key actualUpdater dict)


uniqueTriangles : AdjacencyList -> List Triangle2d
uniqueTriangles (AdjacencyList dict) =
    let
        folder _ { tri1, tri2 } accum =
            accum
                |> AllDict.insert tri1 ()
                |> AllDict.insert tri2 ()
    in
        dict
            |> AllDict.foldr folder (AllDict.empty hashTriangle)
            |> AllDict.keys


edgeSet : Triangle2d -> Set Edge
edgeSet triangle =
    let
        ( p1, p2, p3 ) =
            Triangle2d.edges triangle
    in
        [ p1, p2, p3 ]
            |> List.map hashEdge
            |> Set.fromList


type OneOrTwo a
    = One a
    | Two a a


edges : List Triangle2d -> AllDict LineSegment2d (OneOrTwo Triangle2d) Edge
edges triangles =
    let
        folder triangle accum =
            let
                ( p1, p2, p3 ) =
                    (Triangle2d.edges triangle)
            in
                [ p1, p2, p3 ]
                    |> List.foldl (\edge -> AllDict.update edge (updater edge triangle)) accum

        updater k triangle existing =
            case existing of
                Nothing ->
                    Just (One triangle)

                Just (One current) ->
                    Just (Two current triangle)

                Just (Two t1 t2) ->
                    if hashTriangle t1 == hashTriangle triangle || hashTriangle t2 == hashTriangle triangle then
                        Just (Two t1 t2)
                    else
                        Debug.crash ("three?" ++ toString ( t1, t2, triangle ))
    in
        triangles
            |> List.map (\tri -> ( tri, () ))
            |> AllDict.fromList hashTriangle
            |> AllDict.keys
            |> List.foldr folder (AllDict.empty hashEdge)


fromTriangles : List Triangle2d -> Result String AdjacencyList
fromTriangles triangles =
    AllDict.foldr
        (\key value accum ->
            case value of
                One _ ->
                    accum

                Two x y ->
                    accum |> Result.andThen (\validAccum -> insert key ( x, y ) validAccum)
        )
        (Ok empty)
        (edges triangles)


toAllDict : AdjacencyList -> AllDict LineSegment2d AdjacentTriangles ( ( Float, Float ), ( Float, Float ) )
toAllDict (AdjacencyList dict) =
    dict


type AdjacencyProof
    = AdjacencyProof { tri1 : Triangle2d, tri2 : Triangle2d, sharedEdge : LineSegment2d, otherVertex1 : Point2d, otherVertex2 : Point2d }


flipProof : AdjacencyProof -> AdjacencyProof
flipProof (AdjacencyProof proof) =
    let
        newSharedEdge =
            LineSegment2d.fromEndpoints ( proof.otherVertex1, proof.otherVertex2 )

        ( s1, s2 ) =
            LineSegment2d.endpoints proof.sharedEdge

        ( o1, o2 ) =
            ( proof.otherVertex1, proof.otherVertex2 )

        tri1 =
            let
                temp =
                    Triangle2d.fromVertices ( s1, o2, o1 )
            in
                if Triangle2d.clockwiseArea temp >= 0 then
                    temp
                else
                    Triangle2d.fromVertices ( o1, o2, s1 )

        tri2 =
            let
                temp =
                    Triangle2d.fromVertices ( s2, o2, o1 )
            in
                if Triangle2d.clockwiseArea temp >= 0 then
                    temp
                else
                    Triangle2d.fromVertices ( o1, o2, s2 )
    in
        AdjacencyProof
            { tri1 = tri1
            , tri2 = tri2
            , sharedEdge = newSharedEdge
            , otherVertex1 = s1
            , otherVertex2 = s2
            }


proofAdjacency : Triangle2d -> Triangle2d -> Maybe AdjacencyProof
proofAdjacency tri1 tri2 =
    let
        edgesT1 =
            (edgeSet tri1)

        edgesT2 =
            (edgeSet tri2)

        combinedEdgeSet =
            (Set.intersect edgesT2 edgesT1)
    in
        case hasCommonEdge tri1 tri2 of
            Just edge ->
                let
                    mapBoth f ( a, b ) =
                        ( f a, f b )

                    ( p1, p2 ) =
                        LineSegment2d.endpoints edge

                    findOtherEdge edges =
                        edges
                            |> Set.toList
                            |> List.concatMap (\( a, b ) -> [ a, b ])
                            |> List.filter (\p -> p /= Point2d.coordinates p1 && p /= Point2d.coordinates p2)
                            |> List.head
                            |> Maybe.map Point2d.fromCoordinates
                in
                    case Maybe.map2 (,) (findOtherEdge edgesT1) (findOtherEdge edgesT2) of
                        Just ( s1, s2 ) ->
                            Just <|
                                AdjacencyProof
                                    { tri1 = tri1
                                    , tri2 = tri2
                                    , sharedEdge = edge
                                    , otherVertex1 = s1
                                    , otherVertex2 = s2
                                    }

                        Nothing ->
                            Nothing

            Nothing ->
                Nothing


proofAdjacencyFromEdge : LineSegment2d -> Triangle2d -> Triangle2d -> Maybe AdjacencyProof
proofAdjacencyFromEdge edge tri1 tri2 =
    let
        edgesT1 =
            (edgeSet tri1)

        edgesT2 =
            (edgeSet tri2)

        combinedEdgeSet =
            (Set.intersect edgesT1 edgesT2)
    in
        if Set.member (hashEdge edge) combinedEdgeSet then
            proofAdjacency tri1 tri2
        else
            Nothing


makeClockwise : Triangle2d -> Triangle2d
makeClockwise triangle =
    if Triangle2d.clockwiseArea triangle < 0 then
        let
            ( p1, p2, p3 ) =
                Triangle2d.vertices triangle
        in
            Triangle2d.fromVertices ( p3, p2, p1 )
    else
        triangle


{-| Put the shared edge in the expected position
-}
sortOnShared : AdjacencyProof -> AdjacencyProof
sortOnShared (AdjacencyProof proof) =
    let
        ( newTri1, newTri2, newShared ) =
            sortOnSharedNew (AdjacencyProof proof)
    in
        AdjacencyProof
            { proof
                | tri1 = newTri1
                , tri2 = newTri2
                , sharedEdge = newShared
            }


sortOnSharedNew : AdjacencyProof -> ( Triangle2d, Triangle2d, LineSegment2d )
sortOnSharedNew (AdjacencyProof proof) =
    let
        t1 =
            proof.tri1

        t2 =
            proof.tri2

        shared =
            proof.sharedEdge

        other1 =
            proof.otherVertex1

        other2 =
            proof.otherVertex2
    in
        let
            ( p1, p2 ) =
                LineSegment2d.endpoints shared
        in
            ( Triangle2d.fromVertices ( p1, p2, other1 )
            , Triangle2d.fromVertices ( p1, other2, p2 )
            , shared
            )


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


{-| The first possible triangulation
-}
triangulation1 : AdjacencyProof -> Maybe ( Float, Float )
triangulation1 (AdjacencyProof proof) =
    let
        triOrdered1 =
            proof.tri1

        triOrdered2 =
            proof.tri2

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
        Maybe.map2 (,) (calcTriangleAngle triangle1) (calcTriangleAngle triangle3)


{-| The first possible triangulation
-}
triangulation2 : AdjacencyProof -> Maybe ( Float, Float, AdjacencyProof )
triangulation2 (AdjacencyProof proof) =
    let
        ( triOrdered1, triOrdered2 ) =
            ( proof.tri1, proof.tri2 )

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

        newProof =
            AdjacencyProof
                { tri1 = triangle2
                , tri2 = triangle4
                , sharedEdge = LineSegment2d.fromEndpoints ( quad.one, quad.three )
                , otherVertex1 = quad.two
                , otherVertex2 = quad.zero
                }
    in
        Maybe.map2 (,) (calcTriangleAngle triangle2) (calcTriangleAngle triangle4)
            |> Maybe.map (\( t2, t4 ) -> ( t2, t4, newProof ))


{-| Calculate the internal angle between the third vertex and the other two
-}
calcTriangleAngle : Triangle2d -> Maybe Float
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
            Nothing
        else
            ( Vector2d.crossProduct v2 v1, Vector2d.dotProduct v1 v2 )
                |> (Tuple.mapSecond (clamp -1 1))
                |> (\( crossProd, dotProd ) ->
                        if crossProd < 0 then
                            Just (2 * pi - acos dotProd)
                        else
                            Just (acos dotProd)
                   )
