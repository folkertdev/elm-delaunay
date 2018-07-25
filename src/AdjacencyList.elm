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
        , AdjacentTriangles
        , partition
        , size
        , filter
        , member
        , unsafeUnwrap
        , findOtherEdge
        , hashTriangle
        , hashEdge
        , hasCommonEdge
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


hashTriangle : Triangle2d -> ( ( Float, Float ), ( Float, Float ), ( Float, Float ) )
hashTriangle triangle =
    let
        ( q1, q2, q3 ) =
            Triangle2d.vertices triangle

        p1 =
            Point2d.coordinates q1

        p2 =
            Point2d.coordinates q2

        p3 =
            Point2d.coordinates q3
    in
        if p1 < p2 then
            if p3 < p1 then
                ( p3, p1, p2 )
            else if p3 < p2 then
                ( p1, p3, p2 )
            else
                ( p1, p2, p3 )
        else if p3 < p2 then
            ( p3, p2, p1 )
        else if p3 < p1 then
            ( p2, p3, p1 )
        else
            ( p2, p1, p3 )


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


singleton : LineSegment2d -> ( Triangle2d, Triangle2d ) -> Result Error AdjacencyList
singleton key value =
    insert key value empty


type Error
    = DontShareEdge { tri1 : Triangle2d, tri2 : Triangle2d, edge : LineSegment2d }


unsafeUnwrap : String -> Result Error AdjacencyList -> AdjacencyList
unsafeUnwrap desc v =
    case v of
        Err e ->
            Debug.crash (desc ++ ": " ++ toString e)

        Ok v ->
            v


insert : LineSegment2d -> ( Triangle2d, Triangle2d ) -> AdjacencyList -> Result Error AdjacencyList
insert edge ( t1, t2 ) alist =
    case Maybe.map (\p -> insertProof p alist) (proofAdjacencyFromEdge edge t1 t2) of
        Just done ->
            Ok done

        Nothing ->
            Err <|
                DontShareEdge
                    { tri1 = t1
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


partition : (LineSegment2d -> AdjacentTriangles -> Bool) -> AdjacencyList -> ( AdjacencyList, AdjacencyList )
partition predicate (AdjacencyList dict) =
    let
        ( d1, d2 ) =
            AllDict.partition predicate dict
    in
        ( AdjacencyList d1, AdjacencyList d2 )


size : AdjacencyList -> Int
size (AdjacencyList dict) =
    AllDict.size dict


member : LineSegment2d -> AdjacencyList -> Bool
member edge (AdjacencyList dict) =
    AllDict.member edge dict


filter : (LineSegment2d -> AdjacentTriangles -> Bool) -> AdjacencyList -> AdjacencyList
filter predicate (AdjacencyList dict) =
    AdjacencyList (AllDict.filter predicate dict)


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


fromList : List ( LineSegment2d, ( Triangle2d, Triangle2d ) ) -> Result Error AdjacencyList
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


map : (LineSegment2d -> AdjacentTriangles -> ( Triangle2d, Triangle2d )) -> AdjacencyList -> Result Error AdjacencyList
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
                accum
                    |> AllDict.update p1 (updater p1 triangle)
                    |> AllDict.update p2 (updater p2 triangle)
                    |> AllDict.update p3 (updater p3 triangle)

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
            |> List.foldl (\triangle -> AllDict.insert triangle ()) (AllDict.empty hashTriangle)
            |> AllDict.keys
            |> List.foldl folder (AllDict.empty hashEdge)


fromTriangles : List Triangle2d -> Result Error AdjacencyList
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


findOtherEdgeOld : Point2d -> Point2d -> Set Edge -> Maybe Point2d
findOtherEdgeOld point1 point2 edges =
    edges
        |> Set.toList
        |> List.concatMap (\( a, b ) -> [ a, b ])
        |> List.filter (\p -> p /= Point2d.coordinates point1 && p /= Point2d.coordinates point2)
        |> List.head
        |> Maybe.map Point2d.fromCoordinates


findOtherEdge : Point2d -> Point2d -> Set Edge -> Maybe Point2d
findOtherEdge point1_ point2_ edges =
    let
        point1 =
            Point2d.coordinates point1_

        point2 =
            Point2d.coordinates point2_

        folder : Edge -> Maybe ( Float, Float ) -> Maybe ( Float, Float )
        folder ( p1, p2 ) accum =
            case accum of
                Just v ->
                    Just v

                Nothing ->
                    if p1 /= point1 && p1 /= point2 then
                        Just p1
                    else if p2 /= point1 && p2 /= point2 then
                        Just p2
                    else
                        Nothing
    in
        edges
            |> Set.foldl folder Nothing
            |> Maybe.map Point2d.fromCoordinates


proofAdjacencyCommon : Triangle2d -> Triangle2d -> Set Edge -> Set Edge -> Maybe AdjacencyProof
proofAdjacencyCommon tri1 tri2 edgesT1 edgesT2 =
    case hasCommonEdge tri1 tri2 of
        Just edge ->
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints edge
            in
                case Maybe.map2 (,) (findOtherEdge p1 p2 edgesT1) (findOtherEdge p1 p2 edgesT2) of
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
        proofAdjacencyCommon tri1 tri2 edgesT1 edgesT2


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
            proofAdjacencyCommon tri1 tri2 edgesT1 edgesT2
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
                Set.fromList [ hashEdge e1, hashEdge e2, hashEdge e3 ]

        folder edge accum =
            case accum of
                Just x ->
                    Just x

                Nothing ->
                    if Set.member (hashEdge edge) edges2 then
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
