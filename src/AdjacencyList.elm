module AdjacencyList
    exposing
        ( AdjacencyList
        , AdjacencyProof
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
        , proofAdjacencyFromEdge
        , proofAdjacency
        )

import AllDict exposing (AllDict)
import LineSegment2d exposing (LineSegment2d)
import Triangle2d exposing (Triangle2d)
import Point2d exposing (Point2d)
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
insert edge ( t1, t2 ) (AdjacencyList dict) =
    case proofAdjacencyFromEdge edge t1 t2 of
        Just (AdjacencyProof proof) ->
            AllDict.insert edge { tri1 = t1, tri2 = t2, otherVertex1 = proof.otherVertex1, otherVertex2 = proof.otherVertex2 } dict
                |> AdjacencyList
                |> Ok

        Nothing ->
            Err <|
                toString
                    { error = "invalid insertion: triangles do not share edge"
                    , tri1 = t1
                    , tri2 = t2
                    , edge = edge
                    }


remove : LineSegment2d -> AdjacencyList -> AdjacencyList
remove edge (AdjacencyList dict) =
    AdjacencyList (AllDict.remove edge dict)


get : LineSegment2d -> AdjacencyList -> Maybe AdjacentTriangles
get edge (AdjacencyList dict) =
    AllDict.get edge dict


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
            (Set.intersect edgesT1 edgesT2)
    in
        case Set.toList combinedEdgeSet of
            [] ->
                Nothing

            [ x ] ->
                proofAdjacencyFromEdgeUnsafe x edgesT1 edgesT2 tri1 tri2
                    |> Just

            _ ->
                Nothing


proofAdjacencyFromEdgeUnsafe : Edge -> Set Edge -> Set Edge -> Triangle2d -> Triangle2d -> AdjacencyProof
proofAdjacencyFromEdgeUnsafe edge edgesT1 edgesT2 tri1 tri2 =
    let
        mapBoth f ( a, b ) =
            ( f a, f b )

        ( p1, p2 ) =
            edge

        findOtherEdge edges =
            edges
                |> Set.toList
                |> List.concatMap (\( a, b ) -> [ a, b ])
                |> List.filter (\p -> p /= p1 && p /= p2)
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
                |> Point2d.fromCoordinates
    in
        AdjacencyProof
            { tri1 = tri1
            , tri2 = tri2
            , sharedEdge =
                edge
                    |> mapBoth Point2d.fromCoordinates
                    |> LineSegment2d.fromEndpoints
            , otherVertex1 = findOtherEdge edgesT1
            , otherVertex2 = findOtherEdge edgesT2
            }


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
            proofAdjacencyFromEdgeUnsafe (hashEdge edge) edgesT1 edgesT2 tri1 tri2
                |> Just
        else
            Nothing
