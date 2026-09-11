module Studio.Scope exposing
    ( Scope
    , addCommunity
    , addGroup
    , addNeighborhood
    , addSelection
    , counts
    , empty
    , encodeCanonical
    , isEmpty
    , scopeEdges
    )

{-| Subgraph extraction (`studio-subgraph-extract`): a scope is a set algebra
over node ids — communities, group members, neighborhoods and manual
selections union into one set — exported in the `json-graph-web-view`
canonical vis-network shape so the existing views catalog consumes it
unchanged.
-}

import Dict exposing (Dict)
import Json.Encode as E
import Set exposing (Set)
import Studio.Data.Graph as Graph exposing (Edge, Graph)
import Studio.Groups as Groups exposing (Group)



-- SCOPE ALGEBRA


type alias Scope =
    Set String


empty : Scope
empty =
    Set.empty


isEmpty : Scope -> Bool
isEmpty =
    Set.isEmpty


addCommunity : Graph -> Int -> Scope -> Scope
addCommunity g cid scope =
    Graph.communityMembers g cid
        |> List.foldl (\n acc -> Set.insert n.id acc) scope


addGroup : Graph -> Group -> Scope -> Scope
addGroup g group scope =
    Dict.foldl
        (\nid node acc ->
            if Groups.matchLocal group.query node then
                Set.insert nid acc

            else
                acc
        )
        scope
        g.nodes


addNeighborhood : Graph -> String -> Int -> Scope -> Scope
addNeighborhood g start depth scope =
    Set.union scope (Graph.neighborhood g start depth)


addSelection : List String -> Scope -> Scope
addSelection ids scope =
    List.foldl Set.insert scope ids



-- EDGES: both endpoints in scope; boundary adds one-hop edges + their nodes


{-| Edges of the scoped subgraph. With `boundary = True`, edges with exactly
one endpoint in scope are included too (their outside endpoints become part
of the export).
-}
scopeEdges : Graph -> Bool -> Scope -> { edges : List Edge, boundaryNodes : Set String }
scopeEdges g boundary scope =
    Dict.foldl
        (\_ e acc ->
            let
                inS =
                    Set.member e.source scope

                inT =
                    Set.member e.target scope
            in
            if inS && inT then
                { acc | edges = e :: acc.edges }

            else if boundary && (inS || inT) then
                { edges = e :: acc.edges
                , boundaryNodes =
                    Set.insert
                        (if inS then
                            e.target

                         else
                            e.source
                        )
                        acc.boundaryNodes
                }

            else
                acc
        )
        { edges = [], boundaryNodes = Set.empty }
        g.edges


{-| Live counts for the scope panel. -}
counts : Graph -> Bool -> Scope -> { nodes : Int, edges : Int }
counts g boundary scope =
    let
        { edges, boundaryNodes } =
            scopeEdges g boundary scope
    in
    { nodes = Set.size scope + Set.size boundaryNodes
    , edges = List.length edges
    }



-- CANONICAL EXPORT (json-graph-web-view vis-network shape)


{-| Encode the scope as the canonical `{title, nodes, edges}` view document:
vis-network shape, string ids, labels and community metadata preserved,
`relation` on edges. When `bakeGroups` is set, first-match-wins group colors
replace community colors on matching nodes.
-}
encodeCanonical :
    { title : String, boundary : Bool, bakeGroups : Bool }
    -> Graph
    -> List Group
    -> Scope
    -> E.Value
encodeCanonical opts g groups scope =
    let
        { edges, boundaryNodes } =
            scopeEdges g opts.boundary scope

        exportIds =
            Set.union scope boundaryNodes

        groupColors =
            if opts.bakeGroups then
                Groups.memberships groups g

            else
                Dict.empty

        colorOf nid node =
            case Dict.get nid groupColors |> Maybe.andThen (\i -> List.head (List.drop i groups)) of
                Just grp ->
                    grp.color

                Nothing ->
                    node.communityId
                        |> Maybe.andThen (\cid -> Graph.aggregateFor g cid)
                        |> Maybe.map .color
                        |> Maybe.withDefault "hsl(0,0%,53%)"

        encodeNode nid =
            Dict.get nid g.nodes
                |> Maybe.map
                    (\node ->
                        E.object
                            ([ ( "id", E.string node.id )
                             , ( "label", E.string node.label )
                             , ( "color", E.string (colorOf nid node) )
                             ]
                                ++ (case node.communityId of
                                        Just cid ->
                                            [ ( "community_id", E.int cid ) ]

                                        Nothing ->
                                            []
                                   )
                            )
                    )

        encodeEdge e =
            E.object
                [ ( "from", E.string e.source )
                , ( "to", E.string e.target )
                , ( "relation", E.string e.relation )
                ]
    in
    E.object
        [ ( "title", E.string opts.title )
        , ( "nodes", E.list identity (Set.toList exportIds |> List.filterMap encodeNode) )
        , ( "edges", E.list encodeEdge edges )
        ]
