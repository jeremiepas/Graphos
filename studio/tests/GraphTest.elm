module GraphTest exposing (suite)

{-| graph-json-contract decoding, aggregate synthesis, traversal, identity. -}

import Dict
import Expect
import Fixtures
import Json.Decode as D
import Json.Encode as E
import Set
import Studio.Data.Graph as Graph
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Studio.Data.Graph"
        [ describe "contract decoder"
            [ test "decodes required and optional fields" <|
                \_ ->
                    case D.decodeString Graph.decoder Fixtures.contractJson of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g ->
                            Expect.all
                                [ \_ -> Graph.nodeCount g |> Expect.equal 2
                                , \_ -> Graph.edgeCount g |> Expect.equal 1
                                , \_ ->
                                    Dict.get "mod_Auth" g.nodes
                                        |> Maybe.map .kind
                                        |> Expect.equal (Just (Just "Module"))
                                , \_ ->
                                    Dict.get "doc_readme" g.nodes
                                        |> Maybe.map .lineStart
                                        |> Expect.equal (Just Nothing)
                                , \_ ->
                                    Graph.communityLabel g 483
                                        |> Expect.equal "Authentication"
                                ]
                                ()
            , test "synthesizes aggregates when community_aggregates is absent" <|
                \_ ->
                    case D.decodeString Graph.decoder Fixtures.contractJson of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g ->
                            g.aggregates
                                |> List.map (\a -> ( a.id, a.memberCount ))
                                |> List.sortBy Tuple.first
                                |> Expect.equalLists [ ( 7, 1 ), ( 483, 1 ) ]
            , test "label falls back to Community <id>" <|
                \_ ->
                    Graph.communityLabel Fixtures.chainGraph 7
                        |> Expect.equal "Community 7"
            ]
        , describe "contract encoder round-trip"
            [ test "encodeContract output re-decodes to the same nodes and edges" <|
                \_ ->
                    let
                        g =
                            Fixtures.chainGraph

                        reDecoded =
                            D.decodeString Graph.decoder (E.encode 0 (Graph.encodeContract g))
                    in
                    case reDecoded of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g2 ->
                            ( g2.nodes, g2.edges )
                                |> Expect.equal ( g.nodes, g.edges )
            ]
        , describe "traversal"
            [ test "neighborhood: depth-2 BFS over the chain" <|
                \_ ->
                    Graph.neighborhood Fixtures.chainGraph "mod_Auth" 2
                        |> Expect.equal
                            (Set.fromList [ "mod_Auth", "fn_verify", "mod_Config" ])
            , test "neighborhood of unknown node is empty" <|
                \_ ->
                    Graph.neighborhood Fixtures.chainGraph "ghost" 3
                        |> Set.isEmpty
                        |> Expect.equal True
            , test "incidentEdges finds both directions" <|
                \_ ->
                    Graph.incidentEdges Fixtures.chainGraph "fn_verify"
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "identity fingerprint"
            [ test "deterministic" <|
                \_ ->
                    Graph.fingerprint "hello graphos"
                        |> Expect.equal (Graph.fingerprint "hello graphos")
            , test "differs on different content" <|
                \_ ->
                    Graph.fingerprint "graph A"
                        |> Expect.notEqual (Graph.fingerprint "graph B")
            ]
        ]
