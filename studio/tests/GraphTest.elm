module GraphTest exposing (suite)

{-| graph-json-contract decoding, aggregate synthesis, traversal, identity. -}

import Dict
import Expect
import Fixtures
import Json.Decode as D
import Json.Encode as E
import Set
import Studio.Data.Graph as Graph
import Studio.Data.Source as Source
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
        , describe "checked-in fixture shape (studio/tests/fixtures/graph.json)"
            [ test "round-trips through the contract decoder" <|
                \_ ->
                    case D.decodeString Graph.decoder Fixtures.fixtureJson of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g ->
                            Expect.all
                                [ \_ -> Graph.nodeCount g |> Expect.equal 12
                                , \_ -> Graph.edgeCount g |> Expect.equal 11
                                , \_ -> List.length g.aggregates |> Expect.equal 4
                                , \_ -> Graph.communityLabel g 483 |> Expect.equal "Authentication"
                                , \_ ->
                                    -- community_aggregates present: no synthesis needed,
                                    -- so the decoded aggregate matches the fixture exactly.
                                    Graph.aggregateFor g 7
                                        |> Expect.equal (Just { id = 7, memberCount = 3, cohesion = 0.71, bridgeCount = 0, color = "hsl(140,62%,52%)", label = "Config", representativeLabels = [ "parseFlags", "README" ] })
                                ]
                                ()
            ]
        , describe "JGF envelope (real binary output shape)"
            [ test "decodes nodes, edges, aggregates and labels" <|
                \_ ->
                    case D.decodeString Graph.decoder jgfJson of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g ->
                            Expect.all
                                [ \_ -> Graph.nodeCount g |> Expect.equal 2
                                , \_ -> Graph.edgeCount g |> Expect.equal 1
                                , \_ ->
                                    Dict.get "fn_add" g.nodes
                                        |> Maybe.map
                                            (\n ->
                                                { lineStart = n.lineStart, signature = n.signature, kind = n.kind, communityId = n.communityId }
                                            )
                                        |> Expect.equal (Just { lineStart = Just 1, signature = Just "function add", kind = Just "Function", communityId = Just 3 })
                                , \_ ->
                                    Dict.get "e1" g.edges
                                        |> Maybe.map
                                            (\e ->
                                                ( e.relation, e.weight, e.confidence )
                                            )
                                        |> Expect.equal (Just ( "references", Just 0.5, Just 0.6 ))
                                , \_ ->
                                    -- JGF writes aggregate ids as JSON strings ("3")
                                    Graph.aggregateFor g 3
                                        |> Maybe.map .label
                                        |> Expect.equal (Just "Math")
                                , \_ -> Graph.communityLabel g 3 |> Expect.equal "Math"
                                , \_ ->
                                    Graph.communityMembers g 3
                                        |> List.map .id
                                        |> List.sort
                                        |> Expect.equal [ "doc_readme", "fn_add" ]
                                ]
                                ()
            , test "synthesizes aggregates when the graphos metadata omits them" <|
                \_ ->
                    case D.decodeString Graph.decoder jgfMinimalJson of
                        Err err ->
                            Expect.fail (D.errorToString err)

                        Ok g ->
                            Expect.all
                                [ \_ -> Graph.nodeCount g |> Expect.equal 1
                                , \_ ->
                                    g.aggregates
                                        |> List.map (\a -> ( a.id, a.memberCount ))
                                        |> Expect.equal [ ( 3, 1 ) ]
                                ]
                                ()
            ]
        , describe "file-mode size guard"
            [ test "refuses above the documented threshold" <|
                \_ ->
                    Source.exceedsSizeLimit Source.sizeThresholdBytes (Source.sizeThresholdBytes + 1)
                        |> Expect.equal True
            , test "accepts a 40 MB offline file (the offline scenario)" <|
                \_ ->
                    Source.exceedsSizeLimit Source.sizeThresholdBytes (40 * 1024 * 1024)
                        |> Expect.equal False
            ]
        ]


{-| The JGF envelope (`graphos.code-knowledge-graph`) written by the graphos
binary — nodes/edges under `graph` with field data in `metadata`, analysis
sections under `graph.metadata.graphos`, aggregate ids as JSON strings.
Mirrors `Graphos.Domain.Types.JGF`.
-}
jgfJson : String
jgfJson =
    """
{
  "graph": {
    "directed": true,
    "type": "graphos.code-knowledge-graph",
    "nodes": {
      "fn_add": {
        "id": "fn_add",
        "label": "add",
        "metadata": {
          "file_type": "code",
          "source_file": "src/Math.js",
          "line_start": 1,
          "line_end": 2,
          "signature": "function add",
          "community_id": 3,
          "kind": "Function"
        }
      },
      "doc_readme": {
        "id": "doc_readme",
        "label": "README",
        "metadata": {
          "file_type": "doc",
          "source_file": "README.md",
          "community_id": 3
        }
      }
    },
    "edges": [
      {
        "source": "fn_add",
        "relation": "references",
        "target": "doc_readme",
        "directed": true,
        "metadata": { "id": "e1", "weight": 0.5, "confidence": 0.6 }
      }
    ],
    "metadata": {
      "graphos": {
        "schemaVersion": "1.0",
        "community_aggregates": [
          {
            "id": "3",
            "member_count": 2,
            "cohesion": 0.5,
            "bridge_count": 0,
            "color": "#7dd3fc",
            "label": "Math",
            "representative_labels": [ "add" ]
          }
        ],
        "community_labels": { "3": "Math" }
      }
    }
  }
}
"""


{-| JGF envelope with no analysis sections — aggregates must be synthesized
from node community assignments, like flat exports without
`community_aggregates`.
-}
jgfMinimalJson : String
jgfMinimalJson =
    """
{
  "graph": {
    "directed": true,
    "type": "graphos.code-knowledge-graph",
    "nodes": {
      "fn_add": {
        "id": "fn_add",
        "label": "add",
        "metadata": {
          "file_type": "code",
          "source_file": "src/Math.js",
          "community_id": 3
        }
      }
    },
    "edges": [],
    "metadata": { "graphos": { "schemaVersion": "1.0" } }
  }
}
"""
