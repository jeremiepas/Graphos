module Fixtures exposing (chainGraph, contractJson, fixtureJson, mkEdge, mkNode)

{-| Shared test fixtures: a 4-node chain across two communities
(mod_Auth—fn_verify in 483, mod_Config—doc_readme in 7) plus an isolated
node, mirroring the Lean model's fixture.
-}

import Dict
import Studio.Data.Graph as Graph exposing (Edge, Graph, Node)


mkNode : String -> String -> Int -> Node
mkNode id label cid =
    { id = id
    , label = label
    , fileType = Graph.CodeFile
    , sourceFile = "src/" ++ id ++ ".hs"
    , lineStart = Just 1
    , lineEnd = Nothing
    , signature = Nothing
    , communityId = Just cid
    , kind = Just "Function"
    , degree = Just 1
    , isBridge = Nothing
    , extra = Nothing
    }


mkEdge : String -> String -> String -> Edge
mkEdge source target relation =
    { id = source ++ "->" ++ target
    , source = source
    , target = target
    , relation = relation
    , weight = Nothing
    , confidence = Just 0.9
    , extra = Nothing
    }


chainGraph : Graph
chainGraph =
    let
        nodes =
            [ mkNode "mod_Auth" "Auth" 483
            , mkNode "fn_verify" "verifyToken" 483
            , mkNode "mod_Config" "Config" 7
            , mkNode "doc_readme" "README" 7
            , mkNode "zz_isolated" "Isolated" 7
            ]

        edges =
            [ mkEdge "mod_Auth" "fn_verify" "calls"
            , mkEdge "fn_verify" "mod_Config" "imports"
            , mkEdge "mod_Config" "doc_readme" "references"
            ]

        base =
            { nodes = Dict.fromList (List.map (\n -> ( n.id, n )) nodes)
            , edges = Dict.fromList (List.map (\e -> ( e.id, e )) edges)
            , aggregates = []
            , communityLabels = Dict.fromList [ ( 483, "Authentication" ) ]
            }
    in
    { base | aggregates = Graph.synthesizeAggregates base }


{-| A graph-json-contract document exercising required + optional fields. -}
contractJson : String
contractJson =
    """
{ "nodes":
  [ { "id": "mod_Auth", "label": "Auth", "file_type": "code",
      "source_file": "src/Auth.hs", "line_start": 10, "line_end": 90,
      "signature": null, "community_id": 483, "kind": "Module",
      "degree": 2, "is_bridge": true, "extra": null }
  , { "id": "doc_readme", "label": "README", "file_type": "doc",
      "source_file": "README.md", "community_id": 7 }
  ]
, "edges":
  [ { "id": "e1", "source": "mod_Auth", "target": "doc_readme",
      "relation": "references", "weight": 1.0, "confidence": 0.8 }
  ]
, "communities": {}
, "cohesion": {}
, "god_nodes": []
, "community_labels": { "483": "Authentication" }
}
"""

{-| The checked-in contract fixture (`studio/tests/fixtures/graph.json`) as a
constant. Elm tests cannot read files, so this embedded copy is the source of
truth for decoder round-trips; the CI drift step guards that the on-disk file
keeps parity with it (see .github/workflows/studio.yml). 12 nodes across four
communities, with `community_aggregates` present so synthesis is not exercised.
-}
fixtureJson : String
fixtureJson =
    """
{
  "cohesion": {},
  "communities": {},
  "community_aggregates": [
    {
      "bridge_count": 1,
      "cohesion": 0.82,
      "color": "hsl(210,62%,52%)",
      "id": 483,
      "label": "Authentication",
      "member_count": 3,
      "representative_labels": [
        "verifyToken",
        "loginHandler"
      ]
    },
    {
      "bridge_count": 0,
      "cohesion": 0.71,
      "color": "hsl(140,62%,52%)",
      "id": 7,
      "label": "Config",
      "member_count": 3,
      "representative_labels": [
        "parseFlags",
        "README"
      ]
    },
    {
      "bridge_count": 1,
      "cohesion": 0.68,
      "color": "hsl(40,62%,52%)",
      "id": 12,
      "label": "Logging",
      "member_count": 3,
      "representative_labels": [
        "main",
        "formatLog"
      ]
    },
    {
      "bridge_count": 0,
      "cohesion": 0.9,
      "color": "hsl(320,62%,52%)",
      "id": 5,
      "label": "Web",
      "member_count": 2,
      "representative_labels": [
        "route",
        "viewPage"
      ]
    }
  ],
  "community_labels": {
    "12": "Logging",
    "483": "Authentication",
    "5": "Web",
    "7": "Config"
  },
  "edges": [
    {
      "confidence": 0.95,
      "id": "e1",
      "relation": "defines",
      "source": "mod_Auth",
      "target": "fn_verify",
      "weight": 1.0
    },
    {
      "confidence": 0.9,
      "id": "e2",
      "relation": "defines",
      "source": "mod_Auth",
      "target": "fn_login",
      "weight": 1.0
    },
    {
      "confidence": 0.7,
      "id": "e3",
      "relation": "calls",
      "source": "fn_login",
      "target": "fn_verify",
      "weight": 0.8
    },
    {
      "confidence": 0.95,
      "id": "e4",
      "relation": "defines",
      "source": "mod_Config",
      "target": "fn_parse",
      "weight": 1.0
    },
    {
      "confidence": 0.6,
      "id": "e5",
      "relation": "references",
      "source": "mod_Config",
      "target": "doc_readme",
      "weight": 0.5
    },
    {
      "confidence": 0.8,
      "id": "e6",
      "relation": "calls",
      "source": "log_main",
      "target": "fn_format",
      "weight": 0.7
    },
    {
      "confidence": 0.7,
      "id": "e7",
      "relation": "calls",
      "source": "log_main",
      "target": "mod_Auth",
      "weight": 0.6
    },
    {
      "confidence": 0.7,
      "id": "e8",
      "relation": "calls",
      "source": "log_main",
      "target": "mod_Config",
      "weight": 0.6
    },
    {
      "confidence": 0.7,
      "id": "e9",
      "relation": "calls",
      "source": "log_main",
      "target": "fn_route",
      "weight": 0.6
    },
    {
      "confidence": 0.85,
      "id": "e10",
      "relation": "calls",
      "source": "fn_route",
      "target": "fn_view",
      "weight": 0.9
    },
    {
      "confidence": 0.5,
      "id": "e11",
      "relation": "references",
      "source": "fn_format",
      "target": "doc_changelog",
      "weight": 0.4
    }
  ],
  "god_nodes": [],
  "nodes": [
    {
      "community_id": 483,
      "degree": 3,
      "extra": null,
      "file_type": "code",
      "id": "mod_Auth",
      "is_bridge": true,
      "kind": "Module",
      "label": "Auth",
      "line_end": 90,
      "line_start": 10,
      "signature": "module Auth where",
      "source_file": "src/Auth.hs"
    },
    {
      "community_id": 483,
      "degree": 1,
      "extra": null,
      "file_type": "code",
      "id": "fn_verify",
      "is_bridge": false,
      "kind": "Function",
      "label": "verifyToken",
      "line_end": 60,
      "line_start": 40,
      "signature": "verifyToken :: Text -> IO Bool",
      "source_file": "src/Auth.hs"
    },
    {
      "community_id": 483,
      "degree": 2,
      "extra": null,
      "file_type": "code",
      "id": "fn_login",
      "is_bridge": false,
      "kind": "Function",
      "label": "loginHandler",
      "line_end": 88,
      "line_start": 62,
      "signature": null,
      "source_file": "src/Auth.hs"
    },
    {
      "community_id": 7,
      "degree": 2,
      "extra": null,
      "file_type": "code",
      "id": "mod_Config",
      "is_bridge": false,
      "kind": "Module",
      "label": "Config",
      "line_end": 40,
      "line_start": 1,
      "signature": "module Config where",
      "source_file": "src/Config.hs"
    },
    {
      "community_id": 7,
      "degree": 1,
      "extra": null,
      "file_type": "code",
      "id": "fn_parse",
      "is_bridge": false,
      "kind": "Function",
      "label": "parseFlags",
      "line_end": 38,
      "line_start": 12,
      "signature": null,
      "source_file": "src/Config.hs"
    },
    {
      "community_id": 7,
      "degree": 1,
      "extra": null,
      "file_type": "doc",
      "id": "doc_readme",
      "is_bridge": false,
      "kind": "Documentation",
      "label": "README",
      "source_file": "README.md"
    },
    {
      "community_id": 12,
      "degree": 4,
      "extra": null,
      "file_type": "code",
      "id": "log_main",
      "is_bridge": true,
      "kind": "Function",
      "label": "main",
      "line_end": 30,
      "line_start": 5,
      "signature": "module Main where",
      "source_file": "app/Main.hs"
    },
    {
      "community_id": 12,
      "degree": 1,
      "extra": null,
      "file_type": "code",
      "id": "fn_format",
      "is_bridge": false,
      "kind": "Function",
      "label": "formatLog",
      "line_end": 45,
      "line_start": 20,
      "signature": null,
      "source_file": "src/Log.hs"
    },
    {
      "community_id": 12,
      "degree": 0,
      "extra": null,
      "file_type": "doc",
      "id": "doc_changelog",
      "is_bridge": false,
      "kind": "Documentation",
      "label": "CHANGELOG",
      "source_file": "CHANGELOG.md"
    },
    {
      "community_id": 5,
      "degree": 2,
      "extra": null,
      "file_type": "code",
      "id": "fn_route",
      "is_bridge": false,
      "kind": "Function",
      "label": "route",
      "line_end": 50,
      "line_start": 8,
      "signature": null,
      "source_file": "src/Web.hs"
    },
    {
      "community_id": 5,
      "degree": 1,
      "extra": null,
      "file_type": "code",
      "id": "fn_view",
      "is_bridge": false,
      "kind": "Function",
      "label": "viewPage",
      "line_end": 95,
      "line_start": 52,
      "signature": null,
      "source_file": "src/Web.hs"
    },
    {
      "community_id": null,
      "degree": 0,
      "extra": null,
      "file_type": "code",
      "id": "zz_orphan",
      "is_bridge": false,
      "kind": "Function",
      "label": "LegacyUtil",
      "line_end": 5,
      "line_start": 1,
      "signature": null,
      "source_file": "src/Legacy.hs"
    }
  ]
}
"""
