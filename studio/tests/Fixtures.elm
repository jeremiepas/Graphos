module Fixtures exposing (chainGraph, contractJson, mkEdge, mkNode)

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
