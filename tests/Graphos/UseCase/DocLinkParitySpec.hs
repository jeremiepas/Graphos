{-# LANGUAGE OverloadedStrings #-}
-- | Checker parity with the Lean model (openspec/changes/deterministic-doc-code-edges/lean/DocLink.lean).
--
-- The same discipline as Graphos.Domain.SpecCheckSpec: the Domain linking
-- passes run on the Lean model's toy fixture @G@ and the verdicts must be
-- identical to the kernel-checked @rfl@ examples in @DocLink.lean@ section 5:
--
-- > example : (colocEdges G).map (·.dst) = [200, 201]
-- > example : (symbolEdges G).map (·.dst) = [200, 201]
-- > example : (pathRefEdges G).map (·.dst) = [202]
-- > example : (pathRefEdges G).map (·.src) = [101]
-- > example : (docCodeEdges G).all (fun e => e.rel = .documents) = true
-- > example : (docCodeEdges G).all (fun e => survivesSemantic e.conf) = true
-- > example : (docCodeEdges G).all (fun e => e.src ≠ 102) = true
module Graphos.UseCase.DocLinkParitySpec where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.UseCase.Infer.Document

-- ── The Lean toy fixture G (DocLink.lean, section 5) ─────────────────────────

-- Lean: 100 doc "libraries/jwt-verifier/README.md"
--         text "The verifier exposes validateToken and parseConfig. See ./src/lib.rs for details."
--       101 doc "docs/adr/0007-task-model.md"
--         text "The task model lives in src/domain/workflow/task-definition.ts. index.ts is too ambiguous."
--       102 doc "other/NOTES.md"
--         text "Nothing relevant. Common words like config and handler everywhere."
--       200 code "libraries/jwt-verifier/src/lib.rs" defines validateToken
--       201 code "libraries/jwt-verifier/app/Main.hs" defines parseConfig
--       202 code "src/domain/workflow/task-definition.ts" defines createTask

leanNode :: Text -> FileType -> Text -> Node
leanNode nid ft sf = Node
  { nodeId        = nid
  , nodeLabel     = fromText nid
  , nodeFileType  = ft
  , nodeSourceFile = fromText sf
  , nodeLineStart = Just 1
  , nodeLineEnd   = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind      = Nothing
  , nodeDegree    = Nothing
  , nodeIsBridge  = Nothing
  , nodeExtra     = Nothing
  , nodePresentBits = 0
  }

docText :: Map NodeId Text
docText = Map.fromList
  [ ("100", "The verifier exposes validateToken and parseConfig. See ./src/lib.rs for details.")
  , ("101", "The task model lives in src/domain/workflow/task-definition.ts. index.ts is too ambiguous.")
  , ("102", "Nothing relevant. Common words like config and handler everywhere.")
  ]

-- The Lean graph nodes; the graph itself (plus doc text) is passed per pass.
leanNodes :: [Node]
leanNodes =
  [ leanNode "100" DocFile "libraries/jwt-verifier/README.md"
  , leanNode "101" DocFile "docs/adr/0007-task-model.md"
  , leanNode "102" DocFile "other/NOTES.md"
  , leanNode "200" CodeFile "libraries/jwt-verifier/src/lib.rs"
  , leanNode "201" CodeFile "libraries/jwt-verifier/app/Main.hs"
  , leanNode "202" CodeFile "src/domain/workflow/task-definition.ts"
  ]

leanGraph :: Graph
leanGraph = buildGraph False (extractionFromLists leanNodes [])

-- In the Lean model, "defines" is part of the node; in Graphos the
-- symbol/definition index keys on the code node's label, so the fixtures
-- label their defining nodes with the Lean `defines` identifiers.
defLabelledGraph :: Graph
defLabelledGraph = buildGraph False $ extractionFromLists
  [ leanNode "100" DocFile "libraries/jwt-verifier/README.md"
  , leanNode "101" DocFile "docs/adr/0007-task-model.md"
  , leanNode "102" DocFile "other/NOTES.md"
  , leanNode "200" CodeFile "libraries/jwt-verifier/src/lib.rs" `withLabel` "validateToken"
  , leanNode "201" CodeFile "libraries/jwt-verifier/app/Main.hs" `withLabel` "parseConfig"
  , leanNode "202" CodeFile "src/domain/workflow/task-definition.ts" `withLabel` "createTask"
  ] []
  where
    withLabel n lbl = n { nodeLabel = fromText lbl }

-- ── Parity checks ────────────────────────────────────────────────────────────

endpoints :: [Edge] -> [(NodeId, NodeId)]
endpoints es = sortOn id [ (edgeSource e, edgeTarget e) | e <- es ]

spec :: Spec
spec = do
  describe "checker parity with Lean DocLink.lean toy fixture" $ do
    it "colocEdges: sibling README links to its subtree code [200, 201]" $ do
      -- Lean: (colocEdges G).map (·.dst) = [200, 201]
      endpoints (inferCoLocationEdges leanGraph)
        `shouldBe` [("100", "200"), ("100", "201")]

    it "colocEdges: the unrelated doc (102) is isolated; 202 is not co-located" $ do
      let es = endpoints (inferCoLocationEdges leanGraph)
      all (\(s, _) -> s /= "102") es `shouldBe` True
      lookup "102" [(edgeSource e, edgeTarget e) | e <- inferCoLocationEdges leanGraph]
        `shouldBe` Nothing

    it "symbolEdges: unique mentions validateToken/parseConfig link [200, 201]" $ do
      -- Lean: (symbolEdges G).map (·.dst) = [200, 201]
      endpoints (inferSymbolMentionEdges docText defLabelledGraph)
        `shouldBe` [("100", "200"), ("100", "201")]

    it "symbolEdges: common words config/handler (never uniquely defined) skipped" $ do
      endpoints (inferSymbolMentionEdges docText leanGraph)
        `shouldBe` []

    it "pathRefEdges: ADR cites cross-subtree path → 101→202 only" $ do
      -- Lean: (pathRefEdges G).map (·.dst) = [202], (·.src) = [101]
      endpoints (inferPathReferenceEdges docText leanGraph)
        `shouldBe` [("101", "202")]

    it "pathEdges: dangling ./src/lib.rs (no such node) and bare index.ts skipped" $ do
      let es = inferPathReferenceEdges docText leanGraph
      all (\e -> edgeSource e /= "100") es `shouldBe` True

    it "docCodeEdges: every edge carries the Documents relation" $ do
      -- Lean: (docCodeEdges G).all (fun e => e.rel = .documents) = true
      let es = inferCoLocationEdges leanGraph
            ++ inferSymbolMentionEdges docText defLabelledGraph
            ++ inferPathReferenceEdges docText leanGraph
      all (\e -> edgeRelation e == Documents) es `shouldBe` True

    it "docCodeEdges: every edge survives the semantic filter (confidence ≥ 0.7)" $ do
      -- Lean: (docCodeEdges G).all (fun e => survivesSemantic e.conf) = true
      let es = inferCoLocationEdges leanGraph
            ++ inferSymbolMentionEdges docText defLabelledGraph
            ++ inferPathReferenceEdges docText leanGraph
      all (\e -> case edgeConfidence e of Confidence c -> c >= 0.7) es `shouldBe` True

    it "docCodeEdges: the unrelated doc (102) produces no edge in any pass" $ do
      -- Lean: (docCodeEdges G).all (fun e => e.src ≠ 102) = true
      let es = inferCoLocationEdges leanGraph
            ++ inferSymbolMentionEdges docText defLabelledGraph
            ++ inferPathReferenceEdges docText leanGraph
      all (\e -> edgeSource e /= "102") es `shouldBe` True