-- | Mirrors the Lean model's Scenarios.lean
-- (openspec/changes/spec-graph-verification/lean/): same fixture graphs, and
-- the verdicts must be identical — the Domain checker and the Lean model are
-- kept in lockstep by this suite.
module Graphos.Domain.SpecCheckSpec (spec) where

import Data.Aeson (decode, encode, object, (.=))
import Data.Maybe (isJust)
import Test.Hspec

import Graphos.Domain.SpecCheck
import Graphos.Domain.Types
import Graphos.Infrastructure.SpecParse (parseSpecFile)
import Graphos.UseCase.SpecCheck

import qualified Data.Text as T
import Data.Text.Short (fromText)

-- ── Fixture builders (mirror Scenarios.lean req/dec/code/tgt) ────────────────

mkN :: T.Text -> T.Text -> FileType -> Bool -> Node
mkN nid kind ft active =
  let n = Node
        { nodeId = nid
        , nodeLabel = fromText nid
        , nodeFileType = ft
        , nodeSourceFile = fromText "fixture.md"
        , nodeLineStart = Just 1
        , nodeLineEnd = Nothing
        , nodeSignature = Nothing
        , nodeCommunityId = Nothing
        , nodeKind = Just (fromText kind)
        , nodeDegree = Nothing
        , nodeIsBridge = Nothing
        , nodeExtra =
            if active then Nothing else Just (object [ "active" .= False ])
        , nodePresentBits = 0
        }
  in n { nodePresentBits = computePresentBits n }

reqN :: T.Text -> Node
reqN nid = mkN nid "Requirement" DocFile True

decN :: T.Text -> Bool -> Node
decN nid = mkN nid "Decision" DocFile

codeN :: T.Text -> Node
codeN nid = mkN nid "Function" CodeFile True

tgtN :: T.Text -> Node
tgtN nid = mkN nid "Artifact" DocFile True

mkE :: T.Text -> Relation -> T.Text -> Edge
mkE s r t = Edge
  { edgeId = EdgeId (s <> "|" <> t)
  , edgeSource = s
  , edgeTarget = t
  , edgeRelation = r
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

-- The main fixture: the historical graph.html conflict shape (Lean gMain).
mainNodes :: [Node]
mainNodes =
  [ reqN "R-selfcontained", reqN "R-remote", reqN "R-lod", reqN "R-ghost"
  , decN "ADR-embed" False, decN "ADR-lsp" True
  , codeN "fn_render", tgtN "graph.html"
  ]

mainEdges :: [Edge]
mainEdges =
  [ mkE "R-selfcontained" Constrains "graph.html"
  , mkE "R-remote" Constrains "graph.html"
  , mkE "R-lod" Satisfies "fn_render"
  , mkE "R-remote" References "ADR-embed"
  , mkE "R-lod" DependsOn "R-selfcontained"
  ]

mainSpecIds :: [NodeId]
mainSpecIds = [ nodeId n | n <- mainNodes, isRequirementNode n || isDecisionNode n ]

-- Cycle fixture (Lean gCycle): A depends B, B refines C, C depends A.
cycleNodes :: [Node]
cycleNodes = [ reqN "A", reqN "B", reqN "C" ]

cycleEdges :: [Edge]
cycleEdges =
  [ mkE "A" DependsOn "B", mkE "B" Refines "C", mkE "C" DependsOn "A" ]

spec :: Spec
spec = do
  describe "relation vocabulary (graph-json-contract delta)" $ do
    it "round-trips the five spec relations through JSON" $ do
      let rels = [ Refines, ConflictsWith, Satisfies, Supersedes, Constrains ]
      decode (encode rels) `shouldBe` Just rels
    it "encodes with the lowercase text convention" $
      map relationToText [ Refines, ConflictsWith, Satisfies, Supersedes, Constrains ]
        `shouldBe` [ "refines", "conflicts_with", "satisfies", "supersedes", "constrains" ]

  describe "certificate-carrying cycle check (mirrors Lean Topo)" $ do
    it "the closed walk A->B->C->A checks as a witness" $
      isClosedChain (stepRel cycleEdges specRels) [ "A", "B", "C", "A" ]
        `shouldBe` True
    it "the cycle fixture yields no certificate" $
      checkAcyclic (map nodeId cycleNodes) cycleEdges specRels `shouldBe` Nothing
    it "the cyclic verdict carries a re-checked witness" $ do
      let w = findCycleWitness (map nodeId cycleNodes) cycleEdges specRels
      fmap (isClosedChain (stepRel cycleEdges specRels)) w `shouldBe` Just True
    it "the DAG fixture yields a certificate the re-checker accepts" $ do
      let cert = checkAcyclic mainSpecIds mainEdges specRels
      cert `shouldSatisfy` isJust
      fmap (isTopoOrder mainSpecIds mainEdges specRels) cert `shouldBe` Just True

  describe "coverage with path certificates (mirrors Lean Paths)" $ do
    it "R-lod is implemented with a certificate that re-checks" $ do
      let w = implementedWitness mainNodes mainEdges "R-lod"
          isCode nid = any (\n -> nodeId n == nid && isCodeNode n) mainNodes
      w `shouldSatisfy` isJust
      fmap (isPathTo (stepRel mainEdges coverageRels) "R-lod" isCode) w
        `shouldBe` Just True
    it "R-ghost is unimplemented; R-lod is not" $ do
      let un = unimplementedRequirements mainNodes mainEdges
      un `shouldSatisfy` elem "R-ghost"
      un `shouldSatisfy` notElem "R-lod"

  describe "contradiction candidates (mirrors Lean Candidates)" $ do
    it "the historical shared-target pair is surfaced" $
      areCandidates mainNodes mainEdges "R-selfcontained" "R-remote"
        `shouldBe` True
    it "enumeration contains the ordered pair" $
      candidatePairs mainNodes mainEdges
        `shouldBe` [ ("R-remote", "R-selfcontained") ]
    it "requirements without a shared constrained target are not candidates" $
      areCandidates mainNodes mainEdges "R-lod" "R-remote" `shouldBe` False
    it "stale supersession: active R-remote references superseded ADR-embed" $
      staleSupersessions mainNodes mainEdges
        `shouldBe` [ ("R-remote", "ADR-embed") ]

  describe "adjudication gate (mirrors Lean gate theorems)" $ do
    let candidates = candidatePairs mainNodes mainEdges
        confirmFirst p =
          if p == ("R-remote", "R-selfcontained")
            then VerdictConflict
            else VerdictCompatible
    it "confirmed conflict blocks with trace (exactly the confirmed pair)" $
      gate confirmFirst candidates `shouldBe` [ ("R-remote", "R-selfcontained") ]
    it "dismissed candidates do not gate" $
      gate (const VerdictCompatible) candidates `shouldBe` []
    it "unadjudicated never blocks" $
      gate (const VerdictUnadjudicated) candidates `shouldBe` []

  describe "report gating discipline" $ do
    it "cycles gate; candidates alone never gate" $ do
      let cyclic = runSpecCheck cycleNodes cycleEdges False
          okish = runSpecCheck mainNodes mainEdges False
      reportGates cyclic `shouldBe` True
      -- main fixture has candidates + unimplemented, but no cycle → PASS
      reportGates okish `shouldBe` False
    it "strict coverage gates on unimplemented" $
      reportGates (runSpecCheck mainNodes mainEdges True) `shouldBe` True

  describe "structural parser (spec-artifact-schema, no-model layer)" $ do
    let doc = T.unlines
          [ "# demo Specification"
          , ""
          , "## Requirements"
          , ""
          , "### Requirement: First rule"
          , ""
          , "The system SHALL do the thing."
          , ""
          , "#### Scenario: It does the thing"
          , ""
          , "- **WHEN** x"
          , "- **THEN** y"
          , ""
          , "### Requirement: Second rule"
          , ""
          , "#### Scenario: A"
          , "#### Scenario: B"
          ]
        (cap, ns, es) = parseSpecFile "openspec/specs/demo/spec.md" doc
    it "derives the capability from the directory" $
      cap `shouldBe` "demo"
    it "extracts requirements and scenarios with spans" $ do
      length [ n | n <- ns, isRequirementNode n ] `shouldBe` 2
      length [ n | n <- ns, fmap (== fromText "Scenario") (nodeKind n) == Just True ]
        `shouldBe` 3
      [ nodeLineStart n | n <- ns, nodeId n == "req:demo/first-rule" ]
        `shouldBe` [ Just 5 ]
    it "links scenarios to their requirement via contains" $
      length [ e | e <- es
             , edgeRelation e == Contains
             , edgeSource e == "req:demo/first-rule"
             ] `shouldBe` 1
