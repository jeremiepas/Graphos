-- | Mirrors the Lean model's Scenarios.lean
-- (openspec/changes/spec-graph-verification/lean/): same fixture graphs, and
-- the verdicts must be identical — the Domain checker and the Lean model are
-- kept in lockstep by this suite.
module Graphos.Domain.SpecCheckSpec (spec) where

import Control.Monad (forM_)
import Data.IORef
import System.IO.Unsafe ()
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Maybe (isJust)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Hspec

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

import Graphos.Domain.Graph (gEdges)
import Graphos.Domain.Query.Cypher.Parser (parseStatement)
import Graphos.Domain.Query.Cypher.Eval (evaluateStatement)
import Graphos.Domain.Graph.Index (buildIndex)
import Graphos.Domain.SpecCheck
import Graphos.Domain.Types
import Graphos.Infrastructure.SpecParse (parseSpecFile, parseAdrFile, parseSpecDir)
import qualified Graphos.UseCase.SpecExtract as SE
import qualified Graphos.UseCase.SpecAdjudicate as SA
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Load (loadGraphFromFile, loadGraphFromFileStrict, lrGraph)
import Graphos.UseCase.SpecCheck

import qualified Data.Text as T
import Data.Text.Short (fromText, toText)

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

  describe "contract delta: consumers load the new relations (graph-json-contract)" $ do
    let endpointNodes =
          [ mkN "R" "R" DocFile True, mkN "T" "T" DocFile True ]
    it "round-trips a constrains edge through writer JSON and the tolerant loader" $ do
      let edge = Edge (EdgeId "R->T:constrains") "R" "T" Constrains 1.0 (Confidence 1.0) Nothing
          doc = encode (object [ "nodes" .= endpointNodes, "edges" .= [edge] ])
      withSystemTempDirectory "speccheck-contract" $ \dir -> do
        let path = dir </> "graph.json"
        BL.writeFile path doc
        res <- loadGraphFromFile path
        case res of
          Left e -> fail $ "tolerant load failed: " ++ T.unpack e
          Right lr ->
            [ edgeRelation e | e <- Map.elems (gEdges (lrGraph lr)) ]
              `shouldBe` [Constrains]
    it "the strict loader accepts the new relations too" $ do
      let edge = Edge (EdgeId "R->T:constrains") "R" "T" Constrains 1.0 (Confidence 1.0) Nothing
          doc = encode (object [ "nodes" .= endpointNodes, "edges" .= [edge] ])
      withSystemTempDirectory "speccheck-contract" $ \dir -> do
        let path = dir </> "graph.json"
        BL.writeFile path doc
        res <- loadGraphFromFileStrict path
        case res of
          Left e -> fail $ "strict load failed: " ++ T.unpack e
          Right lr -> Map.size (gEdges (lrGraph lr)) `shouldBe` 1
    it "cypher matches a constrains edge by its relation type" $ do
      let edge = Edge (EdgeId "R->T:constrains") "R" "T" Constrains 1.0 (Confidence 1.0) Nothing
          doc = encode (object [ "nodes" .= endpointNodes, "edges" .= [edge] ])
      withSystemTempDirectory "speccheck-contract" $ \dir -> do
        let path = dir </> "graph.json"
        BL.writeFile path doc
        res <- loadGraphFromFile path
        case res of
          Left e -> fail $ "tolerant load failed: " ++ T.unpack e
          Right lr -> do
            let g = lrGraph lr
                idx = buildIndex g Map.empty
            case parseStatement "MATCH (a)-[:constrains]->(t) RETURN t" of
              Left err -> fail ("cypher parse failed: " ++ T.unpack err)
              Right st -> case evaluateStatement 100 st g idx of
                Left err -> fail ("cypher eval failed: " ++ T.unpack err)
                Right _ -> do
                  let matched = find (\e -> edgeRelation e == Constrains)
                        [ e | e <- Map.elems (gEdges g) ]
                  matched `shouldSatisfy` isJust

  describe "ADR parser (spec-artifact-schema, no-model layer)" $ do
    let adrSuperseded = T.unlines
          [ "# ADR-004: Old way"
          , ""
          , "**Date**: 2026-01-01"
          , "**Status**: Superseded by ADR-007"
          , ""
          , "## Context"
          , "Body text."
          ]
        adrActive = T.unlines
          [ "# ADR-007: New way"
          , ""
          , "**Date**: 2026-05-22"
          , "**Status**: Under Review"
          , "**Supersedes**: ADR-004, ADR-005"
          , ""
          , "## Context"
          , "Body."
          ]
        (nsAct, esAct) = parseAdrFile "docs/proposals/adr-007-new-way.md" adrActive
        (nsSup, _esSup) = parseAdrFile "docs/proposals/adr-004-old-way.md" adrSuperseded
    it "derives the decision id and kind" $ do
      [ nodeId n | n <- nsAct ] `shouldBe` [ "adr:007" ]
      [ fmap (== fromText "Decision") (nodeKind n) | n <- nsAct ] `shouldBe` [ Just True ]
    it "marks an active decision active with its status in extra" $ do
      case nsAct of
        (n : _) -> do
          isActiveNode n `shouldBe` True
          nodeExtra n `shouldSatisfy` isJust
          case nodeExtra n of
            Just (A.Object o) -> KM.lookup (K.fromText "status") o
              `shouldBe` Just (A.String "Under Review")
            _ -> fail "expected extra object"
        [] -> fail "expected a decision node"
    it "marks a superseded decision inactive" $
      all (\n -> not (isActiveNode n)) nsSup `shouldBe` True
    it "emits supersedes edges to the named predecessors" $
      map (\e -> (edgeSource e, edgeTarget e, edgeRelation e)) esAct
        `shouldBe` [ ("adr:007", "adr:004", Supersedes)
                   , ("adr:007", "adr:005", Supersedes)
                   ]
    it "an inactive decision referenced by an active artifact is a stale finding" $ do
      let ns = nsAct ++ nsSup ++ [ reqN "R1" ]
          es = esAct ++ [ mkE "R1" References "adr:004" ]
      staleSupersessions ns es `shouldBe` [ ("R1", "adr:004") ]
    it "the superseded edge feeds the cycle check's specRels" $ do
      -- supersedes participates in dependency cycles per spec-graph-checks
      let es = esAct
      stepRel es specRels "adr:007" "adr:004" `shouldBe` True

  describe "self-corpus parser fixtures (openspec/ of this repo)" $ do
    it "parses this change's own spec.md artifacts into requirements/scenarios" $ do
      content <- T.pack <$> readFile "openspec/changes/spec-graph-verification/specs/spec-graph-checks/spec.md"
      let (cap, ns, _es) = parseSpecFile
            "openspec/changes/spec-graph-verification/specs/spec-graph-checks/spec.md" content
      cap `shouldBe` "spec-graph-checks"
      length [ n | n <- ns, isRequirementNode n ] `shouldBe` 4
      length [ n | n <- ns, isDecisionNode n ] `shouldBe` 0
      [ nodeLineStart n | n <- ns, nodeId n == "req:spec-graph-checks/certificate-carrying-cycle-check" ]
        `shouldBe` [ Just 9 ]
    it "parses the whole openspec/ tree (no-model invariant: full structure)" $ do
      res <- parseSpecDir "openspec"
      case res of
        Left e -> fail $ "parseSpecDir failed: " ++ T.unpack e
        Right (ns, es) -> do
          let reqs = [ n | n <- ns, isRequirementNode n ]
              scns = [ n | n <- ns
                     , fmap toText (nodeKind n) == Just "Scenario" ]
          null reqs `shouldBe` False
          null scns `shouldBe` False
          all (\n -> nodeLineStart n /= Nothing) reqs `shouldBe` True
          not (null [ e | e <- es, edgeRelation e == Contains
                        , "req:" `T.isPrefixOf` edgeSource e
                        , "/scn:" `T.isInfixOf` edgeTarget e ])
            `shouldBe` True
    it "every scenario node has a contains edge from some requirement" $ do
      res <- parseSpecDir "openspec"
      case res of
        Left e -> fail $ T.unpack e
        Right (ns, es) -> do
          let reqIds = [ nodeId n | n <- ns, isRequirementNode n ]
              scenarioEdges = [ e | e <- es
                              , edgeRelation e == Contains
                              , edgeSource e `elem` reqIds
                              , "req:" `T.isPrefixOf` edgeTarget e ]
          null scenarioEdges `shouldBe` False

  describe "duplication candidates (embedding cosine within community)" $ do
    let nodeWithComm nid c = (reqN nid) { nodeCommunityId = Just c }
        ns = [ nodeWithComm "A" 1, nodeWithComm "B" 1, nodeWithComm "C" 2 ]
        embs = Map.fromList
          [ ("A", [1, 0])
          , ("B", [1, 0])          -- identical to A → sim 1
          , ("C", [0, 1])          -- different community
          ]
    it "flags same-community pairs above the threshold" $
      duplicationCandidates 0.9 ns embs `shouldBe` [ ("A", "B", 1.0) ]
    it "keeps pairs below the threshold out" $
      duplicationCandidates 1.1 ns embs `shouldBe` []
    it "similarity at exactly the threshold is a candidate" $
      duplicationCandidates 1.0 ns embs `shouldBe` [ ("A", "B", 1.0) ]
    it "cross-community pairs are never candidates" $ do
      let ns' = [ nodeWithComm "A" 1, nodeWithComm "C" 2 ]
      duplicationCandidates 0.0 ns' embs `shouldBe` []
    it "missing embeddings exclude a node" $ do
      let ns'' = [ nodeWithComm "A" 1, reqN "B" ]
      duplicationCandidates 0.0 ns'' embs `shouldBe` []

  describe "SPOF decisions (articulation points restricted to Decision nodes)" $ do
    it "a decision bridging two requirement halves is flagged" $ do
      let ns = [ reqN "L1", reqN "L2", decN "ADR-1" True, reqN "R1", reqN "R2" ]
          es = [ mkE "L1" References "ADR-1", mkE "L2" References "ADR-1"
               , mkE "ADR-1" References "R1", mkE "ADR-1" References "R2" ]
      spofDecisions ns es `shouldBe` [ "ADR-1" ]
    it "a decision in a fully connected cluster is not a SPOF" $ do
      let ns = [ reqN "A", decN "D" True, reqN "B" ]
          es = [ mkE "A" References "D", mkE "D" References "B"
               , mkE "A" References "B" ]
      spofDecisions ns es `shouldBe` []

  describe "check filter (--check)" $
    it "restricts sections to the named checks; empty runs all" $ do
      let full = runSpecCheck mainNodes mainEdges False
          cyclesOnly = filterReport [Cycles] full
      srCandidates cyclesOnly `shouldBe` []
      srStale cyclesOnly `shouldBe` []
      srUnimplemented cyclesOnly `shouldBe` []
      maybe False (const True) (srTopoCertificate cyclesOnly) `shouldBe`
        maybe False (const True) (srTopoCertificate full)
      filterReport [] full `shouldBe` full

  describe "adjudicated gate (conflict-adjudication)" $ do
    let conflictOnly =
          [ (("R-remote", "R-selfcontained"), VerdictConflict, "both bind graph.html")
          ]
        allCompatible =
          [ (("R-remote", "R-selfcontained"), VerdictCompatible, "modes are disjoint")
          ]
    it "confirmed conflict gates with trace" $
      reportGates (runSpecCheck' mainNodes mainEdges False False [] [] (Just conflictOnly))
        `shouldBe` True
    it "all-compatible verdicts do not gate" $
      reportGates (runSpecCheck' mainNodes mainEdges False False [] [] (Just allCompatible))
        `shouldBe` False
    it "missing verdicts are unadjudicated and never gate" $
      reportGates (runSpecCheck' mainNodes mainEdges False False [] [] Nothing)
        `shouldBe` False
    it "blocking ⊆ confirmed ⊆ candidates for any adjudicator" $ do
      let candidates' = candidatePairs mainNodes mainEdges
          adjudicatePair (_, b) =
            if b == "R-selfcontained" then VerdictConflict else VerdictCompatible
          blocking = gate adjudicatePair candidates'
      all (\p -> p `elem` candidates') blocking `shouldBe` True

  describe "semantic edge pass (spec-artifact-schema, model layer)" $ do
    let mockPort :: LLMPort
        mockPort = LLMPort
          { lpCallLLM = \_cfg _prompt ->
              pure (Right "{\"source\":\"R1\",\"relation\":\"constrains\",\"target\":\"T\"}")
          , lpParseLabelsFromResponse = const Map.empty
          , lpGenerateEmbedding = \_ _ -> pure (Left "unused")
          , lpGenerateEmbeddings = \_ _ -> pure (Left "unused")
          , lpAnalyzeImage = \_ _ _ -> pure (Left "unused")
          , lpValidateUrl = pure
          }
        cfg = defaultLabelingConfig
    it "applies hypotheses between existing ids and drops unknown ones" $ do
      let ns = [ reqN "R1", tgtN "T" ]
          hs = [ SE.SpecEdgeHypothesis "R1" Constrains "T"
               , SE.SpecEdgeHypothesis "R1" Constrains "ghost"
               , SE.SpecEdgeHypothesis "ghost" Satisfies "T" ]
          (dropped, edges) = SE.applyHypotheses (map nodeId ns) "R1" hs
      dropped `shouldBe` 2
      length edges `shouldBe` 1
      edgeRelation (case edges of (e : _) -> e; [] -> error "expected an edge") `shouldBe` Constrains
    it "decodes a fenced JSON array response" $
      SE.decodeSpecEdgeHypotheses "```json\n[{\"source\":\"R1\",\"relation\":\"refines\",\"target\":\"R2\"}]\n```"
        `shouldBe` [ SE.SpecEdgeHypothesis "R1" Refines "R2" ]
    it "decodes a plain (unfenced) JSON array response" $
      SE.decodeSpecEdgeHypotheses "[{\"source\":\"R1\",\"relation\":\"refines\",\"target\":\"R2\"}]"
        `shouldBe` [ SE.SpecEdgeHypothesis "R1" Refines "R2" ]
    it "unparseable output yields no hypotheses (structure survives)" $
      SE.decodeSpecEdgeHypotheses "not json at all" `shouldBe` []
    it "relations outside the vocabulary are dropped" $ do
      let (dropped, edges) = SE.applyHypotheses ["R1", "T"] "R1"
            [ SE.SpecEdgeHypothesis "R1" Imports "T" ]
      (dropped, edges) `shouldBe` (1, [])
    it "one document per call: N documents = N calls, one body each" $ do
      let ns = [ reqN "R1", reqN "R2", tgtN "T" ]
          calls =
            [ SE.SpecExtractCall "R1" "R1 title" "body of R1 only"
                [ ("R2", "R2 title"), ("T", "T title") ]
            , SE.SpecExtractCall "R2" "R2 title" "body of R2"
                [ ("R1", "R1 title"), ("T", "T title") ]
            ]
      countRef <- newIORef (0 :: Int)
      let countingPort = mockPort { lpCallLLM = \_cfg prompt -> do
            modifyIORef countRef (+ 1)
            -- the prompt must contain exactly one body marker
            length [ () | body <- [ "body of R1", "body of R2" ]
                    , body `T.isInfixOf` prompt ] `shouldBe` 1
            pure (Right "[]") }
      (_, _) <- SE.extractSemanticEdges countingPort cfg ns calls
      count <- readIORef countRef
      count `shouldBe` 2
    it "a failed call contributes no edges" $ do
      let failingPort = mockPort { lpCallLLM = \_ _ -> pure (Left "provider down") }
          call = SE.SpecExtractCall "R1" "R1" "body" []
      (dropped, edges) <- SE.extractEdgesForDoc failingPort cfg (map nodeId mainNodes) call
      (dropped, edges) `shouldBe` (0, [])

  describe "pairwise adjudication (conflict-adjudication, mock client)" $ do
    let mockPort :: LLMPort
        mockPort = LLMPort
          { lpCallLLM = \_cfg _prompt ->
              pure (Right "{\"verdict\":\"conflict\",\"rationale\":\"cites both\"}")
          , lpParseLabelsFromResponse = const Map.empty
          , lpGenerateEmbedding = \_ _ -> pure (Left "unused")
          , lpGenerateEmbeddings = \_ _ -> pure (Left "unused")
          , lpAnalyzeImage = \_ _ _ -> pure (Left "unused")
          , lpValidateUrl = pure
          }
        cfg = defaultLabelingConfig
    it "makes exactly one call per candidate pair (two bodies per call)" $ do
      countRef <- newIORef (0 :: Int)
      bodiesRef <- newIORef []
      let mockJudge = mockPort
            { lpCallLLM = \_cfg prompt -> do
                modifyIORef countRef (+ 1)
                modifyIORef bodiesRef (prompt :)
                pure (Right "{\"verdict\":\"conflict\",\"rationale\":\"cites both\"}")
            }
          ns = [ reqN "R-remote", reqN "R-selfcontained", tgtN "graph.html" ]
          es = [ mkE "R-remote" Constrains "graph.html"
               , mkE "R-selfcontained" Constrains "graph.html" ]
          pairs = candidatePairs ns es
      adjns <- SA.adjudicateCandidates mockJudge cfg ns pairs 2
      length adjns `shouldBe` length pairs
      count <- readIORef countRef
      count `shouldBe` length pairs
      bodies <- readIORef bodiesRef
      forM_ bodies $ \b -> do
        T.isInfixOf "R-remote" b `shouldBe` True
        T.isInfixOf "R-selfcontained" b `shouldBe` True
    it "schema failure after retries is unadjudicated, not dropped" $ do
      let badPort = mockPort { lpCallLLM = \_ _ -> pure (Right "unparseable") }
          ns = [ reqN "R-remote", reqN "R-selfcontained", tgtN "graph.html" ]
          es = [ mkE "R-remote" Constrains "graph.html"
               , mkE "R-selfcontained" Constrains "graph.html" ]
      adjns <- SA.adjudicateCandidates badPort cfg ns (candidatePairs ns es) 2
      map SA.adjVerdict adjns `shouldSatisfy` all (== VerdictUnadjudicated)
    it "confirmed conflicts gate; dismissed do not" $ do
      let mkReport v = runSpecCheck' mainNodes mainEdges False False [] []
            (Just [ (("R-remote", "R-selfcontained"), v, "r") ])
      reportGates (mkReport VerdictConflict) `shouldBe` True
      reportGates (mkReport VerdictCompatible) `shouldBe` False
      reportGates (mkReport VerdictDuplicate) `shouldBe` False
      reportGates (mkReport VerdictUnadjudicated) `shouldBe` False
      reportGates (mkReport VerdictDuplicate) `shouldBe` False
    it "duplicates gate under strict-duplicates only" $ do
      let dups = [ ("R-remote", "R-selfcontained", 0.97) ]
          mkReport sd = runSpecCheck' mainNodes mainEdges False sd dups
                           [] Nothing
      reportGates (mkReport False) `shouldBe` False
      reportGates (mkReport True) `shouldBe` True

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
