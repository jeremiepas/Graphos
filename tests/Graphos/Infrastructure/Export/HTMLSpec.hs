{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Export.HTMLSpec where

import Test.Hspec
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, gEdges)
import Graphos.Domain.Analysis (analyze)
import Graphos.Domain.Community (detectCommunities, scoreAllCohesion)
import Graphos.Infrastructure.Export.HTML
  ( communityAggregatesToJSON
  , computePayload
  , exportHTML
  , VisCommunityAggregate(..)
  , VisPayload(..)
  )

-- Helpers
mkNode :: Text -> Text -> Text -> Maybe Text -> Node
mkNode nid label srcFile mKind = Node
  { nodeId           = nid
  , nodeLabel        = label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = srcFile
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Just 1
  , nodeKind         = mKind
  , nodeDegree       = Just 2
  , nodeIsBridge     = Just False
  , nodeExtra        = Nothing
  }

mkEdge :: Text -> Text -> Relation -> Edge
mkEdge src tgt rel = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = rel
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

payloadFromGraph :: Graph -> VisPayload
payloadFromGraph g =
  let commMap = detectCommunities g
      cohesion = scoreAllCohesion g commMap
      analysis = analyze g commMap cohesion
  in computePayload g analysis Nothing []

spec :: Spec
spec = do
  describe "communityAggregatesToJSON" $ do
    let g = buildGraph False (extractionFromLists [] [])
        commMap = Map.fromList [(1, [T.pack "a", T.pack "b"]), (2, [T.pack "c"])]

    it "uses LLM label when present" $ do
      let labels = Just (Map.fromList [(1, T.pack "Auth Module"), (2, T.pack "Parser")])
          aggs = communityAggregatesToJSON g commMap labels
      length aggs `shouldBe` 2
      map vcaLabel aggs `shouldMatchList` [T.pack "Auth Module", T.pack "Parser"]

    it "falls back to Community <id> when labels are absent" $ do
      let aggs = communityAggregatesToJSON g commMap Nothing
      length aggs `shouldBe` 2
      map vcaLabel aggs `shouldMatchList` [T.pack "Community 1", T.pack "Community 2"]

    it "falls back for missing keys in partial label map" $ do
      let labels = Just (Map.fromList [(1, T.pack "Auth Module")])
          aggs = communityAggregatesToJSON g commMap labels
      length aggs `shouldBe` 2
      map vcaLabel aggs `shouldMatchList` [T.pack "Auth Module", T.pack "Community 2"]

    it "falls back for empty string labels" $ do
      let labels = Just (Map.fromList [(1, T.pack ""), (2, T.pack "Parser")])
          aggs = communityAggregatesToJSON g commMap labels
      length aggs `shouldBe` 2
      map vcaLabel aggs `shouldMatchList` [T.pack "Community 1", T.pack "Parser"]

  describe "computePayload interning" $ do
    let nodes =
          [ mkNode "a" "label-a" "src/A.hs" (Just "Function")
          , mkNode "b" "label-b" "src/A.hs" (Just "Function")
          , mkNode "c" "label-c" "src/B.hs" Nothing
          ]
        edges =
          [ mkEdge "a" "b" Calls
          , mkEdge "b" "c" Imports
          ]
        g = buildGraph False (extractionFromLists nodes edges)
        payload = payloadFromGraph g
        encodedLazy = A.encode payload
        encoded = BSL.toStrict encodedLazy
        decoded = fromMaybe (error "payload decode failed") (A.decodeStrict encoded :: Maybe A.Object)

    it "interns source files so each distinct file appears once" $ do
      let files = KM.lookup "files" decoded >>= parseMaybe A.parseJSON :: Maybe [Text]
      files `shouldBe` Just ["src/A.hs", "src/B.hs"]

    it "references source files by integer index" $ do
      let nodes' = KM.lookup "nodes" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
      case nodes' of
        Just (n:_) -> KM.lookup "file_idx" n `shouldBe` Just (A.Number 0)
        _          -> expectationFailure "expected nodes"

    it "round-trips node identity through strings table" $ do
      let strings = KM.lookup "strings" decoded >>= parseMaybe A.parseJSON :: Maybe [Text]
          nodes' = KM.lookup "nodes" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
      strings `shouldBe` Just ["a", "b", "c"]
      case nodes' of
        Just (n0:_) -> KM.lookup "label" n0 `shouldBe` Just (A.String "label-a")
        _           -> expectationFailure "expected at least one node"

    it "serializes edges as integer index triples" $ do
      let edges' = KM.lookup "edges" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Array]
      case edges' of
        Just (e:_) -> do
          length (toList e) `shouldBe` 3
          toList e `shouldBe` [A.Number 0, A.Number 1, A.Number 0]
        _          -> expectationFailure "expected edges"

    it "emits numeric community ids in aggregate records" $ do
      let aggs = KM.lookup "aggregates" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
      case aggs of
        Just as ->
          mapM_ (\a ->
            case KM.lookup "id" a of
              Just (A.Number _) -> pure ()
              _ -> expectationFailure "expected numeric aggregate id"
            ) as
        _ -> expectationFailure "expected aggregates"

    it "emits the same community id type in node and aggregate records" $ do
      let nodes' = KM.lookup "nodes" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
          aggs = KM.lookup "aggregates" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
      case (nodes', aggs) of
        (Just ns, Just as) -> do
          let nodeCids = [cid | n <- ns, Just (A.Number cid) <- [KM.lookup "community_id" n]]
              aggIds   = [aid | a <- as, Just (A.Number aid) <- [KM.lookup "id" a]]
          all (\_ -> True) nodeCids `shouldBe` True
          all (\_ -> True) aggIds `shouldBe` True
        _ -> expectationFailure "expected nodes and aggregates"

    it "does not emit forbidden per-node styling keys" $ do
      let nodes' = KM.lookup "nodes" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Object]
      case nodes' of
        Just ns ->
          mapM_ (\n -> do
            KM.member "color" n `shouldBe` False
            KM.member "group" n `shouldBe` False
            KM.member "title" n `shouldBe` False
            ) ns
        _ -> expectationFailure "expected nodes"

    it "does not emit forbidden per-edge styling keys" $ do
      let edges' = KM.lookup "edges" decoded >>= parseMaybe A.parseJSON :: Maybe [A.Array]
      case edges' of
        Just es ->
          mapM_ (\e -> do
            let txt = TE.decodeUtf8 (BSL.toStrict (A.encode e))
            T.isInfixOf "color"   txt `shouldBe` False
            T.isInfixOf "arrows"  txt `shouldBe` False
            T.isInfixOf "dashes"  txt `shouldBe` False
            T.isInfixOf "width"   txt `shouldBe` False
            T.isInfixOf "title"   txt `shouldBe` False
            T.isInfixOf "label"   txt `shouldBe` False
            ) es
        _ -> expectationFailure "expected edges"

    it "does not embed signature text in the payload" $ do
      let raw = TE.decodeUtf8 encoded
      T.isInfixOf "signature text" raw `shouldBe` False

    it "produces deterministic output for the same graph" $ do
      let p1 = payloadFromGraph g
          p2 = payloadFromGraph g
      A.encode p1 `shouldBe` A.encode p2

    it "meets the per-node and per-edge budget on a small graph" $ do
      let nodeCount = Map.size (gNodes g)
          edgeCount = Map.size (gEdges g)
          nodesSize = fromIntegral (BSL.length (A.encode (vpNodes payload))) :: Double
          edgesSize = fromIntegral (BSL.length (A.encode (vpEdges payload))) :: Double
          perNode = nodesSize / fromIntegral nodeCount
          perEdge = edgesSize / fromIntegral edgeCount
      perNode `shouldSatisfy` (<= 200.0)
      perEdge `shouldSatisfy` (<= 24.0)

  describe "exportHTML produces a self-contained document" $ do
    it "writes a syntactically closed HTML document" $ do
      let nodes = [mkNode "x" "label-x" "src/X.hs" Nothing]
          g = buildGraph False (extractionFromLists nodes [])
          commMap = detectCommunities g
          cohesion = scoreAllCohesion g commMap
          analysis = analyze g commMap cohesion
      exportHTML g analysis Nothing [] "/tmp/graphos-test.html"
      raw <- TIO.readFile "/tmp/graphos-test.html"
      T.isInfixOf "<!DOCTYPE html>" raw `shouldBe` True
      T.isInfixOf "</html>" raw `shouldBe` True
      T.isInfixOf "const _payloadData = " raw `shouldBe` True
