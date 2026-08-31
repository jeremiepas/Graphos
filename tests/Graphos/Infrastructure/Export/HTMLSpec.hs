{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Export.HTMLSpec where

import Test.Hspec
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key ()
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Short (fromText)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import System.Directory (findExecutable)
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))

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
  , nodeLabel        = fromText label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText srcFile
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Just 1
  , nodeKind         = fmap fromText mKind
  , nodeDegree       = Just 2
  , nodeIsBridge     = Just False
  , nodeExtra        = Nothing
  , nodePresentBits  = 0
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

  describe "exported document conformance (viewer assets)" $ do
    let g = buildGraph False (extractionFromLists
            [ mkNode "a" "label-a" "src/A.hs" (Just "Function")
            , mkNode "b" "label-b" "src/A.hs" (Just "Function")
            , mkNode "c" "label-c" "src/B.hs" Nothing
            , mkNode "d" "doc-1" "doc/readme.md" Nothing
            ]
            [ mkEdge "a" "b" Calls
            , mkEdge "b" "c" Imports
            , mkEdge "a" "d" Contains
            ])
        commMap = detectCommunities g
        cohesion = scoreAllCohesion g commMap
        analysis = analyze g commMap cohesion

    it "embeds the vendored renderer and reports its version" $ do
      raw <- exportFixture g analysis
      T.isInfixOf "<meta name='graphos-renderer' content='vis-network 10.1.1'>" raw `shouldBe` True
      T.isInfixOf "vis-network" raw `shouldBe` True

    it "references no external origin in any src/href attribute" $ do
      raw <- exportFixture g analysis
      let isExternal t = "http://" `T.isInfixOf` t || "https://" `T.isInfixOf` t
          attrs = [ T.takeWhile (\c -> c /= ' ' && c /= '>' && c /= '\'') t
                  | t <- T.lines raw
                  , let tl = T.toLower t
                  , "src=" `T.isInfixOf` tl || "href=" `T.isInfixOf` tl ]
          externals = filter isExternal attrs
      externals `shouldBe` []

    it "embeds the viewer stylesheet and script byte-identically" $ do
      raw <- exportFixture g analysis
      cssOk <- TIO.readFile "assets/viewer/viewer.css"
      jsOk <- TIO.readFile "assets/viewer/viewer.js"
      T.isInfixOf cssOk raw `shouldBe` True
      T.isInfixOf jsOk raw `shouldBe` True

    it "contains no back-button element or handler" $ do
      raw <- exportFixture g analysis
      js <- TIO.readFile "assets/viewer/viewer.js"
      T.isInfixOf "btnBack" raw `shouldBe` False
      T.isInfixOf "Back to overview" raw `shouldBe` False
      T.isInfixOf "backToOverview" raw `shouldBe` False
      T.isInfixOf "btnBack" js `shouldBe` False

    it "offers the four depth levels with Overview default" $ do
      raw <- exportFixture g analysis
      T.isInfixOf "<option value='overview'>Overview</option>" raw `shouldBe` True
      T.isInfixOf "<option value='community'>Community</option>" raw `shouldBe` True
      T.isInfixOf "<option value='full'>Full</option>" raw `shouldBe` True
      T.isInfixOf "<option value='custom'>Custom</option>" raw `shouldBe` True

    it "has a golden payload shape" $ do
      raw <- exportFixture g analysis
      let jsonTxt = fromMaybe (error "payload not found") (extractPayload raw)
      case (A.decode (BSL.fromStrict (TE.encodeUtf8 jsonTxt)) :: Maybe A.Object) of
        Nothing -> expectationFailure "payload is not valid JSON"
        Just o -> do
          let keys = L.sort (map id (KM.keys o))
          keys `shouldBe` ["aggregates", "edges", "files", "kinds", "nodes", "relations", "strings"]

    it "carries a non-empty payload for a graph with nodes and edges" $ do
      raw <- exportFixture g analysis
      let jsonTxt = fromMaybe (error "payload not found") (extractPayload raw)
      case (A.decode (BSL.fromStrict (TE.encodeUtf8 jsonTxt)) :: Maybe A.Object) of
        Nothing -> expectationFailure "payload not decodable"
        Just o -> do
          let nodes = KM.lookup "nodes" o >>= parseMaybe A.parseJSON :: Maybe [A.Value]
              edges = KM.lookup "edges" o >>= parseMaybe A.parseJSON :: Maybe [A.Value]
              aggs  = KM.lookup "aggregates" o >>= parseMaybe A.parseJSON :: Maybe [A.Value]
          length (fromMaybe [] nodes) `shouldSatisfy` (> 0)
          length (fromMaybe [] edges) `shouldSatisfy` (> 0)
          length (fromMaybe [] aggs) `shouldSatisfy` (> 0)

  describe "viewer source conformance" $ do
    it "does not duplicate viewer code as string literals in the Haskell module" $ do
      hsSource <- TIO.readFile "src/Graphos/Infrastructure/Export/HTML.hs"
      -- distinctive JS/CSS tokens that would only appear in the module as
      -- duplicated string literals (all embedded via file-embed instead)
      mapM_
        (\tok -> T.isInfixOf (T.pack tok) hsSource `shouldBe` False)
        [ "function neighborhoodNodeIds"
        , "sessionStorage"
        , "hideEdgesOnDrag"
        , "deepMerge"
        , ".legend-item.active"
        , ".search-verdict"
        ]
      -- the assets must be pulled in from their source files at compile time
      T.isInfixOf "embedFile \"assets/viewer/viewer.css\"" hsSource `shouldBe` True
      T.isInfixOf "embedFile \"assets/viewer/viewer.js\"" hsSource `shouldBe` True
      T.isInfixOf "embedFile \"assets/viewer/vis-network.min.js\"" hsSource `shouldBe` True

    it "defines the renderer options exactly once with interaction keys in place" $ do
      js <- TIO.readFile "assets/viewer/viewer.js"
      T.count (T.pack "var BASE_OPTIONS = {") js `shouldBe` 1
      T.count (T.pack "new vis.Network(") js `shouldBe` 1
      T.count (T.pack "hideEdgesOnDrag: true") js `shouldBe` 1
      T.count (T.pack "hideEdgesOnZoom: true") js `shouldBe` 1
      let withinInteraction = T.isInfixOf
            (T.pack "interaction: {\n      hover: true, tooltipDelay: 200, navigationButtons: false, keyboard: true,\n      zoomView: true, dragView: true,\n      hideEdgesOnDrag: true, hideEdgesOnZoom: true\n    }")
            js
      withinInteraction `shouldBe` True

    it "persists state via sessionStorage with a stale-reference fallback" $ do
      js <- TIO.readFile "assets/viewer/viewer.js"
      T.isInfixOf "sessionStorage.setItem" js `shouldBe` True
      T.isInfixOf "sessionStorage.getItem" js `shouldBe` True
      T.isInfixOf "4096" js `shouldBe` True

    it "implements client-side N-hop BFS for Custom depth" $ do
      js <- TIO.readFile "assets/viewer/viewer.js"
      T.isInfixOf "function neighborhoodNodeIds(startId, hops)" js `shouldBe` True

    it "has a CSS rule for every class written by the viewer JS" $ do
      js <- TIO.readFile "assets/viewer/viewer.js"
      css <- TIO.readFile "assets/viewer/viewer.css"
      let jsClasses = nubSorted (classesInHtml js)
          cssClasses = nubSorted (classesInCss css)
          missing = filter (`notElem` cssClasses) jsClasses
      missing `shouldBe` []

    it "emitted viewer script parses under node --check" $ do
      node <- findExecutable "node"
      case node of
        Nothing -> pendingWith "node is not installed"
        Just nodePath -> do
          let cg = buildGraph False (extractionFromLists
                [ mkNode "a" "label-a" "src/A.hs" (Just "Function")
                , mkNode "b" "label-b" "src/A.hs" (Just "Function")
                , mkNode "c" "label-c" "src/B.hs" Nothing
                , mkNode "d" "doc-1" "doc/readme.md" Nothing
                ]
                [ mkEdge "a" "b" Calls
                , mkEdge "b" "c" Imports
                , mkEdge "a" "d" Contains
                ])
              ccm = detectCommunities cg
              cco = scoreAllCohesion cg ccm
              can = analyze cg ccm cco
          raw <- exportFixture cg can
          withSystemTempDirectory "graphos-js" $ \dir -> do
            let path = dir </> "viewer.js"
            TIO.writeFile path (extractViewerScript raw)
            (code, _, _) <- readProcessWithExitCode nodePath ["--check", path] ""
            code `shouldBe` ExitSuccess

-- Helpers for asset conformance checks

-- | Export the small conformance fixture and return its text.
exportFixture :: Graph -> Analysis -> IO Text
exportFixture g analysis = do
  exportHTML g analysis Nothing [] "/tmp/graphos-conformance.html"
  TIO.readFile "/tmp/graphos-conformance.html"

-- | Extract the inline payload JSON from an emitted document.
extractPayload :: Text -> Maybe Text
extractPayload raw = do
  let marker = "const _payloadData = "
      (_, rest) = T.breakOn marker raw
      body = T.drop (T.length marker) rest
      (jsonTxt, _) = T.breakOn ";\n</script>" body
  if T.null jsonTxt then Nothing else Just jsonTxt

-- | Extract the viewer script between the payload script and the footer.
extractViewerScript :: Text -> Text
extractViewerScript raw =
  let marker = "/* Graphos HTML viewer."
      (_, rest) = T.breakOn marker raw
      fromMarker = marker `T.append` T.drop (T.length marker) rest
      (script, _) = T.breakOn "</script>\n</body></html>" fromMarker
  in script

-- | Collect distinct class tokens from `class="..."` literals in JS.
classesInHtml :: Text -> [Text]
classesInHtml = go
  where
    go t =
      case T.breakOn "class=\"" t of
        (_, rest) | T.null rest -> []
        (_, rest) ->
          let afterQuotes = T.drop 7 rest
              (cls, more) = T.breakOn "\"" afterQuotes
          in filter isClass (T.words cls) ++ go (T.drop 1 more)

    isClass w = not (T.null w)
      && isAlpha (T.head w)
      && T.all isClassChar w

    isClassChar c = isAlpha c || (c >= '0' && c <= '9') || c == '-' || c == '_'
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

-- | Collect distinct class tokens from CSS selectors (`.foo`, `.foo.bar`).
classesInCss :: Text -> [Text]
classesInCss = go
  where
    go t
      | T.null t = []
      | otherwise =
          let (_, rest) = T.breakOn "." t
          in if T.null rest
               then []
               else let afterDot = T.drop 1 rest
                        cls = T.takeWhile (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '-' || c == '_') afterDot
                     in if T.null cls
                          then go afterDot
                          else cls : go (T.drop (T.length cls) afterDot)

nubSorted :: Ord a => [a] -> [a]
nubSorted = L.sort . L.nub
