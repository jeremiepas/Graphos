{-# LANGUAGE OverloadedStrings #-}
-- | Specs for the Neo4j push internals (optimize-neo4j-full-push).
--
-- Covers the change's three test-first tasks:
--   * 1.1 hash helper — sha1 lowercase-hex over UTF-8 bytes, parity vectors
--     shared with the backfill Cypher, property test (any Text → 40 hex)
--   * 1.2 batch planning — node chunks at 1,000 rows or ~4 MB payload
--     (a single oversized node ships alone), edge rows grouped by relationship
--     type with 5,000-row chunks, nodes always flushed before edges
--   * 1.3 statement generation — node UNWIND MERGE keys on id_hash with raw
--     id in ON CREATE SET, edge UNWIND matches endpoints by id_hash,
--     relationship type backtick-escaped (never parameterized as a string)
module Graphos.Infrastructure.Export.Neo4jSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAsciiLower, isDigit, isHexDigit)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Graphos.Domain.CypherHash (nodeIdHash, textSha1Hex)
import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Infrastructure.Export.Neo4j

-- ────────────────────────────────────────────────────────────────
-- Fixtures
-- ────────────────────────────────────────────────────────────────

mkNode :: NodeId -> Node
mkNode nid = Node
  { nodeId           = nid
  , nodeLabel        = TS.fromText ("label:" <> nid)
  , nodeFileType     = CodeFile
  , nodeSourceFile   = TS.fromText "test.hs"
,   nodeSource = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeSignature    = Nothing
  , nodeCommunityId  = Nothing
  , nodeKind         = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodePresentBits  = 33  -- line_start + line_end + signature presence, arbitrary
  }

mkEdge :: Relation -> NodeId -> NodeId -> Edge
mkEdge rel src tgt = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt <> ":" <> relationToText rel)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = rel
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 0.9
  , edgeExtra      = Nothing
  }

mkGraphFrom :: [Node] -> [Edge] -> Graph
mkGraphFrom nodes edges = buildGraph False (extractionFromLists nodes edges)

statementText :: Value -> Maybe Text
statementText (Object o) = KM.lookup "statement" o >>= asText
  where asText (String t) = Just t
        asText _          = Nothing
statementText _ = Nothing

statementRows :: Value -> Maybe [Value]
statementRows (Object o) = do
  params <- KM.lookup "parameters" o
  (Object po) <- pure params
  rows <- KM.lookup "rows" po
  asArray rows
  where
    asArray (Array a) = Just (foldr (:) [] a)
    asArray _         = Nothing
statementRows _ = Nothing

rowField :: Text -> Value -> Maybe Value
rowField key = \case
  Object o -> KM.lookup (Key.fromText key) o
  _        -> Nothing

statementString :: Value -> Text
statementString stmt = maybe "" id (statementText stmt)

rowByteLen :: Value -> Int64
rowByteLen = fromIntegral . BSL.length . Aeson.encode

nodeWithLabel :: Node -> Text -> Node
nodeWithLabel n l = n { nodeLabel = TS.fromText l }

isHexString :: Text -> Bool
isHexString = T.all (\c -> (isAsciiLower c && isHexDigit c) || isDigitHex c)
  where isDigitHex c = isDigit c && isHexDigit c

-- ────────────────────────────────────────────────────────────────
spec :: Spec
spec = do
  hashSpecs
  batchPlanningSpecs
  statementGenerationSpecs

-- 1.1 — sha1 lowercase-hex hash helper --------------------------------------

hashSpecs :: Spec
hashSpecs = describe "Graphos.Domain.CypherHash" $ do

  it "hashes known parity vectors (also what backfill Cypher sha1() produces)" $ do
    textSha1Hex "abc" `shouldBe` "a9993e364706816aba3e25717850c26c9cd0d89d"
    textSha1Hex ""    `shouldBe` "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    textSha1Hex "The quick brown fox jumps over the lazy dog"
      `shouldBe` "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"

  it "produces 40 lowercase-hex chars for any Text (property)" $
    forAll asciiText $ \t ->
      let h = textSha1Hex t
      in T.length h == 40 && isHexString h

  it "nodeIdHash matches textSha1Hex on the raw id (property)" $
    forAll asciiText $ \t -> nodeIdHash t === textSha1Hex t

  it "is deterministic" $
    nodeIdHash "node-111222333" `shouldBe` nodeIdHash "node-111222333"

  it "differing ids hash differently (sanity sample)" $
    length (dedup (map nodeIdHash ["a", "b", "ab", "ba", "abc"]))
      `shouldBe` 5
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- ASCII-limited generator: covers control chars, quotes, backslashes,
-- backticks and multi-byte letters without surrogate-pair noise.
asciiText :: Gen Text
asciiText = T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
                                          ++ "éüñ中\"'\\`/ :{}[],.-_#"))

-- 1.2 — batch planning --------------------------------------------------------

data StmtKind = NodeKind | EdgeKind | OtherKind deriving (Eq, Show)

stmtKind :: Text -> StmtKind
stmtKind s
  | "MERGE (n:Node" `T.isInfixOf` s = NodeKind
  | "MERGE (src)-[" `T.isInfixOf` s = EdgeKind
  | otherwise = OtherKind

indexOf :: (Eq a) => a -> [a] -> [Int]
indexOf k xs = [i | (i, k') <- zip [0 :: Int ..] xs, k == k']

batchPlanningSpecs :: Spec
batchPlanningSpecs = describe "Neo4j push batch planning" $ do

  it "splits node rows at 1,000 rows by row count" $ do
    let planned = planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes
                    [ nodeRow (mkNode (T.pack ("n" ++ show i))) | i <- [1 .. 2600 :: Int] ]
    map length planned `shouldBe` [1000, 1000, 600]

  it "splits node rows on the ~4 MB payload cap before the row cap when labels are huge" $ do
    let megaLabel = T.replicate (512 * 1024) "x"  -- ~512 KB label ⇒ ~5 rows > 4 MB chunk
        rows      = [ nodeRow (mkNode (T.pack ("huge-" ++ show i)) `nodeWithLabel` megaLabel)
                    | i <- [1 .. 10 :: Int] ]
        planned   = planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes rows
    sum (map length planned) `shouldBe` 10
    -- No chunk exceeds the byte cap.
    map (sum . map rowByteLen) planned `shouldSatisfy` all (<= defaultNodeChunkBytes)
    -- And the split happened well before 1,000 rows.
    map length planned `shouldSatisfy` all (< 1000)

  it "ships a single oversized node alone in its own batch" $ do
    let biggerThanCap = T.replicate (5 * 1024 * 1024) "x"  -- ~5 MB > 4 MB cap
        giant = nodeRow (mkNode "giant" `nodeWithLabel` biggerThanCap)
        small = nodeRow (mkNode "small")
    rowByteLen giant `shouldSatisfy` (> defaultNodeChunkBytes)
    let planned = planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes [giant, small, small]
    -- Giant exceeds the byte cap so it can never share a chunk; the two
    -- smalls bundle together under the caps. The giant still ships (not dropped).
    map length planned `shouldBe` [1, 2]
    case planned of
      ((c0 : _) : _) -> rowByteLen c0 `shouldBe` rowByteLen giant
      _              -> expectationFailure "no chunks"

  it "groups edge rows by relationship type" $ do
    let edges = [ mkEdge Calls "a" "b", mkEdge Imports "b" "c", mkEdge Calls "c" "d" ]
        planned = planEdgeBatches defaultEdgeChunkRows edges
    map fst planned `shouldBe` ["calls", "imports"]
    map (length . snd) planned `shouldBe` [2, 1]

  it "chunks edge groups at 5,000 rows by default" $ do
    let edges = [ mkEdge Calls (T.pack ("n" ++ show i)) (T.pack ("m" ++ show i)) | i <- [1 .. 12000 :: Int] ]
        planned = planEdgeBatches defaultEdgeChunkRows edges
    map fst planned `shouldBe` ["calls", "calls", "calls"]
    map (length . snd) planned `shouldBe` [5000, 5000, 2000]

  it "flushes all node statements before the first edge statement" $ do
    let g = mkGraphFrom [mkNode "a", mkNode "b"] [mkEdge Calls "a" "b"]
        stmts = map statementString (generateParameterizedStatements g)
        kinds = map stmtKind stmts
    -- Nodes strictly precede edges.
    (lastMaybe (indexOf NodeKind kinds), firstMaybe (indexOf EdgeKind kinds))
      `shouldSatisfy` (\(ln, fe) ->
        isJust ln && isJust fe && fromJustS ln < fromJustS fe)
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)
    firstMaybe [] = Nothing
    firstMaybe (x : _) = Just x
    fromJustS (Just x) = x
    fromJustS Nothing  = -1

-- 1.3 — statement generation --------------------------------------------------

statementGenerationSpecs :: Spec
statementGenerationSpecs = describe "Neo4j UNWIND statement generation" $ do

  it "node UNWIND MERGE keys on id_hash and ON CREATE SET stores raw id" $ do
    let stmt = statementString (nodeBatchStatement [nodeRow (mkNode "n1")])
    "MERGE (n:Node {id_hash: row.id_hash})" `T.isInfixOf` stmt `shouldBe` True
    "ON CREATE SET n.id = row.id" `T.isInfixOf` stmt `shouldBe` True
    "MERGE (n:Node {id: " `T.isInfixOf` stmt `shouldBe` False

  it "node rows carry id_hash as the 40-char sha1 of the id" $ do
    let row = nodeRow (mkNode "row-key-test")
    rowField "id_hash" row `shouldBe` Just (String (nodeIdHash "row-key-test"))
    rowField "id" row `shouldBe` Just (String "row-key-test")

  it "edge UNWIND matches both endpoints by id_hash" $ do
    let stmt = statementString (edgeBatchStatement "calls" [edgeRow (mkEdge Calls "a" "b")])
    "MATCH (src:Node {id_hash: row.source_hash})" `T.isInfixOf` stmt `shouldBe` True
    "MATCH (tgt:Node {id_hash: row.target_hash})" `T.isInfixOf` stmt `shouldBe` True
    "{id: row" `T.isInfixOf` stmt `shouldBe` False

  it "edge rows carry endpoint hashes matching the nodes' id_hash" $ do
    let e = mkEdge Calls "endpoint-a" "endpoint-b"
        row = edgeRow e
    rowField "source_hash" row `shouldBe` Just (String (nodeIdHash (edgeSource e)))
    rowField "target_hash" row `shouldBe` Just (String (nodeIdHash (edgeTarget e)))

  it "relationship type is backtick-escaped, never a parameter" $ do
    let stmt = statementString (edgeBatchStatement "calls" [])
    "[r:`calls`]->" `T.isInfixOf` stmt `shouldBe` True
    "$rel" `T.isInfixOf` stmt `shouldBe` False
    "row.relation" `T.isInfixOf` stmt `shouldBe` False

  it "backticks in relationship types survive escaping" $ do
    let stmt = statementString (edgeBatchStatement "we`ird" [])
    "[r:`we``ird`]->" `T.isInfixOf` stmt `shouldBe` True

  it "statement values ride as JSON parameters, never interpolated" $ do
    let nasty = "nasty'\\\"`{}[]id"
        row   = nodeRow (mkNode nasty)
        stmt  = statementString (nodeBatchStatement [row])
    isJust (statementRows (nodeBatchStatement [row])) `shouldBe` True
    -- The id (with quotes/backslashes) must appear only in row params, never in the Cypher text.
    nasty `T.isInfixOf` stmt `shouldBe` False
    case statementRows (nodeBatchStatement [row]) of
      Just (r0 : _) -> rowField "id" r0 `shouldBe` Just (String nasty)
      _             -> expectationFailure "missing rows"

  it "nodes of a full graph push all precede edges and community data precedes nodes" $ do
    let g = mkGraphFrom [mkNode "a", mkNode "b"] [mkEdge Calls "a" "b"]
        commMap = Map.fromList [(1, ["a", "b"] :: [NodeId])]
        cohesionMap = Map.fromList [(1, 0.5)]
        cLabels = Map.empty
        stmts = map statementString (generateCommunityStatements g commMap cohesionMap cLabels
                                      ++ generateParameterizedStatements g)
    case stmts of
      (first : _) -> "UNWIND $rows AS row MERGE (c:Community" `T.isPrefixOf` first `shouldBe` True
      []          -> expectationFailure "no statements"
    any ("MERGE (n:Node" `T.isInfixOf`) stmts `shouldBe` True
    case reverse stmts of
      (last' : _) -> "MERGE (src)-[" `T.isInfixOf` last' `shouldBe` True
      []          -> expectationFailure "no statements"