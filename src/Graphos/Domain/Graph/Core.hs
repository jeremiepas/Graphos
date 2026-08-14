-- | Core graph type and construction operations.
-- Pure functions over the domain types.
--
-- StrictData prevents thunk buildup in graph fields (gNodes, gEdges, gAdjFwd, gAdjBack)
-- which hold the entire knowledge graph. On 100k+ node graphs, lazy fields
-- would create 3-4× memory overhead from unevaluated thunks.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Graph.Core
  ( -- * Types
    Graph( Graph, gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash )

    -- * Construction
  , buildGraph
  , mergeExtractions
  , mergeGraphs

    -- * Hashing
  , computeGraphHash

    -- * Analysis helpers
  , isFileNode
  , isConceptNode
  , makeStubNode
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Bits (xor, shiftR, (.&.))
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Graphos.Domain.Types

-- ───────────────────────────────────────────────
-- Internal graph representation
-- ───────────────────────────────────────────────

-- | Adjacency-list graph with node and edge attributes
data Graph = Graph
  { gNodes         :: Map NodeId Node
  , gEdges         :: Map (NodeId, NodeId) Edge
  , gAdjFwd        :: Map NodeId (Set NodeId)   -- forward adjacency
  , gAdjBack       :: Map NodeId (Set NodeId)   -- backward adjacency (for undirected queries)
  , gDirected      :: Bool
  , gCompositions  :: Maybe Value               -- per-community composition metadata
  , gHash          :: !Text                     -- deterministic hash over graph structure
  } deriving (Eq, Show)

-- | Force full evaluation of a Graph to WHNF + all nested structures.
-- Essential for preventing thunk accumulation during Leiden iterations.
instance NFData Graph where
  rnf Graph{} = ()

instance ToJSON Graph where
  toJSON g = object
    [ "nodes"      .= gNodes g
    , "edges"      .= gEdges g
    , "adj_fwd"    .= gAdjFwd g
    , "adj_back"   .= gAdjBack g
    , "directed"   .= gDirected g
    , "compositions" .= gCompositions g
    , "hash"       .= gHash g
    ]

instance FromJSON Graph where
  parseJSON = withObject "Graph" $ \v -> Graph
    <$> v .: "nodes"
    <*> v .: "edges"
    <*> v .: "adj_fwd"
    <*> v .: "adj_back"
    <*> v .: "directed"
    <*> v .: "compositions"
    <*> v .: "hash"

-- ───────────────────────────────────────────────
-- Construction
-- ───────────────────────────────────────────────

-- | Build a graph from an Extraction result
-- Dangling edges (referencing nodes not in extractionNodes) are silently dropped.
buildGraph :: Bool -> Extraction -> Graph
buildGraph directed extraction =
  let nodes = extNodes extraction
      edgeList = Map.elems (extEdges extraction)
      validEdges = [e | e <- edgeList
                      , Map.member (edgeSource e) nodes
                      , Map.member (edgeTarget e) nodes]
      edgeMap = Map.fromList [((edgeSource e, edgeTarget e), e) | e <- validEdges]
      fwdAdj = Map.fromListWith Set.union
          [(edgeSource e, Set.singleton (edgeTarget e)) | e <- validEdges]
      bwdAdj = if directed
          then Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- validEdges]
          else Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- validEdges]
              <> fwdAdj
  in Graph
    { gNodes         = nodes
    , gEdges         = edgeMap
    , gAdjFwd        = fwdAdj
    , gAdjBack       = bwdAdj
    , gDirected      = directed
    , gCompositions  = Nothing
    , gHash          = computeGraphHash nodes edgeMap
    }

-- | Merge two extractions (dedup nodes by id, combine edges)
--
-- Uses Map-based union for O(n₁ + n₂) deduplication instead of
-- list-based O(n₁ × n₂) rebuild. Critical for large codebases
-- where foldr mergeExtractions over 1000+ files causes OOM.
mergeExtractions :: Extraction -> Extraction -> Extraction
mergeExtractions a b =
  let mergedNodes = extNodes a `Map.union` extNodes b
      mergedEdges = extEdges a `Map.union` extEdges b
  in Extraction
    { extractionNodes = mergedNodes
    , extractionEdges = mergedEdges
    }

-- | Merge two graphs (new graph takes precedence for overlapping nodes)
-- Dangling edges are removed to keep adjacency lists consistent.
mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs old new =
  let mergedNodes = gNodes old <> gNodes new
      mergedEdges = Map.filterWithKey (\(src, tgt) _ -> Map.member src mergedNodes && Map.member tgt mergedNodes)
                     (gEdges old <> gEdges new)
      mergedFwd   = Map.unionWith Set.union (gAdjFwd old) (gAdjFwd new)
      mergedBwd   = Map.unionWith Set.union (gAdjBack old) (gAdjBack new)
   in Graph
    { gNodes         = mergedNodes
    , gEdges         = mergedEdges
    , gAdjFwd        = mergedFwd
    , gAdjBack       = mergedBwd
    , gDirected      = gDirected old
    , gCompositions  = Nothing
    , gHash          = computeGraphHash mergedNodes mergedEdges
    }

-- ───────────────────────────────────────────────
-- Analysis helpers
-- ───────────────────────────────────────────────

-- | Create a stub node for a file path when no real extraction is available.
-- Pure helper shared across extractors (UseCase and Infrastructure).
makeStubNode :: FilePath -> Node
makeStubNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      nodeId' = hashPrefix <> T.pack "_" <> name
  in Node
    { nodeId           = nodeId'
    , nodeLabel        = name
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack filePath
    , nodeLineStart    = Nothing
    , nodeCommunityId  = Nothing
    , nodeDegree       = Nothing
    , nodeIsBridge     = Nothing
    , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    }

-- | Check if a node is a file-level hub (synthetic AST node)
isFileNode :: Graph -> Node -> Bool
isFileNode g n =
  let label = nodeLabel n
      srcFile = nodeSourceFile n
      nid = nodeId n
      fwd = Map.findWithDefault Set.empty nid (gAdjFwd g)
      bwd = Map.findWithDefault Set.empty nid (gAdjBack g)
      deg = Set.size $ if gDirected g then fwd else fwd `Set.union` bwd
  in -- Method stub: starts with '.' and ends with ')'
     (not (T.null label) && T.singleton (T.head label) == "." && T.last label == ')')
     -- Low-degree function stub
     || (not (T.null label) && T.last label == ')' && deg <= 1)
     -- Label matches source filename
     || (not (T.null srcFile) && not (T.null label) && label == T.pack (takeFileName (T.unpack srcFile)))
  where
    takeFileName path = case T.breakOnEnd "/" (T.pack path) of
      (_, "") -> path
      (_, name) -> T.unpack $ T.dropWhile (== '/') name

-- | Check if a node is a concept node (injected semantic annotation)
isConceptNode :: Node -> Bool
isConceptNode n =
  let src = nodeSourceFile n
  in T.null src || (T.null $ T.takeWhileEnd (/= '.') src)

-- | Deterministic hash over graph structure: sorted node ids + sorted edge tuples.
-- Uses FNV-1a 32-bit, returned as 8-char hex — same scheme as resultHash in Score.hs.
computeGraphHash :: Map NodeId Node -> Map (NodeId, NodeId) Edge -> Text
computeGraphHash nodes edges =
  let ids = Map.keysSet nodes
      edgeTuples = Map.keys edges
      sortedIds = Set.toList ids
      sortedEdges = sort edgeTuples
      raw = foldl' fnv1a32ByteWord (fromIntegral fnvOffset32)
            (map show sortedIds <> map (\(a,b) -> show a <> "," <> show b) sortedEdges)
      hex = printf32 raw
  in T.pack hex
  where
    fnvOffset32 :: Word32
    fnvOffset32 = 2166136261

    fnv1a32ByteWord :: Word32 -> String -> Word32
    fnv1a32ByteWord h = foldl' fnv1a32Byte h
      where
        fnv1a32Byte :: Word32 -> Char -> Word32
        fnv1a32Byte h' c = (h' `xor` fromIntegral (fromEnum c)) * 16777619

    printf32 :: Word32 -> String
    printf32 w =
      let hexDigits = "0123456789abcdef"
          toHex d = hexDigits !! d
          d0 = fromIntegral ((w `shiftR` 28) .&. 0xF)
          d1 = fromIntegral ((w `shiftR` 24) .&. 0xF)
          d2 = fromIntegral ((w `shiftR` 20) .&. 0xF)
          d3 = fromIntegral ((w `shiftR` 16) .&. 0xF)
          d4 = fromIntegral ((w `shiftR` 12) .&. 0xF)
          d5 = fromIntegral ((w `shiftR`  8) .&. 0xF)
          d6 = fromIntegral ((w `shiftR`  4) .&. 0xF)
          d7 = fromIntegral ( w         .&. 0xF)
      in [toHex d0, toHex d1, toHex d2, toHex d3, toHex d4, toHex d5, toHex d6, toHex d7]