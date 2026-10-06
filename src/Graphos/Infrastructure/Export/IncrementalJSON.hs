{-# LANGUAGE OverloadedStrings #-}
-- | Incremental (streaming) graph.json writer — JGF envelope edition.
--
-- The document is streamed key-by-key so the pipeline never holds the full
-- JSON AST in memory:
--
-- > { "graph": { "directed": true
-- >            , "type": "graphos.code-knowledge-graph"
-- >            , "nodes": { "<id>": {...} }
-- >            , "edges": [ {...} ]
-- >            , "metadata": { "graphos": { "schemaVersion": "1.0"
-- >                                       , "communities": {...}
-- >                                       , "cohesion": {...}
-- >                                       , "god_nodes": [...]
-- >                                       , "community_labels": {...}
-- >                                       , "community_aggregates": [...]
-- >                                       , "compositions": {...}
-- >                                       , "embeddings_path": "..." } } } }
--
-- @graph.metadata.graphos@ is opened lazily by the first analysis section
-- writer ('writeCommunities', 'writeCohesion', …) via 'writeGraphosKey';
-- @nodes@ / @edges@ stream at the @graph@ level via 'writeGraphKey' in the
-- JGF canonical shape (nodes object keyed by id, edges array).
-- 'closeWriter' terminates every open brace. The reader (UseCase/Load)
-- accepts this document as standard JGF.
module Graphos.Infrastructure.Export.IncrementalJSON
  ( IncrementalWriter
  , openWriter
  , closeWriter
  , abortWriter
  , flushWriter
  , writeNodes
  , writeEdges
  , writeCommunities
  , writeCohesion
  , writeGodNodes
  , writeAnalysisTail
  , writeCommunityAggregates
  , writeCompositions
  , writeEmbeddingsPath
  ) where

import Data.Aeson (Value(..), encode, object)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map, empty)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Vector as V
import System.IO (hFlush, hClose, hPutStr)

import Graphos.Domain.Types
import qualified Graphos.Domain.Types.Writer as W
import Graphos.Infrastructure.FileSystem.AtomicWrite
  ( commitAtomicHandle
  , discardAtomicHandle
  , openAtomicHandle
  )

-- | Sanitize JSON bytes: replace invalid UTF-8 sequences with replacement char.
-- This prevents pipeline crashes when source files contain mixed encodings.
sanitizeUtf8 :: BSL.ByteString -> BSL.ByteString
sanitizeUtf8 bs =
  case TE.decodeUtf8' (BSL.toStrict bs) of
    Right _ -> bs  -- already valid UTF-8, pass through unchanged
    Left _  -> BSL.fromStrict (TE.encodeUtf8 (TE.decodeUtf8With TEE.lenientDecode (BSL.toStrict bs)))

-- | Open an incremental JGF writer.
--
-- Emits @{ "graph": { "directed": true, "type": ... @, leaving the document
-- inside @graph@ so node/edge section writers add keys there. Analysis
-- sections nest one level deeper (@graph.metadata.graphos@) via
-- 'writeGraphosKey'.
openWriter :: FilePath -> IO W.IncrementalWriter
openWriter path = do
  -- Write to a temp file in the target's directory; the caller commits it
  -- atomically via 'closeWriter' (rename over the target only on success).
  (tmpPath, h) <- openAtomicHandle path
  firstRef <- newIORef True
  graphosRef <- newIORef False
  hPutStr h "{\n"
  let iw = W.IncrementalWriter { W.iwHandle = h, W.iwFirst = firstRef
                               , W.iwGraphosOpen = graphosRef
                               , W.iwTmpPath = Just tmpPath, W.iwTarget = Just path }
  writeKey iw "\"graph\""
  hPutStr (W.iwHandle iw) "{"
  writeIORef (W.iwFirst iw) True
  writeKey iw "\"directed\""
  safePut iw (encode True)
  writeKey iw "\"type\""
  safePut iw (encode jgfTypeText)
  pure iw

-- | @graph.type@ value for the streamed envelope (matches
-- 'Graphos.Domain.Types.JGF.jgfGraphType').
jgfTypeText :: Text
jgfTypeText = "graphos.code-knowledge-graph"

closeWriter :: W.IncrementalWriter -> IO ()
closeWriter iw = do
  graphosOpen <- readIORef (W.iwGraphosOpen iw)
  hPutStr (W.iwHandle iw) $
    (if graphosOpen then "\n    }" else "")    -- close graphos object
    <> "\n  }"                                  -- close metadata object
    <> "\n }\n}\n"                              -- close graph + document
  hFlush (W.iwHandle iw)
  case (W.iwTmpPath iw, W.iwTarget iw) of
    (Just tmpPath, Just target) -> commitAtomicHandle tmpPath target (W.iwHandle iw)
    _ -> hClose (W.iwHandle iw)

-- | Abort an in-flight incremental write: close the handle and remove the
-- temp file, leaving the target untouched. Safe to call after 'closeWriter'
-- (the temp path is cleared once committed).
abortWriter :: W.IncrementalWriter -> IO ()
abortWriter iw = case (W.iwTmpPath iw, W.iwTarget iw) of
  (Just tmpPath, Just _) -> do
    discardAtomicHandle tmpPath (W.iwHandle iw)
    pure ()
  _ -> hClose (W.iwHandle iw)

flushWriter :: W.IncrementalWriter -> IO ()
flushWriter iw = hFlush (W.iwHandle iw)

-- | Write the next key at the current nesting level. Keys are
-- comma-separated; the separator state lives in @iwFirst@.
writeKey :: W.IncrementalWriter -> String -> IO ()
writeKey iw key = do
  first <- readIORef (W.iwFirst iw)
  if first
    then do
      writeIORef (W.iwFirst iw) False
      hPutStr (W.iwHandle iw) $ "\n  " ++ key ++ ": "
    else do
      hPutStr (W.iwHandle iw) $ ",\n  " ++ key ++ ": "

safePut :: W.IncrementalWriter -> BSL.ByteString -> IO ()
safePut iw bs = BSL.hPut (W.iwHandle iw) (sanitizeUtf8 bs)

-- | Stream the @nodes@ section: a JGF object keyed by node id.
writeNodes :: W.IncrementalWriter -> [Node] -> IO ()
writeNodes iw nodes = do
  writeKey iw "\"nodes\""
  safePut iw (encode (jgfNodesObject nodes))

-- | Stream the @edges@ section: a JGF array of edge objects.
writeEdges :: W.IncrementalWriter -> [Edge] -> IO ()
writeEdges iw edges = do
  writeKey iw "\"edges\""
  safePut iw (encode (Array (V.fromList (map edgeToJGF edges))))

-- | Write the next key inside @graph.metadata.graphos@, opening the
-- @metadata@ + @graphos@ objects (with the @schemaVersion@ record) on first
-- use.
writeGraphosKey :: W.IncrementalWriter -> String -> IO ()
writeGraphosKey iw key = do
  opened <- readIORef (W.iwGraphosOpen iw)
  if opened
    then writeKey iw key
    else do
      writeIORef (W.iwGraphosOpen iw) True
      writeKey iw "\"metadata\""
      hPutStr (W.iwHandle iw) "{"
      writeIORef (W.iwFirst iw) True
      writeKey iw "\"graphos\""
      hPutStr (W.iwHandle iw) "{"
      writeIORef (W.iwFirst iw) True
      writeKey iw "\"schemaVersion\""
      safePut iw (encode jgfSchemaVersionText)
      writeKey iw key

-- | @graph.metadata.graphos.schemaVersion@ for the streamed envelope
-- (matches 'Graphos.Domain.Types.JGF.jgfSchemaVersion').
jgfSchemaVersionText :: Text
jgfSchemaVersionText = "1.0"

writeCommunities :: W.IncrementalWriter -> CommunityMap -> IO ()
writeCommunities iw commMap = do
  writeGraphosKey iw "\"communities\""
  safePut iw (encode commMap)

writeCohesion :: W.IncrementalWriter -> CohesionMap -> IO ()
writeCohesion iw cohMap = do
  writeGraphosKey iw "\"cohesion\""
  safePut iw (encode cohMap)

writeGodNodes :: W.IncrementalWriter -> [GodNode] -> IO ()
writeGodNodes iw gods = do
  writeGraphosKey iw "\"god_nodes\""
  safePut iw (encode gods)

writeAnalysisTail :: W.IncrementalWriter -> Maybe (Map Int Text) -> IO ()
writeAnalysisTail iw mLabels = do
  writeGraphosKey iw "\"community_labels\""
  safePut iw (encode (maybe empty id mLabels))

writeCommunityAggregates :: W.IncrementalWriter -> [CommunityAggregate] -> IO ()
writeCommunityAggregates iw aggregates = do
  writeGraphosKey iw "\"community_aggregates\""
  safePut iw (encode aggregates)

writeCompositions :: W.IncrementalWriter -> Maybe Value -> IO ()
writeCompositions iw mCompositions = do
  writeGraphosKey iw "\"compositions\""
  safePut iw (encode (maybe (object []) id mCompositions))

writeEmbeddingsPath :: W.IncrementalWriter -> Maybe Text -> IO ()
writeEmbeddingsPath iw mp = case mp of
  Nothing -> pure ()
  Just p  -> do
    writeGraphosKey iw "\"embeddings_path\""
    safePut iw (encode p)