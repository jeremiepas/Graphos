{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Export.IncrementalJSON
  ( IncrementalWriter
  , openWriter
  , closeWriter
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

import Data.Aeson (Value(..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map, empty)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Control.Exception (SomeException, catch, throwIO)
import System.Directory (removeFile)
import System.IO (hFlush, hClose, hPutStr)

import Graphos.Domain.Types
import qualified Graphos.Domain.Types.Writer as W
import Graphos.Infrastructure.FileSystem.AtomicWrite
  (openAtomicTemp, placeAtomicStreamed)

-- | Sanitize JSON bytes: replace invalid UTF-8 sequences with replacement char.
-- This prevents pipeline crashes when source files contain mixed encodings.
sanitizeUtf8 :: BSL.ByteString -> BSL.ByteString
sanitizeUtf8 bs =
  case TE.decodeUtf8' (BSL.toStrict bs) of
    Right _ -> bs  -- already valid UTF-8, pass through unchanged
    Left _  -> BSL.fromStrict (TE.encodeUtf8 (TE.decodeUtf8With TEE.lenientDecode (BSL.toStrict bs)))

-- | Open an incremental JSON writer that streams into a same-directory temp
-- file. The complete document is placed atomically at @path@ by 'closeWriter'
-- (fsync + rename), so an interrupted write never leaves a truncated graph.
openWriter :: FilePath -> IO W.IncrementalWriter
openWriter path = do
  (tmpPath, h) <- openAtomicTemp path
  firstRef <- newIORef True
  hPutStr h "{\n"
  let iw = W.IncrementalWriter
        { W.iwHandle = h
        , W.iwFirst = firstRef
        , W.iwTmp = tmpPath
        , W.iwTarget = path
        }
  writeKey iw "\"schema_version\""
  safePut iw (encode (graphFileSchemaVersion :: Text))
  pure iw

closeWriter :: W.IncrementalWriter -> IO ()
closeWriter iw = do
  hPutStr (W.iwHandle iw) "\n}\n"
  hFlush (W.iwHandle iw)
  hClose (W.iwHandle iw)
  let tmp = W.iwTmp iw
      tgt = W.iwTarget iw
  placeAtomicStreamed tmp tgt `catch` \(e :: SomeException) -> do
    removeFile tmp `catch` \(_ :: SomeException) -> pure ()
    throwIO e

flushWriter :: W.IncrementalWriter -> IO ()
flushWriter iw = hFlush (W.iwHandle iw)

writeKey :: W.IncrementalWriter -> String -> IO ()
writeKey iw key = do
  first <- readIORef (W.iwFirst iw)
  if first
    then do
      writeIORef (W.iwFirst iw) False
      hPutStr (W.iwHandle iw) $ "  " ++ key ++ ": "
    else do
      hPutStr (W.iwHandle iw) $ ",\n  " ++ key ++ ": "

safePut :: W.IncrementalWriter -> BSL.ByteString -> IO ()
safePut iw bs = BSL.hPut (W.iwHandle iw) (sanitizeUtf8 bs)

writeNodes :: W.IncrementalWriter -> [Node] -> IO ()
writeNodes iw nodes = do
  writeKey iw "\"nodes\""
  hPutStr (W.iwHandle iw) "[\n"
  case nodes of
    [] -> hPutStr (W.iwHandle iw) "]"
    (first:rest) -> do
      safePut iw ("    " <> encode first)
      mapM_ (\n -> do
        hPutStr (W.iwHandle iw) ",\n"
        safePut iw ("    " <> encode n)
        ) rest
      hPutStr (W.iwHandle iw) "\n  ]"

writeEdges :: W.IncrementalWriter -> [Edge] -> IO ()
writeEdges iw edges = do
  writeKey iw "\"edges\""
  hPutStr (W.iwHandle iw) "[\n"
  case edges of
    [] -> hPutStr (W.iwHandle iw) "]"
    (first:rest) -> do
      safePut iw ("    " <> encode first)
      mapM_ (\e -> do
        hPutStr (W.iwHandle iw) ",\n"
        safePut iw ("    " <> encode e)
        ) rest
      hPutStr (W.iwHandle iw) "\n  ]"

writeCommunities :: W.IncrementalWriter -> CommunityMap -> IO ()
writeCommunities iw commMap = do
  writeKey iw "\"communities\""
  safePut iw (encode commMap)

writeCohesion :: W.IncrementalWriter -> CohesionMap -> IO ()
writeCohesion iw cohMap = do
  writeKey iw "\"cohesion\""
  safePut iw (encode cohMap)

writeGodNodes :: W.IncrementalWriter -> [GodNode] -> IO ()
writeGodNodes iw gods = do
  writeKey iw "\"god_nodes\""
  safePut iw (encode gods)

writeAnalysisTail :: W.IncrementalWriter -> Maybe (Map Int Text) -> IO ()
writeAnalysisTail iw mLabels = do
  writeKey iw "\"community_labels\""
  safePut iw (encode (maybe empty id mLabels))

writeCommunityAggregates :: W.IncrementalWriter -> [CommunityAggregate] -> IO ()
writeCommunityAggregates iw aggregates = do
  writeKey iw "\"community_aggregates\""
  safePut iw (encode aggregates)

writeCompositions :: W.IncrementalWriter -> Maybe Value -> IO ()
writeCompositions iw mCompositions = do
  writeKey iw "\"compositions\""
  safePut iw (encode (maybe (Object KM.empty) id mCompositions))

writeEmbeddingsPath :: W.IncrementalWriter -> Maybe Text -> IO ()
writeEmbeddingsPath iw mp = case mp of
  Nothing -> pure ()
  Just p  -> do
    writeKey iw "\"embeddings_path\""
    safePut iw (encode p)