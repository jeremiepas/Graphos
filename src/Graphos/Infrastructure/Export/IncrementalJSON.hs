{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO (IOMode(..), hFlush, hClose, openFile, hPutStr)

import Graphos.Domain.Types
import qualified Graphos.Domain.Types.Writer as W

openWriter :: FilePath -> IO W.IncrementalWriter
openWriter path = do
  h <- openFile path WriteMode
  firstRef <- newIORef True
  hPutStr h "{\n"
  pure W.IncrementalWriter { W.iwHandle = h, W.iwFirst = firstRef }

closeWriter :: W.IncrementalWriter -> IO ()
closeWriter iw = do
  hPutStr (W.iwHandle iw) "\n}\n"
  hFlush (W.iwHandle iw)
  hClose (W.iwHandle iw)

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

writeNodes :: W.IncrementalWriter -> [Node] -> IO ()
writeNodes iw nodes = do
  writeKey iw "\"nodes\""
  hPutStr (W.iwHandle iw) "[\n"
  case nodes of
    [] -> hPutStr (W.iwHandle iw) "]"
    (first:rest) -> do
      BSL.hPut (W.iwHandle iw) ("    " <> encode first)
      mapM_ (\n -> do
        hPutStr (W.iwHandle iw) ",\n"
        BSL.hPut (W.iwHandle iw) ("    " <> encode n)
        ) rest
      hPutStr (W.iwHandle iw) "\n  ]"

writeEdges :: W.IncrementalWriter -> [Edge] -> IO ()
writeEdges iw edges = do
  writeKey iw "\"edges\""
  hPutStr (W.iwHandle iw) "[\n"
  case edges of
    [] -> hPutStr (W.iwHandle iw) "]"
    (first:rest) -> do
      BSL.hPut (W.iwHandle iw) ("    " <> encode first)
      mapM_ (\e -> do
        hPutStr (W.iwHandle iw) ",\n"
        BSL.hPut (W.iwHandle iw) ("    " <> encode e)
        ) rest
      hPutStr (W.iwHandle iw) "\n  ]"

writeCommunities :: W.IncrementalWriter -> CommunityMap -> IO ()
writeCommunities iw commMap = do
  writeKey iw "\"communities\""
  BSL.hPut (W.iwHandle iw) (encode commMap)

writeCohesion :: W.IncrementalWriter -> CohesionMap -> IO ()
writeCohesion iw cohMap = do
  writeKey iw "\"cohesion\""
  BSL.hPut (W.iwHandle iw) (encode cohMap)

writeGodNodes :: W.IncrementalWriter -> [GodNode] -> IO ()
writeGodNodes iw gods = do
  writeKey iw "\"god_nodes\""
  BSL.hPut (W.iwHandle iw) (encode gods)

writeAnalysisTail :: W.IncrementalWriter -> Maybe (Map Int Text) -> IO ()
writeAnalysisTail iw mLabels = do
  case mLabels of
    Just labels -> do
      writeKey iw "\"community_labels\""
      BSL.hPut (W.iwHandle iw) (encode labels)
    Nothing -> pure ()

writeCommunityAggregates :: W.IncrementalWriter -> [CommunityAggregate] -> IO ()
writeCommunityAggregates iw aggregates = do
  writeKey iw "\"community_aggregates\""
  BSL.hPut (W.iwHandle iw) (encode aggregates)

writeCompositions :: W.IncrementalWriter -> Maybe Value -> IO ()
writeCompositions iw mCompositions = case mCompositions of
  Just comps -> do
    writeKey iw "\"compositions\""
    BSL.hPut (W.iwHandle iw) (encode comps)
  Nothing -> pure ()