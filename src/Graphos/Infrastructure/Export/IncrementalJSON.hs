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
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO (Handle, IOMode(..), hFlush, hClose, openFile, hPutStr)

import Graphos.Domain.Types

data IncrementalWriter = IncrementalWriter
  { iwHandle    :: Handle
  , iwFirst     :: IORef Bool
  }

openWriter :: FilePath -> IO IncrementalWriter
openWriter path = do
  h <- openFile path WriteMode
  firstRef <- newIORef True
  hPutStr h "{\n"
  pure IncrementalWriter { iwHandle = h, iwFirst = firstRef }

closeWriter :: IncrementalWriter -> IO ()
closeWriter iw = do
  hPutStr (iwHandle iw) "\n}\n"
  hFlush (iwHandle iw)
  hClose (iwHandle iw)

flushWriter :: IncrementalWriter -> IO ()
flushWriter iw = hFlush (iwHandle iw)

writeKey :: IncrementalWriter -> String -> IO ()
writeKey iw key = do
  first <- readIORef (iwFirst iw)
  if first
    then do
      writeIORef (iwFirst iw) False
      hPutStr (iwHandle iw) $ "  " ++ key ++ ": "
    else do
      hPutStr (iwHandle iw) $ ",\n  " ++ key ++ ": "

writeNodes :: IncrementalWriter -> [Node] -> IO ()
writeNodes iw nodes = do
  writeKey iw "\"nodes\""
  hPutStr (iwHandle iw) "[\n"
  case nodes of
    [] -> hPutStr (iwHandle iw) "]"
    (first:rest) -> do
      BSL.hPut (iwHandle iw) ("    " <> encode first)
      mapM_ (\n -> do
        hPutStr (iwHandle iw) ",\n"
        BSL.hPut (iwHandle iw) ("    " <> encode n)
        ) rest
      hPutStr (iwHandle iw) "\n  ]"

writeEdges :: IncrementalWriter -> [Edge] -> IO ()
writeEdges iw edges = do
  writeKey iw "\"edges\""
  hPutStr (iwHandle iw) "[\n"
  case edges of
    [] -> hPutStr (iwHandle iw) "]"
    (first:rest) -> do
      BSL.hPut (iwHandle iw) ("    " <> encode first)
      mapM_ (\e -> do
        hPutStr (iwHandle iw) ",\n"
        BSL.hPut (iwHandle iw) ("    " <> encode e)
        ) rest
      hPutStr (iwHandle iw) "\n  ]"

writeCommunities :: IncrementalWriter -> CommunityMap -> IO ()
writeCommunities iw commMap = do
  writeKey iw "\"communities\""
  BSL.hPut (iwHandle iw) (encode commMap)

writeCohesion :: IncrementalWriter -> CohesionMap -> IO ()
writeCohesion iw cohMap = do
  writeKey iw "\"cohesion\""
  BSL.hPut (iwHandle iw) (encode cohMap)

writeGodNodes :: IncrementalWriter -> [GodNode] -> IO ()
writeGodNodes iw gods = do
  writeKey iw "\"god_nodes\""
  BSL.hPut (iwHandle iw) (encode gods)

writeAnalysisTail :: IncrementalWriter -> Maybe (Map Int Text) -> IO ()
writeAnalysisTail iw mLabels = do
  case mLabels of
    Just labels -> do
      writeKey iw "\"community_labels\""
      BSL.hPut (iwHandle iw) (encode labels)
    Nothing -> pure ()