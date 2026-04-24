-- | Core tree-sitter parsing — walks the AST using the Cursor API.
-- Clean approach: parse → root node → cursor walk → collect named nodes.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.TreeSitter.Core
  ( TSNodeInfo(..)
  , parseWithGrammar
  ) where

import Control.Exception (catch, SomeException(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca, mallocBytes, free)
import Foreign.Storable (peek, poke)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

import TreeSitter.Parser (withParser, withParseTree)
import TreeSitter.Tree (withRootNode, Tree)
import TreeSitter.Node (Node(..), TSPoint(..), nodeChildCount, nodeIsNamed, nodeType, nodeStartByte, nodeEndByte, nodeStartPoint, nodeEndPoint, ts_node_copy_child_nodes)
import TreeSitter.Language (Language)

-- | Simplified node info from tree-sitter AST.
data TSNodeInfo = TSNodeInfo
  { tsnType      :: String
  , tsnText      :: Text
  , tsnStartRow  :: Int
  , tsnStartCol  :: Int
  , tsnEndRow    :: Int
  , tsnEndCol    :: Int
  , tsnIsNamed   :: Bool
  , tsnChildren  :: [TSNodeInfo]
  } deriving (Eq, Show)

-- | Parse file content with a tree-sitter grammar.
-- Returns Nothing on failure, Just root children on success.
parseWithGrammar :: Ptr Language -> ByteString -> IO (Maybe [TSNodeInfo])
parseWithGrammar lang content = catch (do
  withParser lang $ \parser -> do
    withParseTree parser content $ \tree ->
      if tree == nullPtr
        then pure Nothing
        else Just <$> collectNodes tree content
  ) $ \(e :: SomeException) -> do
    hPutStrLn stderr $ "[tree-sitter] Parse error: " ++ show e
    pure Nothing

-- | Collect all named nodes from a parse tree using child buffer copying.
collectNodes :: Ptr Tree -> ByteString -> IO [TSNodeInfo]
collectNodes tree content =
  withRootNode tree $ \rootNodePtr -> do
    root <- peek rootNodePtr
    readNodeTree root content

-- | Read a node and all its named children recursively.
-- Uses child buffer copying (the idiomatic tree-sitter approach).
readNodeTree :: Node -> ByteString -> IO [TSNodeInfo]
readNodeTree node content = do
  childInfos <- readChildren node content
  pure [toNodeInfo node content childInfos]

-- | Read all children of a node (named and unnamed).
readChildren :: Node -> ByteString -> IO [TSNodeInfo]
readChildren node content = do
  let count = fromIntegral (nodeChildCount node) :: Int
  if count == 0
    then pure []
    else do
      -- Allocate buffer and copy all children into it
      buf <- mallocBytes (count * 80)  -- sizeof(Node) = 80
      alloca $ \tsNodeBuf -> do
        -- Copy the parent's TSNode to a buffer so we can pass Ptr TSNode
        poke tsNodeBuf (nodeTSNode node)
        ts_node_copy_child_nodes tsNodeBuf buf
      childNodes <- mapM (\i -> peek (buf `plusPtr` (i * 80) :: Ptr Node)) [0..count-1]
      free buf
      -- Recursively build info for each child
      mapM (\child -> do
        grandChildren <- readChildren child content
        pure (toNodeInfo child content grandChildren)
        ) childNodes

-- | Convert a tree-sitter Node to our TSNodeInfo.
toNodeInfo :: Node -> ByteString -> [TSNodeInfo] -> TSNodeInfo
toNodeInfo node content children = TSNodeInfo
  { tsnType = safeNodeType (nodeType node)
  , tsnText = extractText node content
  , tsnStartRow = fromIntegral (pointRow (nodeStartPoint node))
  , tsnStartCol = fromIntegral (pointColumn (nodeStartPoint node))
  , tsnEndRow = fromIntegral (pointRow (nodeEndPoint node))
  , tsnEndCol = fromIntegral (pointColumn (nodeEndPoint node))
  , tsnIsNamed = nodeIsNamed node /= 0
  , tsnChildren = children
  }

-- | Extract source text for a node.
extractText :: Node -> ByteString -> Text
extractText node content =
  let start = fromIntegral (nodeStartByte node)
      end = fromIntegral (nodeEndByte node)
  in if start >= 0 && end > start && end <= BS.length content
     then truncateText 200 (TE.decodeUtf8 (BS.take (end - start) (BS.drop start content)))
     else ""

-- | Safe CString peek.
safeNodeType :: Ptr a -> String
safeNodeType ptr
  | ptr == nullPtr = "unknown"
  | otherwise = unsafePerformIO $ catch (peekCString (castPtr ptr))
                                         (\(_ :: SomeException) -> pure "unknown")

-- | Truncate text for readability.
truncateText :: Int -> Text -> Text
truncateText maxLen t
  | T.length t <= maxLen = t
  | otherwise = T.take maxLen t <> "..."