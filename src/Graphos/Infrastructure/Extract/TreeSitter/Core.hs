-- | Core tree-sitter parsing — walks the AST using the Cursor API.
-- Clean approach: parse → root node → cursor walk → collect named nodes.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NumericUnderscores #-}
module Graphos.Infrastructure.Extract.TreeSitter.Core
  ( TSNodeInfo(..)
  , parseWithGrammar
  , defaultTruncationBudget
  , nodeIdentityBudget
  , normalizeText
  , truncateText
  , truncateWithElision
   , SourceFile(..)
   , mkSourceFile
   , sourceDirHash
   , sourceStem
   , sourceFilePathStr
   , extractIdentifier
   ) where

import Control.Exception (catch, SomeException(..), bracket)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word (Word64)
import Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca, mallocBytes, free)
import Foreign.Storable (peek, poke)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import TreeSitter.Parser (Parser, withParser, ts_parser_set_timeout_micros)
import TreeSitter.Tree (withRootNode, Tree, ts_tree_delete)
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

-- | Interruptible FFI binding to tree-sitter's parse function so that
-- Haskell async exceptions can abort a pathological parse.
foreign import ccall interruptible "ts_parser_parse_string"
  ts_parser_parse_string_interruptible :: Ptr Parser -> Ptr Tree -> CString -> Int -> IO (Ptr Tree)

-- | Parse file content with a tree-sitter grammar.
-- Returns Nothing on failure, Just root children on success.
parseWithGrammar :: Ptr Language -> ByteString -> IO (Maybe [TSNodeInfo])
parseWithGrammar lang content = catch (do
  withParser lang $ \parser -> do
    ts_parser_set_timeout_micros parser (5000000 :: Word64)
    result <- timeout 6000000 $ unsafeUseAsCStringLen content $ \(source, len) ->
      bracket
        (ts_parser_parse_string_interruptible parser nullPtr source len)
        (\tree -> unless (tree == nullPtr) $ void $ ts_tree_delete tree)
        $ \tree ->
          if tree == nullPtr
            then pure Nothing
            else Just <$> collectNodes tree content
    case result of
      Nothing -> do
        hPutStrLn stderr "[tree-sitter] Parse timed out"
        pure Nothing
      Just r  -> pure r
  ) $ \(e :: SomeException) -> do
    hPutStrLn stderr $ "[tree-sitter] Parse error: " ++ show e
    pure Nothing

-- | Maximum recursion depth for AST walk.
maxDepth :: Int
maxDepth = 256

-- | Collect all named nodes from a parse tree using child buffer copying.
collectNodes :: Ptr Tree -> ByteString -> IO [TSNodeInfo]
collectNodes tree content =
  withRootNode tree $ \rootNodePtr -> do
    root <- peek rootNodePtr
    readNodeTree 0 root content

-- | Read a node and all its named children recursively.
-- Uses child buffer copying (the idiomatic tree-sitter approach).
readNodeTree :: Int -> Node -> ByteString -> IO [TSNodeInfo]
readNodeTree depth node content = do
  childInfos <- readChildren depth node content
  pure [toNodeInfo node content childInfos]

-- | Read all children of a node (named and unnamed).
readChildren :: Int -> Node -> ByteString -> IO [TSNodeInfo]
readChildren depth node content
  | depth >= maxDepth = pure []
  | otherwise = do
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
            grandChildren <- readChildren (depth + 1) child content
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
      raw = BS.take (end - start) (BS.drop start content)
  in if start >= 0 && end > start && end <= BS.length content
     then TE.decodeUtf8With TEE.lenientDecode raw
     else ""

-- | Safe CString peek.
safeNodeType :: Ptr a -> String
safeNodeType ptr
  | ptr == nullPtr = "unknown"
  | otherwise = unsafePerformIO $ catch (peekCString (castPtr ptr))
                                         (\(_ :: SomeException) -> pure "unknown")

-- | Default truncation budget for extracted text.
defaultTruncationBudget :: Int
defaultTruncationBudget = 200

-- | Budget for node identity under AVI-1452 (bounded node ids). Shared by the
-- id's <ident>, the node label, and the node signature so they stay consistent.
-- Capped at 60 (not the 200 readability budget). Both 'nodeLabel' and
-- 'nodeSignature' live in a 'ShortText', whose 63-byte ceiling comfortably holds
-- a 60-byte identity; callers that run the identity through 'truncateText'
-- (which appends a 3-char elision "...") reserve that space so the final
-- identity stays <= nodeIdentityBudget. The 512-byte per-node graph.json bound
-- (task 3.3 criterion e) is satisfied with large headroom.
nodeIdentityBudget :: Int
nodeIdentityBudget = 60

-- | Normalize text by collapsing whitespace.
normalizeText :: Text -> Text
normalizeText = T.unwords . T.words

-- | Truncate text for readability.
truncateText :: Int -> Text -> Text
truncateText maxLen t
  | T.length t <= maxLen = t
  | otherwise = T.take maxLen t <> "..."


-- | Truncate text with middle-elision for specific patterns.
-- The second argument is the 'tail' to preserve.
truncateWithElision :: Int -> Text -> Text -> Text
truncateWithElision maxLen t tailText
  | T.length t <= maxLen = t
  | otherwise = 
      let headLen = max 0 (maxLen - T.length tailText - 3)
      in T.take headLen t <> "..." <> tailText

-- | Logical source file: the parsed file path plus its module name.
-- Used to (a) derive a stable id prefix and (b) label the module node.
data SourceFile = SourceFile
  { sourceFilePath  :: Text
  , sourceModuleName :: Text }

mkSourceFile :: FilePath -> Text -> SourceFile
mkSourceFile fp moduleName = SourceFile (T.pack fp) moduleName

-- | Stable directory hash for a file path.
-- Derives from the directory portion so ids are stable across repos that
-- share a relative layout, while still disambiguating same-named files in
-- different directories. Mirrors the hash used by the LSP exporter.
sourceDirHash :: SourceFile -> Text
sourceDirHash (SourceFile p _) =
  let dirPart = T.reverse (T.dropWhile (/= '/') (T.reverse p))
      h = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) dirPart `mod` 65536)
  in T.pack $ show h

-- | File stem without extension, used as the id prefix.
sourceStem :: SourceFile -> Text
sourceStem (SourceFile p _) =
  T.pack (takeBaseName (T.unpack p))

-- | The parsed file path as a plain FilePath.
sourceFilePathStr :: SourceFile -> FilePath
sourceFilePathStr (SourceFile p _) = T.unpack p

-- | The identifier text for a node: the first named child whose type is one
-- of the language-neutral identifier forms, or — for the few declarations
-- that name themselves otherwise — a fallback keyed on the declaration type,
-- so that a `qualified_name` or a nested `identifier` is never mistaken for
-- the node's own name where it is really a reference. Returns 'Nothing' when
-- none exists, which callers use to suppress identifier-less wrapper nodes.
extractIdentifier :: TSNodeInfo -> Maybe Text
extractIdentifier info =
  case pick identifierTypes (tsnChildren info) of
    Just t  -> Just t
    Nothing -> case tsnType info of
      -- C# `namespace A.B { }` names itself with a `qualified_name`; a plain
      -- `namespace A { }` uses an `identifier`.
      "namespace_declaration" -> pick ["qualified_name"] (tsnChildren info)
      -- C# / Java `public Foo.Bar n;` keeps the name in the declarator —
      -- the other children are the modifiers and the type, so an untargeted
      -- descendant scan would return the type (`Foo`) instead. C# nests one
      -- level deeper than Java: field_declaration > variable_declaration >
      -- variable_declarator.
      t | t `elem` ["field_declaration", "variable_declaration"] ->
            pick identifierTypes
              (concatMap tsnChildren (declarators (tsnChildren info)))
      _ -> Nothing
  where
    pick tys cs =
      case filter (\c -> tsnIsNamed c && tsnType c `elem` tys) cs of
        (c : _) -> Just (tsnText c)
        []      -> Nothing
    -- `variable_declarator`s among @cs@ or their direct children.
    declarators cs =
      [ d | c <- cs
          , d <- if isDeclarator c then [c] else filter isDeclarator (tsnChildren c) ]
    isDeclarator = (== "variable_declarator") . tsnType
    identifierTypes = [ "identifier", "type_identifier", "property_identifier"
                      , "field_identifier", "name"
                      -- Perl `package Shop::Cart;` and PHP `namespace Shop;`
                      -- name themselves with a dedicated node type. Without
                      -- these the declaration gets an empty identifier, hence
                      -- an empty node id — and every such node in the corpus
                      -- collides on that id when the per-file extractions are
                      -- merged (AVI-1765).
                      , "package_name", "namespace_name" ]

