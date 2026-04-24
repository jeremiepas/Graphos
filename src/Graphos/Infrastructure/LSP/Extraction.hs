-- | LSP symbol extraction — document symbols, call hierarchy, workspace symbols,
-- and conversion to Graphos domain types (Node/Edge).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LSP.Extraction
  ( extractViaLSP
  , extractDocumentSymbols
  , extractCallHierarchy
  , extractWorkspaceSymbols
  , workspaceSymbolsToDocumentSymbols
  , symbolToNodes
  , symbolTreeToEdges
  ) where

import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Exception (catch, SomeException(..))
import Control.Monad (unless)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Graphos.Domain.Types
import Graphos.Infrastructure.LSP.Protocol hiding (languageIdFromExt)
import Graphos.Infrastructure.LSP.Transport
  ( LSPClient(..)
  , sendLSPMessageSafe
  , drainNotifications
  , readLSPResponseForId
  )
import Graphos.Infrastructure.LSP.ServerMap (languageIdFromExt, takeExtension)

-- ───────────────────────────────────────────────
-- Extraction via LSP
-- ───────────────────────────────────────────────

-- | Extract entities and relationships from a file using LSP.
-- Returns empty extraction on any error (never throws).
extractViaLSP :: LSPClient -> FilePath -> IO Extraction
extractViaLSP client filePath =
  catch (do
    putStrLn $ "[lsp] Extracting: " ++ filePath
    let ext = takeExtension filePath
        langId = languageIdFromExt ext

    fileContent <- catch (T.pack <$> readFile filePath) $ \(_ :: SomeException) -> pure ""

    let openMsg = lspDidOpen filePath langId fileContent
    sent <- sendLSPMessageSafe client openMsg
    unless sent $ putStrLn "[lsp] Warning: could not send didOpen (server disconnected?)"
    catch (drainNotifications (lspStdout client) 500000) $ \(_ :: SomeException) -> pure ()

    symbols <- extractDocumentSymbols client filePath
    putStrLn $ "[lsp] Got " ++ show (length symbols) ++ " symbols from " ++ filePath

    let closeMsg = lspDidClose filePath
    sentClose <- sendLSPMessageSafe client closeMsg
    unless sentClose $ putStrLn "[lsp] Warning: could not send didClose (server disconnected?)"

    let nodes = symbolToNodes filePath symbols
        edges = symbolTreeToEdges filePath symbols
    pure emptyExtraction
      { extractionNodes = nodes
      , extractionEdges = edges
      }
  ) $ \(e :: SomeException) -> do
    putStrLn $ "[lsp] Warning: extraction failed for " ++ filePath ++ ": " ++ show e
    pure emptyExtraction
      { extractionNodes = [makeStubNode filePath]
      , extractionEdges = []
      }

-- | Extract document symbols from a file.
-- Catches Broken pipe and other IO errors — returns [] instead of crashing.
extractDocumentSymbols :: LSPClient -> FilePath -> IO [DocumentSymbolResult]
extractDocumentSymbols client filePath = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let req = lspDocumentSymbolWithId filePath nextId
  sent <- sendLSPMessageSafe client req
  if not sent
    then do
      putStrLn $ "[lsp] Warning: could not send documentSymbol request for " ++ filePath ++ " (server disconnected?)"
      pure []
    else do
      -- 10s timeout per file: some servers are slow on large/complex files
      mResp <- timeout 10000000 (readLSPResponseForId (lspStdout client) nextId)
      case mResp of
        Nothing -> do
          putStrLn $ "[lsp] Timeout waiting for symbols: " ++ filePath
          pure []
        Just resp -> case resp of
          Left err -> do
            putStrLn $ "[lsp] Failed to get symbols: " ++ err
            pure []
          Right val -> pure $ parseSymbolsFromResponse val
  ) $ \(e :: SomeException) -> do
    putStrLn $ "[lsp] Warning: documentSymbol request failed for " ++ filePath ++ ": " ++ show e
    pure []

-- | Parse symbol tree from JSON-RPC response.
parseSymbolsFromResponse :: Value -> [DocumentSymbolResult]
parseSymbolsFromResponse (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> concatMap (flattenSymbols []) (V.toList arr)
    Just (Object obj) ->
      case KM.lookup "children" obj of
        Just (Array arr) -> concatMap (flattenSymbols []) (V.toList arr)
        _ -> [parseSingleSymbol obj]
    _ -> []
  where
    flattenSymbols :: [Text] -> Value -> [DocumentSymbolResult]
    flattenSymbols parents (Object s) =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          range = case KM.lookup "range" s of
            Just (Object r) -> parseRange r
            _ -> dummyRange
          childrenVals = case KM.lookup "children" s of
            Just (Array arr) -> V.toList arr
            _ -> []
      in if T.null name
         then concatMap (flattenSymbols parents) childrenVals
         else DocumentSymbolResult
              { dsrName = name
              , dsrKind = kind
              , dsrRange = range
              , dsrChildren = []
              }
               : concatMap (flattenSymbols (name : parents)) childrenVals
    flattenSymbols _ _ = []

    parseSingleSymbol s =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          range = case KM.lookup "range" s of
            Just (Object r) -> parseRange r
            _ -> dummyRange
      in DocumentSymbolResult { dsrName = name, dsrKind = kind, dsrRange = range, dsrChildren = [] }

    parseRange r =
      let start = case KM.lookup "start" r of
            Just (Object p) -> parsePos p
            _ -> Position 0 0
          end = case KM.lookup "end" r of
            Just (Object p) -> parsePos p
            _ -> Position 0 0
      in Range start end

    parsePos p =
      let line = case KM.lookup "line" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
          char = case KM.lookup "character" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
      in Position line char

    dummyRange = Range (Position 0 0) (Position 0 0)

parseSymbolsFromResponse _ = []

-- | Extract call hierarchy (incoming calls) for a symbol
extractCallHierarchy :: LSPClient -> Text -> IO [CallHierarchyItem]
extractCallHierarchy client _name = do
  _ <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) 1
  pure []  -- placeholder

-- ───────────────────────────────────────────────
-- Symbol → Node/Edge conversion
-- ───────────────────────────────────────────────

symbolToNodes :: FilePath -> [DocumentSymbolResult] -> [Node]
symbolToNodes filePath symbols =
  [ Node
    { nodeId           = makeNodeId filePath (safeLabel (dsrName sym))
    , nodeLabel        = safeLabel (dsrName sym)
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange sym))))
    , nodeLineEnd      = Just $ posLine (rangeEnd (dsrRange sym))
    , nodeKind         = Just $ symbolKindToText (dsrKind sym)
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }
  | sym <- symbols
  ]

-- | Sanitize a label for use in node IDs and display: strip newlines, quotes, backticks.
safeLabel :: Text -> Text
safeLabel = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`')

-- | Build edges from the symbol tree (parent contains child).
symbolTreeToEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
symbolTreeToEdges filePath flatSymbols =
  let fileEdges =
        [ Edge
          { edgeSource        = T.pack (takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath)
          , edgeTarget        = makeNodeId filePath (safeLabel (dsrName sym))
          , edgeRelation      = Contains
          , edgeConfidence    = Extracted
          , edgeConfidenceScore = 1.0
          , edgeSourceFile    = T.pack filePath
          , edgeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange sym))))
          , edgeWeight        = 1.0
          }
        | sym <- flatSymbols
        ]
      hierarchyEdges = buildHierarchyEdges filePath flatSymbols
  in fileEdges ++ hierarchyEdges

-- | Build parent→child Contains edges from the symbol hierarchy.
-- Uses sorted interval sweep: O(S log S) instead of O(S²).
-- Sort symbols by start position, then each symbol's parent is the nearest
-- preceding symbol whose range still contains it.
buildHierarchyEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
buildHierarchyEdges filePath symbols =
  let sorted = sortOn (\s -> (posLine (rangeStart (dsrRange s)), posCharacter (rangeStart (dsrRange s)))) symbols
      go _ [] = []
      go stack (sym:rest) =
        let startL = posLine (rangeStart (dsrRange sym))
            startC = posCharacter (rangeStart (dsrRange sym))
            endL   = posLine (rangeEnd (dsrRange sym))
            endC   = posCharacter (rangeEnd (dsrRange sym))
            startPos = startL * 10000 + startC
            endPos = endL * 10000 + endC
            newStack = popStack stack startPos
        in case newStack of
             (parent:ps) ->
               let pStart = posLine (rangeStart (dsrRange parent)) * 10000 + posCharacter (rangeStart (dsrRange parent))
               in if pStart < startPos && endPos <= posLine (rangeEnd (dsrRange parent)) * 10000 + posCharacter (rangeEnd (dsrRange parent))
                  then makeEdge parent sym : go ((sym : ps)) rest
                  else go (sym : ps) rest
             [] -> go [sym] rest

      popStack st startPos = dropWhile (\p -> posLine (rangeEnd (dsrRange p)) * 10000 + posCharacter (rangeEnd (dsrRange p)) <= startPos) st

      makeEdge parent child = Edge
        { edgeSource        = makeNodeId filePath (safeLabel (dsrName parent))
        , edgeTarget        = makeNodeId filePath (safeLabel (dsrName child))
        , edgeRelation      = Contains
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange parent))))
        , edgeWeight        = 1.0
        }
  in go [] sorted

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

makeNodeId :: FilePath -> Text -> NodeId
makeNodeId filePath name =
  let stem = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      safeName = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') name
  in hashPrefix <> T.pack "_" <> stem <> T.pack "_" <> safeName

-- | Create a stub node when LSP extraction fails
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
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Convert LSP SymbolKind integer to a human-readable text label.
symbolKindToText :: Int -> Text
symbolKindToText k = case k of
  1  -> "File"
  2  -> "Module"
  3  -> "Namespace"
  4  -> "Package"
  5  -> "Class"
  6  -> "Method"
  7  -> "Property"
  8  -> "Field"
  9  -> "Constructor"
  10 -> "Enum"
  11 -> "Interface"
  12 -> "Function"
  13 -> "Variable"
  14 -> "Constant"
  15 -> "String"
  16 -> "Number"
  17 -> "Boolean"
  18 -> "Array"
  19 -> "Object"
  20 -> "Key"
  21 -> "Null"
  22 -> "EnumMember"
  23 -> "Struct"
  24 -> "Event"
  25 -> "Operator"
  26 -> "TypeParameter"
  _  -> "Unknown"

-- ───────────────────────────────────────────────
-- Workspace symbol extraction
-- ───────────────────────────────────────────────

-- | Extract all symbols in the project using workspace/symbol.
extractWorkspaceSymbols :: LSPClient -> IO (Either Text [SymbolInformation])
extractWorkspaceSymbols client = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let req = lspWorkspaceSymbolWithId nextId ""
  sent <- sendLSPMessageSafe client req
  if not sent
    then pure $ Left $ T.pack "workspace/symbol failed: server disconnected"
    else do
      -- 30s timeout: workspace/symbol with empty query can take a long time
      -- on large projects (1000+ files) while tsserver indexes
      mResp <- timeout 30000000 (readLSPResponseForId (lspStdout client) nextId)
      case mResp of
        Nothing -> pure $ Left $ T.pack "workspace/symbol timed out (30s) — project too large or still indexing"
        Just resp -> case resp of
          Left err -> pure $ Left $ T.pack $ "workspace/symbol failed: " ++ err
          Right val -> pure $ Right $ parseWorkspaceSymbolResponse val
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "workspace/symbol error: " ++ show e

-- | Parse workspace/symbol response into SymbolInformation list
parseWorkspaceSymbolResponse :: Value -> [SymbolInformation]
parseWorkspaceSymbolResponse (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> mapMaybe parseSymInfo (V.toList arr)
    _ -> []
  where
    parseSymInfo (Object s) =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          loc = case KM.lookup "location" s of
            Just (Object l) -> parseLocation l
            _ -> Location "" (Range (Position 0 0) (Position 0 0))
      in if T.null name then Nothing
         else Just SymbolInformation { siName = name, siKind = kind, siLocation = loc }
    parseSymInfo _ = Nothing

    parseLocation l =
      let uri = case KM.lookup "uri" l of
            Just (Aeson.String u) -> T.drop 7 u
            _ -> ""
          range = case KM.lookup "range" l of
            Just (Object r) -> parseRange' r
            _ -> Range (Position 0 0) (Position 0 0)
      in Location uri range

    parseRange' r =
      let start = case KM.lookup "start" r of
            Just (Object p) -> parsePos' p
            _ -> Position 0 0
          end = case KM.lookup "end" r of
            Just (Object p) -> parsePos' p
            _ -> Position 0 0
      in Range start end

    parsePos' p =
      let line = case KM.lookup "line" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
          char = case KM.lookup "character" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
      in Position line char

parseWorkspaceSymbolResponse _ = []

-- | Convert workspace symbols to DocumentSymbolResult format
workspaceSymbolsToDocumentSymbols :: [SymbolInformation] -> Map FilePath [DocumentSymbolResult]
workspaceSymbolsToDocumentSymbols syms =
  Map.fromListWith (++)
    [ (T.unpack (locUri (siLocation sym))
    , [ DocumentSymbolResult
        { dsrName = siName sym
        , dsrKind = siKind sym
        , dsrRange = locRange (siLocation sym)
        , dsrChildren = []
        }
      ]
    )
  | sym <- syms
  , not (T.null (locUri (siLocation sym)))
  ]