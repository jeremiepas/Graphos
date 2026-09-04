-- | LSP symbol extraction — document symbols, call hierarchy, workspace symbols,
-- and conversion to Graphos domain types (Node/Edge).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LSP.Extraction
  ( extractViaLSP
  , extractDocumentSymbols
  , extractCallHierarchy
  , extractReferences
  , extractWorkspaceSymbols
  , workspaceSymbolsToDocumentSymbols
  , symbolToNodes
  , symbolTreeToEdges
  ) where

import Control.Concurrent.MVar (takeMVar, putMVar)
import Control.Exception (catch, SomeException(..), evaluate)
import Control.Monad (unless)
import Data.Aeson (Value(..))
import Data.Bits ((.|.))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import qualified Data.Text.IO as TIO
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

    -- Read file content strictly to avoid holding the entire file as a lazy
    -- thunk chain. For 10k+ files this accumulates megabytes of lazy String
    -- chunks that can't be GC'd until fully consumed.
    fileContent <- catch (TIO.readFile filePath >>= evaluate) $ \(_ :: SomeException) -> pure ""

    let openMsg = lspDidOpen filePath langId fileContent
    sent <- sendLSPMessageSafe client openMsg
    unless sent $ putStrLn "[lsp] Warning: could not send didOpen (server disconnected?)"
    catch (drainNotifications (lspStdout client) 500000) $ \(_ :: SomeException) -> pure ()

    symbols <- extractDocumentSymbols client filePath
    putStrLn $ "[lsp] Got " ++ show (length symbols) ++ " symbols from " ++ filePath

    -- Extract reference edges (textDocument/references) for top symbols
    let hasRefs = scpReferencesProvider (lspServerCaps client)
    refEdges <- if hasRefs
      then extractReferences client filePath symbols
      else pure []

    -- Extract call hierarchy edges if supported
    let hasCallHierarchy = scpCallHierarchyProvider (lspServerCaps client)
    callEdges <- if hasCallHierarchy
      then extractCallHierarchy client filePath symbols
      else pure []

    let closeMsg = lspDidClose filePath
    sentClose <- sendLSPMessageSafe client closeMsg
    unless sentClose $ putStrLn "[lsp] Warning: could not send didClose (server disconnected?)"

    let nodes = symbolToNodes filePath symbols
        edges = symbolTreeToEdges filePath symbols ++ refEdges ++ callEdges
    pure (extractionFromLists nodes edges)
  ) $ \(e :: SomeException) -> do
    putStrLn $ "[lsp] Warning: extraction failed for " ++ filePath ++ ": " ++ show e
    pure (extractionFromLists [makeStubNode filePath] [])

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

-- ───────────────────────────────────────────────
-- Reference extraction
-- ───────────────────────────────────────────────

-- | Symbol kind priority for reference extraction:
--   Class(5) > Method(6) > Function(12) > Constructor(9) > Interface(11) > others
symbolKindPriority :: Int -> Int
symbolKindPriority k = case k of
  5  -> 0   -- Class
  6  -> 1   -- Method
  12 -> 2   -- Function
  9  -> 3   -- Constructor
  11 -> 4   -- Interface
  23 -> 5   -- Struct
  8  -> 6   -- Field
  13 -> 7   -- Variable
  _  -> 8   -- Everything else

-- | Extract reference edges by sending textDocument/references for top symbols.
-- Limits to 10 symbols per file (sorted by kind priority) to keep extraction fast.
extractReferences :: LSPClient -> FilePath -> [DocumentSymbolResult] -> IO [Edge]
extractReferences client filePath symbols = catch (do
  let sorted = sortOn (symbolKindPriority . dsrKind) symbols
      topSymbols = take 10 sorted
  refEdgesList <- mapM (extractRefsForSymbol client filePath) topSymbols
  pure (concat refEdgesList)
  ) $ \(_ :: SomeException) -> pure []

-- | Extract references for a single symbol position
extractRefsForSymbol :: LSPClient -> FilePath -> DocumentSymbolResult -> IO [Edge]
extractRefsForSymbol client filePath sym = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let Position line char = rangeStart (dsrRange sym)
      req = lspReferencesWithId filePath line char nextId
  sent <- sendLSPMessageSafe client req
  if not sent
    then pure []
    else do
      mResp <- timeout 5000000 (readLSPResponseForId (lspStdout client) nextId)
      case mResp of
        Nothing -> pure []
        Just (Left _) -> pure []
        Just (Right val) -> pure $ parseReferencesToEdges filePath sym val
  ) $ \(_ :: SomeException) -> pure []

-- | Parse references response into edges
parseReferencesToEdges :: FilePath -> DocumentSymbolResult -> Value -> [Edge]
parseReferencesToEdges filePath sym (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> mapMaybe (parseRefLocation filePath sym) (V.toList arr)
    _ -> []
parseReferencesToEdges _ _ _ = []

parseRefLocation :: FilePath -> DocumentSymbolResult -> Value -> Maybe Edge
parseRefLocation filePath sym (Object loc) =
  let refUri = case KM.lookup "uri" loc of
        Just (Aeson.String u) -> T.drop 7 u
        _ -> ""
      refRange = case KM.lookup "range" loc of
        Just (Object r) -> parseRangeFromFile r
        _ -> Position 0 0
      srcId = makeNodeId filePath (safeLabel (dsrName sym))
      tgtId = makeNodeId (T.unpack refUri) ("ref_" <> T.pack (show (posLine refRange)))
  in if T.null refUri
     then Nothing
     else Just Edge
       { edgeId        = EdgeId (srcId <> "->ref:" <> tgtId)
       , edgeSource    = srcId
       , edgeTarget    = tgtId
       , edgeRelation  = References
        , edgeConfidence = Confidence 0.8
        , edgeWeight    = 0.8
        , edgeExtra       = Nothing
        }
  where
    parseRangeFromFile r =
      case KM.lookup "start" r of
        Just (Object p) ->
          let line = case KM.lookup "line" p of
                Just (Aeson.Number n) -> round n
                _ -> 0
              char = case KM.lookup "character" p of
                Just (Aeson.Number n) -> round n
                _ -> 0
          in Position line char
        _ -> Position 0 0

parseRefLocation _ _ (Array _) = Nothing
parseRefLocation _ _ _ = Nothing

-- ───────────────────────────────────────────────
-- Call hierarchy extraction
-- ───────────────────────────────────────────────

-- | Extract call hierarchy edges for top-5 symbols per file.
extractCallHierarchy :: LSPClient -> FilePath -> [DocumentSymbolResult] -> IO [Edge]
extractCallHierarchy client filePath symbols = catch (do
  let sorted = sortOn (symbolKindPriority . dsrKind) symbols
      topSymbols = take 5 sorted
  callEdgesList <- mapM (extractCallsForSymbol client filePath) topSymbols
  pure (concat callEdgesList)
  ) $ \(_ :: SomeException) -> pure []

-- | Extract incoming calls for a single symbol
extractCallsForSymbol :: LSPClient -> FilePath -> DocumentSymbolResult -> IO [Edge]
extractCallsForSymbol client filePath sym = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let Position line char = rangeStart (dsrRange sym)
      req = lspCallHierarchyPrepareWithId filePath line char nextId
  sent <- sendLSPMessageSafe client req
  if not sent
    then pure []
    else do
      mResp <- timeout 5000000 (readLSPResponseForId (lspStdout client) nextId)
      case mResp of
        Nothing -> pure []
        Just (Left _) -> pure []
        Just (Right val) -> do
          let items = parseCallHierarchyPrepareResponse val
          if null items
            then pure []
            else do
              -- For each item, request incoming calls
              allEdges <- mapM (getIncomingCalls client filePath sym) items
              pure (concat allEdges)
  ) $ \(_ :: SomeException) -> pure []

-- | Parse call hierarchy prepare response
parseCallHierarchyPrepareResponse :: Value -> [CallHierarchyItem]
parseCallHierarchyPrepareResponse (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> mapMaybe parseCallHierarchyItem (V.toList arr)
    _ -> []
parseCallHierarchyPrepareResponse _ = []

parseCallHierarchyItem :: Value -> Maybe CallHierarchyItem
parseCallHierarchyItem (Object o) =
  let name = case KM.lookup "name" o of
        Just (Aeson.String t) -> t
        _ -> ""
      kind = case KM.lookup "kind" o of
        Just (Aeson.Number n) -> round n
        _ -> 0
      uri = case KM.lookup "uri" o of
        Just (Aeson.String u) -> T.drop 7 u
        _ -> ""
      range = case KM.lookup "range" o of
        Just (Object r) -> parseRangeObj r
        _ -> Range (Position 0 0) (Position 0 0)
      selRange = case KM.lookup "selectionRange" o of
        Just (Object r) -> parseRangeObj r
        _ -> Range (Position 0 0) (Position 0 0)
  in if T.null name then Nothing
     else Just CallHierarchyItem { chiName = name, chiKind = kind, chiUri = uri, chiRange = range, chiSelectionRange = selRange }
parseCallHierarchyItem _ = Nothing

parseRangeObj :: Aeson.Object -> Range
parseRangeObj r =
  let start = case KM.lookup "start" r of
        Just (Object p) -> parsePosObj p
        _ -> Position 0 0
      end = case KM.lookup "end" r of
        Just (Object p) -> parsePosObj p
        _ -> Position 0 0
  in Range start end

parsePosObj :: Aeson.Object -> Position
parsePosObj p =
  let line = case KM.lookup "line" p of
        Just (Aeson.Number n) -> round n
        _ -> 0
      char = case KM.lookup "character" p of
        Just (Aeson.Number n) -> round n
        _ -> 0
  in Position line char

-- | Get incoming calls for a call hierarchy item
getIncomingCalls :: LSPClient -> FilePath -> DocumentSymbolResult -> CallHierarchyItem -> IO [Edge]
getIncomingCalls client filePath sym item = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let req = lspCallHierarchyIncomingWithId item nextId
  sent <- sendLSPMessageSafe client req
  if not sent
    then pure []
    else do
      mResp <- timeout 5000000 (readLSPResponseForId (lspStdout client) nextId)
      case mResp of
        Nothing -> pure []
        Just (Left _) -> pure []
        Just (Right val) -> pure $ parseIncomingCallsEdges filePath sym val
  ) $ \(_ :: SomeException) -> pure []

-- | Parse incoming calls response into edges
parseIncomingCallsEdges :: FilePath -> DocumentSymbolResult -> Value -> [Edge]
parseIncomingCallsEdges filePath sym (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> mapMaybe (parseIncomingCall filePath sym) (V.toList arr)
    _ -> []
parseIncomingCallsEdges _ _ _ = []

parseIncomingCall :: FilePath -> DocumentSymbolResult -> Value -> Maybe Edge
parseIncomingCall filePath sym (Object obj) =
  let fromName = case KM.lookup "from" obj of
        Just (Object from) -> case KM.lookup "name" from of
          Just (Aeson.String t) -> t
          _ -> ""
        _ -> ""
      srcId = makeNodeId filePath (safeLabel fromName)
      tgtId = makeNodeId filePath (safeLabel (dsrName sym))
  in if T.null fromName
     then Nothing
     else Just Edge
       { edgeId        = EdgeId (srcId <> "->call:" <> tgtId)
       , edgeSource    = srcId
       , edgeTarget    = tgtId
       , edgeRelation  = Calls
        , edgeConfidence = Confidence 0.9
        , edgeWeight    = 0.9
        , edgeExtra       = Nothing
        }
parseIncomingCall _ _ _ = Nothing

-- ───────────────────────────────────────────────
-- Symbol → Node/Edge conversion
-- ───────────────────────────────────────────────

symbolToNodes :: FilePath -> [DocumentSymbolResult] -> [Node]
symbolToNodes filePath symbols =
  [ Node
    { nodeId           = makeNodeId filePath (safeLabel (dsrName sym))
    , nodeLabel        = fromText (safeLabel (dsrName sym))
    , nodeFileType     = CodeFile
    , nodeSourceFile   = fromText (T.pack filePath)
    , nodeLineStart    = Just $ posLine (rangeStart (dsrRange sym))
    , nodeCommunityId  = Nothing
    , nodeDegree       = Nothing
    , nodeIsBridge     = Nothing
    , nodeExtra        = Nothing
    , nodeLineEnd      = Just $ posLine (rangeEnd (dsrRange sym))
    , nodeKind         = Just (fromText $ symbolKindToText (dsrKind sym))
    , nodeSignature    = Nothing
    , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind
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
          { edgeId        = EdgeId (T.pack (takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath) <> "->" <> makeNodeId filePath (safeLabel (dsrName sym)) <> ":contains")
          , edgeSource    = T.pack (takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath)
          , edgeTarget    = makeNodeId filePath (safeLabel (dsrName sym))
          , edgeRelation  = Contains
           , edgeConfidence = Confidence 1.0
           , edgeWeight    = 1.0
           , edgeExtra       = Nothing
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
        { edgeId        = EdgeId (makeNodeId filePath (safeLabel (dsrName parent)) <> "->" <> makeNodeId filePath (safeLabel (dsrName child)) <> ":contains")
        , edgeSource    = makeNodeId filePath (safeLabel (dsrName parent))
        , edgeTarget    = makeNodeId filePath (safeLabel (dsrName child))
        , edgeRelation  = Contains
        , edgeConfidence = Confidence 1.0
        , edgeWeight    = 1.0
        , edgeExtra       = Nothing
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
    , nodeLabel        = fromText name
    , nodeFileType     = CodeFile
    , nodeSourceFile   = fromText (T.pack filePath)
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodePresentBits  = 0
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
