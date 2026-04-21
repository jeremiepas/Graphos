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
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Infrastructure.LSP.Protocol hiding (languageIdFromExt)
import Graphos.Infrastructure.LSP.Transport
  ( LSPClient(..)
  , sendLSPMessage
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
    catch (sendLSPMessage (lspStdin client) openMsg) $ \(_ :: SomeException) -> pure ()
    catch (drainNotifications (lspStdout client) 500000) $ \(_ :: SomeException) -> pure ()

    symbols <- extractDocumentSymbols client filePath
    putStrLn $ "[lsp] Got " ++ show (length symbols) ++ " symbols from " ++ filePath

    let closeMsg = lspDidClose filePath
    catch (sendLSPMessage (lspStdin client) closeMsg) $ \(_ :: SomeException) -> pure ()

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
  sendLSPMessage (lspStdin client) req

  resp <- readLSPResponseForId (lspStdout client) nextId
  case resp of
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
    { nodeId           = makeNodeId filePath (dsrName sym)
    , nodeLabel        = dsrName sym
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

-- | Build edges from the symbol tree (parent contains child).
symbolTreeToEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
symbolTreeToEdges filePath flatSymbols =
  let fileEdges =
        [ Edge
          { edgeSource        = T.pack (takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath)
          , edgeTarget        = makeNodeId filePath (dsrName sym)
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
buildHierarchyEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
buildHierarchyEdges filePath symbols =
  let containsPairs = [(parent, child)
                       | parent <- symbols
                       , child <- symbols
                       , dsrName parent /= dsrName child
                       , rangeContains (dsrRange parent) (dsrRange child)
                       ]
      directPairs = [(p, c) | (p, c) <- containsPairs
                            , not (any (\m -> dsrName m /= dsrName p
                                          && dsrName m /= dsrName c
                                          && rangeContains (dsrRange p) (dsrRange m)
                                          && rangeContains (dsrRange m) (dsrRange c)) symbols)]
  in [ Edge
     { edgeSource        = makeNodeId filePath (dsrName parent)
     , edgeTarget        = makeNodeId filePath (dsrName child)
     , edgeRelation      = Contains
     , edgeConfidence    = Extracted
     , edgeConfidenceScore = 1.0
     , edgeSourceFile    = T.pack filePath
     , edgeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange parent))))
     , edgeWeight        = 1.0
     }
     | (parent, child) <- directPairs]

-- | Check if one range fully contains another
rangeContains :: Range -> Range -> Bool
rangeContains outer inner =
  let outerStart = posLine (rangeStart outer) * 10000 + posCharacter (rangeStart outer)
      outerEnd   = posLine (rangeEnd outer) * 10000 + posCharacter (rangeEnd outer)
      innerStart = posLine (rangeStart inner) * 10000 + posCharacter (rangeStart inner)
      innerEnd   = posLine (rangeEnd inner) * 10000 + posCharacter (rangeEnd inner)
  in innerStart >= outerStart && innerEnd <= outerEnd

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

makeNodeId :: FilePath -> Text -> NodeId
makeNodeId filePath name =
  let stem = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
  in hashPrefix <> T.pack "_" <> stem <> T.pack "_" <> name

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
  sendLSPMessage (lspStdin client) req

  resp <- readLSPResponseForId (lspStdout client) nextId
  case resp of
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