-- | Extraction orchestration - parallel LSP extraction for all files
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract
  ( extractAll
  , extractFromFile
  ) where

import Control.Concurrent (newQSemN, waitQSemN, signalQSemN)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_, SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (nub, sort, isPrefixOf, find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.FilePath (takeExtension)

import Graphos.Domain.Types
import Graphos.Domain.Extraction ()
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.Infrastructure.LSP.Client (LSPClient(..), extractViaLSP, findLSPServer, LSPClientConfig(..), connectToLSP, disconnectLSP, languageServerCommands, extractWorkspaceSymbols, workspaceSymbolsToDocumentSymbols, symbolToNodes, symbolTreeToEdges)
import Graphos.Infrastructure.LSP.Protocol (scpWorkspaceSymbolProvider, DocumentSymbolResult(..))
import Graphos.Infrastructure.Logging (LogEnv, logInfo, logDebug, logTrace, logWarn)

-- | Extract entities from all detected files.
-- Uses parallel threads when cfgThreads > 1.
-- Files are grouped by LSP server type so each server connection
-- is reused across its files (sequential per server, parallel across servers).
extractAll :: PipelineConfig -> Detection -> LogEnv -> IO Extraction
extractAll config detection env = do
  let codeFiles = Map.findWithDefault [] CodeFiles (detectionFiles detection)
      docFiles  = Map.findWithDefault [] DocFiles  (detectionFiles detection)
      numThreads = max 1 (cfgThreads config)

  -- Canonicalize project root to absolute path for LSP rootUri
  absRoot <- canonicalizePath (cfgInputPath config)

  -- Log discovered file types
  logInfo env $ T.pack $ "  Processing " ++ show (length codeFiles) ++ " code files, " ++ show (length docFiles) ++ " doc files"

  -- Log unique extensions and their LSP status
  let exts = nub (sort (map takeExtension codeFiles))
  logDebug env $ T.pack $ "  File extensions: " ++ show exts

  -- Check LSP availability per extension
  mapM_ (\ext -> do
    mbLSP <- findLSPServer ext
    case mbLSP of
      Just (cmd, args) -> logDebug env $ T.pack $ "  LSP for " ++ ext ++ ": " ++ cmd ++ " " ++ unwords args
      Nothing          -> logWarn env $ T.pack $ "  No LSP for " ++ ext ++ " - using stub extraction"
    ) exts

  -- Group files by LSP server, then extract in parallel across groups
  let fileGroups = groupByLSPServer codeFiles
      numGroups = length fileGroups

  logInfo env $ T.pack $ "  LSP server groups: " ++ show numGroups ++ " (threads: " ++ show numThreads ++ ")"

  codeExtractions <- if numThreads <= 1
    then -- Sequential: process each group one at a time
         concatMapM (extractGroup env absRoot) fileGroups
    else if numGroups <= numThreads
      then do
        -- One thread per group — ideal case
        results <- mapConcurrently (extractGroup env absRoot) fileGroups
        pure (concat results)
      else do
        -- More groups than threads — use semaphore-based pool
        sem <- newQSemN numThreads
        results <- mapConcurrently (\grp -> bracket_
          (waitQSemN sem 1)
          (signalQSemN sem 1)
          (extractGroup env absRoot grp)) fileGroups
        pure (concat results)

  -- Doc extraction: parse markdown files for headers, links, tags
  docExtractions <- mapM (extractDocFile env) docFiles
  let docExtraction = foldr mergeExtractions emptyExtraction docExtractions

  let merged = foldr mergeExtractions docExtraction codeExtractions
  logInfo env $ T.pack $ "  Extracted " ++ show (length (extractionNodes merged)) ++ " nodes, " ++ show (length (extractionEdges merged)) ++ " edges"
  pure merged

-- | A group of files sharing the same LSP server command
type FileGroup = (String, [FilePath])  -- (server command, files)

-- | Group files by their LSP server command
groupByLSPServer :: [FilePath] -> [FileGroup]
groupByLSPServer files =
  let -- Map each file to its LSP server command (or "stub")
      fileWithServer = [(serverCmd f, f) | f <- files]
      -- Group by server command
      grouped = Map.toList $ Map.fromListWith (++) [(cmd, [fp]) | (cmd, fp) <- fileWithServer]
  in grouped
  where
    serverCmd fp = case Map.lookup (takeExtension fp) languageServerCommands of
      Just (cmd, _) -> cmd
      Nothing       -> "stub"

-- | Extract all files in a group using a single shared LSP connection
extractGroup :: LogEnv -> FilePath -> FileGroup -> IO [Extraction]
extractGroup env absRoot (serverCmd, files) =
  if serverCmd == "stub"
    then -- No LSP available — process stubs (fast, no connection needed)
         mapM (\fp -> do
           logDebug env $ T.pack $ "  [stub] " ++ fp
           pure emptyExtraction { extractionNodes = [makeStubNode fp] }
         ) files
    else -- Connect once, extract all files, disconnect
         doExtractWithSharedLSP env absRoot serverCmd files

-- | Connect to an LSP server once and extract all files for it.
-- Tries workspace/symbol first (project-level, single request) if the server supports it.
-- Falls back to per-file documentSymbol if workspace/symbol fails or isn't supported.
doExtractWithSharedLSP :: LogEnv -> FilePath -> String -> [FilePath] -> IO [Extraction]
doExtractWithSharedLSP env absRoot serverCmd files = do
  mbLSPOpts <- findLSPServer (takeExtension (case files of (f:_) -> f; [] -> ""))
  case mbLSPOpts of
    Nothing -> -- Fallback to stubs
      mapM (\fp -> do
        logWarn env $ T.pack $ "  LSP " ++ serverCmd ++ " disappeared for " ++ fp
        pure emptyExtraction { extractionNodes = [makeStubNode fp] }
      ) files
    Just (cmd, args) -> do
      logDebug env $ T.pack $ "  [lsp] Connecting to " ++ cmd ++ " for " ++ show (length files) ++ " files"
      let config = LSPClientConfig
            { lspCommand = cmd
            , lspArgs    = args
            , lspRootUri = absRoot
             , lspTimeout  = 60
            }
      result <- Graphos.Infrastructure.LSP.Client.connectToLSP config
      case result of
        Left err -> do
          logWarn env $ T.pack $ "  [lsp] Connection failed: " ++ T.unpack err
          mapM (\fp -> pure emptyExtraction { extractionNodes = [makeStubNode fp] }) files
        Right client -> do
          let hasWsSym = scpWorkspaceSymbolProvider (lspServerCaps client)
          extractions <- if hasWsSym
            then do
              logInfo env $ T.pack $ "  [lsp] Server supports workspace/symbol — using project-level extraction"
              wsResult <- extractWorkspaceSymbols client
              case wsResult of
                Right syms
                  | not (null syms) -> do
                    let fileSymbols = Graphos.Infrastructure.LSP.Client.workspaceSymbolsToDocumentSymbols syms
                    logInfo env $ T.pack $ "  [lsp] workspace/symbol returned " ++ show (length syms) ++ " symbols across " ++ show (Map.size fileSymbols) ++ " files"
                    pure [extractionFromSymbols fp (Map.findWithDefault [] fp fileSymbols) | fp <- files]
                  | otherwise -> do
                    logDebug env $ T.pack $ "  [lsp] workspace/symbol returned empty — falling back to per-file extraction"
                    mapM (extractViaLSP client) files
                Left err -> do
                  logWarn env $ T.pack $ "  [lsp] workspace/symbol failed: " ++ T.unpack err ++ " — falling back to per-file extraction"
                  mapM (extractViaLSP client) files
            else do
              logDebug env $ T.pack $ "  [lsp] Server does not support workspace/symbol — using per-file extraction"
              mapM (extractViaLSP client) files
          mapM_ (\(fp, ext) -> do
            let nNodes = length (extractionNodes ext)
                nEdges = length (extractionEdges ext)
            logDebug env $ T.pack $ "  [lsp] " ++ fp ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
            ) (zip files extractions)
          enriched <- mapM (\(fp, ext) ->
            if null (extractionNodes ext) && takeExtension fp `elem` [".hs", ".lhs"]
              then do
                logDebug env $ T.pack $ "  [haskell-stub] LSP gave 0 symbols for " ++ fp ++ ", using parser fallback"
                extractHaskellStub fp
              else pure ext
            ) (zip files extractions)
          Graphos.Infrastructure.LSP.Client.disconnectLSP client
          pure enriched

-- | Extract from a single file using LSP (standalone, opens its own connection)
extractFromFile :: LogEnv -> FilePath -> IO Extraction
extractFromFile env filePath = do
  let ext = takeExtension filePath
  logTrace env $ T.pack $ "  Extracting: " ++ filePath ++ " (extension: " ++ ext ++ ")"
  absRoot <- canonicalizePath "."
  mbLSPOpts <- findLSPServer ext
  case mbLSPOpts of
    Nothing -> do
      logDebug env $ T.pack $ "  [stub] " ++ filePath ++ " - no LSP for " ++ ext
      pure emptyExtraction
           { extractionNodes = [makeStubNode filePath]
           , extractionEdges = []
           }
    Just (cmd, args) -> do
      logDebug env $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ cmd ++ " " ++ unwords args
      let config = LSPClientConfig
            { lspCommand = cmd
            , lspArgs    = args
            , lspRootUri = absRoot
             , lspTimeout  = 60
            }
      result <- Graphos.Infrastructure.LSP.Client.connectToLSP config
      case result of
        Left err -> do
          logWarn env $ T.pack $ "  [lsp] Connection failed for " ++ filePath ++ ": " ++ T.unpack err
          pure emptyExtraction
              { extractionNodes = [makeStubNode filePath]
              }
        Right client -> do
          extraction <- extractViaLSP client filePath
          Graphos.Infrastructure.LSP.Client.disconnectLSP client
          let nNodes = length (extractionNodes extraction)
              nEdges = length (extractionEdges extraction)
          logDebug env $ T.pack $ "  [lsp] " ++ filePath ++ " → " ++ show nNodes ++ " nodes, " ++ show nEdges ++ " edges"
          pure extraction

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Create a stub node when no LSP is available
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

-- | Haskell-aware stub extraction: parses module name, imports, and top-level declarations.
-- Used as fallback when HLS returns 0 symbols (e.g., not yet indexed).
extractHaskellStub :: FilePath -> IO Extraction
extractHaskellStub filePath = catch (do
  content <- readFile filePath
  let allNodes = haskellStubNodes filePath content
      edges = haskellStubEdges filePath allNodes
  pure emptyExtraction
    { extractionNodes = allNodes
    , extractionEdges = edges
    }
  ) $ \(_ :: SomeException) -> pure emptyExtraction
    { extractionNodes = [makeStubNode filePath] }

-- | Parse Haskell source for module name, imports, and top-level decl names
haskellStubNodes :: FilePath -> String -> [Node]
haskellStubNodes filePath content =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      modName = parseHaskellModule content
      imports = parseHaskellImports content
      decls = parseHaskellDecls content
      modNode = case modName of
        Just mn ->
          [ Node
            { nodeId           = hashPrefix <> T.pack "_" <> T.pack mn
            , nodeLabel        = T.pack mn
            , nodeFileType     = CodeFile
            , nodeSourceFile   = T.pack filePath
            , nodeSourceLocation = Just "L1"
            , nodeLineEnd      = Nothing
            , nodeKind         = Just "Module"
            , nodeSignature    = Nothing
            , nodeSourceUrl    = Nothing
            , nodeCapturedAt   = Nothing
            , nodeAuthor       = Nothing
            , nodeContributor  = Nothing
            }
          ]
        Nothing -> []
      impNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_import_" <> T.pack imp
        , nodeLabel        = T.pack imp
        , nodeFileType     = CodeFile
        , nodeSourceFile   = T.pack filePath
        , nodeSourceLocation = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just "Module"
        , nodeSignature    = Nothing
        , nodeSourceUrl    = Nothing
        , nodeCapturedAt   = Nothing
        , nodeAuthor       = Nothing
        , nodeContributor  = Nothing
        }
        | imp <- imports
        ]
      declNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_" <> T.pack decl
        , nodeLabel        = T.pack decl
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
        | decl <- decls
        ]
  in modNode ++ impNodes ++ declNodes

-- | Build edges from Haskell stub nodes: module→import, module→decl
haskellStubEdges :: FilePath -> [Node] -> [Edge]
haskellStubEdges filePath nodes =
  let modNodeM = find (\n -> not ("_import_" `T.isInfixOf` nodeId n)) nodes
  in case modNodeM of
    Just mn ->
      let otherNodes = filter (\n -> nodeId n /= nodeId mn) nodes
      in [ Edge
        { edgeSource        = nodeId mn
        , edgeTarget        = nodeId other
        , edgeRelation      = Imports
        , edgeConfidence    = Ambiguous
        , edgeConfidenceScore = 0.7
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = nodeSourceLocation mn
        , edgeWeight        = 0.7
        }
        | other <- otherNodes
        ]
    Nothing -> []

-- | Parse the module name from a Haskell source file
parseHaskellModule :: String -> Maybe String
parseHaskellModule content =
  case [line | line <- lines content, "module " `isPrefixOf` dropWhile (== ' ') line] of
    (line:_) -> Just $ extractModuleName line
    [] -> Nothing
  where
    extractModuleName line =
      let afterModule = dropWhile (== ' ') $ drop 7 line
          name = takeWhile (\c -> isAlphaNum c || c `elem` ("._" :: String)) afterModule
      in if null name then "Main" else name

-- | Parse import declarations from a Haskell source file
parseHaskellImports :: String -> [String]
parseHaskellImports content =
  [ extractImportName line
  | line <- lines content
  , "import " `isPrefixOf` dropWhile (== ' ') line
  ]
  where
    extractImportName line =
      let trimmed = dropWhile (== ' ') line
          afterImport = dropWhile (== ' ') $ drop 7 trimmed
          isQualified = "qualified " `isPrefixOf` afterImport
          afterQual = if isQualified then dropWhile (== ' ') $ drop 9 afterImport else afterImport
          name = takeWhile (\c -> isAlphaNum c || c `elem` ("._" :: String)) afterQual
      in if null name then "Unknown" else name

-- | Parse top-level declaration names from a Haskell source file
parseHaskellDecls :: String -> [String]
parseHaskellDecls content =
  [ extractDeclName line
  | line <- lines content
  , isTopLevelDecl line
  ]
  where
    isTopLevelDecl line =
      let trimmed = dropWhile (== ' ') line
      in not (null trimmed)
         && case trimmed of (c:_) -> c `notElem` ("-{-#" :: String); [] -> False
         && not ("module " `isPrefixOf` trimmed)
         && not ("import " `isPrefixOf` trimmed)
         && not ("where" `isPrefixOf` trimmed)
         && not ("deriving" `isPrefixOf` trimmed)
         && not ("else" `isPrefixOf` trimmed)
         && not ("then" `isPrefixOf` trimmed)
         && not ("in " `isPrefixOf` trimmed)
         && not ("do" == trimmed)
         && not ("let" `isPrefixOf` trimmed)
         && not ("=" == dropWhile (/= '=') trimmed)

    extractDeclName line =
      let trimmed = dropWhile (== ' ') line
          -- Handle data/newtype/class/type/instance
          (_prefixLen, rest) = case trimmed of
            s | "data " `isPrefixOf` s -> (5 :: Int, drop 5 s)
              | "newtype " `isPrefixOf` s -> (8, drop 8 s)
              | "type " `isPrefixOf` s -> (5, drop 5 s)
              | "class " `isPrefixOf` s -> (6, drop 6 s)
              | "instance " `isPrefixOf` s -> (9, drop 9 s)
              | "type family " `isPrefixOf` s -> (12, drop 12 s)
              | "data family " `isPrefixOf` s -> (12, drop 12 s)
              | otherwise -> (0, s)
          nameRest = dropWhile (== ' ') rest
          name = takeWhile (\c -> isAlphaNum c || c `elem` ("'_" :: String)) nameRest
      in if null name
         then take 20 trimmed
         else name

-- | Sequential concatMapM (since it's not in base)
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | Build an Extraction from a file's DocumentSymbolResults.
-- Reuses the same symbol→node/edge conversion as extractViaLSP.
extractionFromSymbols :: FilePath -> [DocumentSymbolResult] -> Extraction
extractionFromSymbols filePath symbols =
  let nodes = symbolToNodes filePath symbols
      edges = symbolTreeToEdges filePath symbols
  in emptyExtraction
    { extractionNodes = nodes
    , extractionEdges = edges
    }

-- ───────────────────────────────────────────────
-- Document extraction (Markdown, texts)
-- ───────────────────────────────────────────────

-- | Extract concepts and relationships from a document file.
-- Parses headers as nodes, wikilinks/links/tags as edges.
extractDocFile :: LogEnv -> FilePath -> IO Extraction
extractDocFile env filePath = catch (do
  content <- readFile filePath
  let allNodes = docNodes filePath content
      allEdges = docEdges filePath content allNodes
  logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → " ++ show (length allNodes) ++ " nodes, " ++ show (length allEdges) ++ " edges"
  pure emptyExtraction
    { extractionNodes = allNodes
    , extractionEdges = allEdges
    }
  ) $ \(_ :: SomeException) -> do
    logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → stub (read error)"
    pure emptyExtraction { extractionNodes = [makeStubNode filePath] }

-- | Parse a document for nodes: file node, headers, tags
docNodes :: FilePath -> String -> [Node]
docNodes filePath content =
  let fileNode = docFileNode filePath
      headerNodes = docHeaderNodes filePath content
      tagNodes = docTagNodes filePath content
  in fileNode : headerNodes ++ tagNodes

-- | Create a file-level node for a document
docFileNode :: FilePath -> Node
docFileNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> T.pack "_doc_" <> name
  in Node
    { nodeId           = nid
    , nodeLabel        = name
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just "L1"
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "File"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Parse headers (## Title) as nodes
docHeaderNodes :: FilePath -> String -> [Node]
docHeaderNodes filePath content =
  [ mkHeaderNode filePath level titleText lineNum
  | (lineNum, line) <- zip [1..] (lines content)
  , Just (level, titleText) <- [parseHeader line]
  , level <= 4  -- only H1-H4
  ]

-- | Parse a markdown header line, returns (level, title)
parseHeader :: String -> Maybe (Int, String)
parseHeader line =
  let trimmed = dropWhile (== ' ') line
  in case trimmed of
    '#':'#':'#':'#':rest -> Just (4, dropWhile (== ' ') rest)
    '#':'#':'#':rest     -> Just (3, dropWhile (== ' ') rest)
    '#':'#':rest         -> Just (2, dropWhile (== ' ') rest)
    '#':rest             -> Just (1, dropWhile (== ' ') rest)
    _                    -> Nothing

-- | Create a node for a header
mkHeaderNode :: FilePath -> Int -> String -> Int -> Node
mkHeaderNode filePath level title lineNum =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      cleanTitle = T.pack $ takeWhile (\c -> isAlphaNum c || c `elem` (" -'_/" :: String)) title
      nid = T.pack (show dirHash) <> T.pack "_h" <> T.pack (show level) <> T.pack "_" <> cleanTitle
  in Node
    { nodeId           = nid
    , nodeLabel        = cleanTitle
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just (T.pack $ "L" ++ show lineNum)
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Header"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Parse tags (#tag or #tag/sub) as nodes
docTagNodes :: FilePath -> String -> [Node]
docTagNodes filePath content =
  let tags = nub $ parseTags content
  in [ mkTagNode filePath tag | tag <- tags ]
  where
    parseTags :: String -> [String]
    parseTags txt = [ tag | tag <- extractTags txt, not (isHeaderTag txt tag) ]

-- | Extract #tags from text (but exclude # headers)
extractTags :: String -> [String]
extractTags text =
  [ tag
  | (i, ch) <- zip [0..] text
  , ch == '#'
  , i > 0  -- not start of line header
  , let prev = if i > 0 then text !! (i-1) else ' '
  , prev == ' ' || prev == '\n' || prev == ','
  , let afterHash = takeWhile (\ch' -> isAlphaNum ch' || ch' `elem` ("_/-" :: String)) (drop (i+1) text)
  , not (null afterHash)
  , case afterHash of (c':_) -> c' `notElem` (" " :: String); [] -> True  -- not a header
  , let tag = afterHash
  , length tag >= 2
  ]

isHeaderTag :: String -> String -> Bool
isHeaderTag _ _ = False  -- simplified: all #tags are tags, headers are parsed separately

-- | Create a node for a tag
mkTagNode :: FilePath -> String -> Node
mkTagNode filePath tag =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> T.pack "_tag_" <> T.pack tag
  in Node
    { nodeId           = nid
    , nodeLabel        = T.pack $ "#" ++ tag
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Tag"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Build edges: file→header (contains), file→tag (tags), wikilinks (references)
docEdges :: FilePath -> String -> [Node] -> [Edge]
docEdges filePath content nodes =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      fileNid = T.pack (show dirHash) <> T.pack "_doc_" <> T.pack (takeWhile (/= '.') (reverse $ takeWhile (/= '/') $ reverse filePath))
      -- file → header edges (Contains)
      headerEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = nodeId n
        , edgeRelation      = Contains
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 1.0
        }
        | n <- nodes
        , T.isInfixOf (T.pack "_h") (nodeId n)  -- header nodes
        ]
      -- file → tag edges (Tags)
      tagEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = nodeId n
        , edgeRelation      = References
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 1.0
        }
        | n <- nodes
        , T.isInfixOf (T.pack "_tag_") (nodeId n)  -- tag nodes
        ]
      -- wikilink edges [[Target]] → file node of target
      wikilinkEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = T.pack target  -- best-effort: link to target name
        , edgeRelation      = References
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 0.8
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 0.8
        }
        | target <- parseWikiLinks content
        ]
  in headerEdges ++ tagEdges ++ wikilinkEdges

-- | Parse [[wikilinks]] from markdown content
parseWikiLinks :: String -> [String]
parseWikiLinks content =
  [ T.unpack $ T.strip $ T.takeWhile (/= '|') $ T.drop 2 t
  | t <- T.splitOn (T.pack "[[") (T.pack content)
  , T.isInfixOf (T.pack "]]") t
  , let linkText = T.takeWhile (/= ']') t
  , not (T.null linkText)
  ]