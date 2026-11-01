-- | Haskell stub extraction — parses module name, imports, and top-level declarations.
-- Used as fallback when HLS returns 0 symbols (e.g., not yet indexed).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract.Haskell
  ( extractHaskellStub
  , parseHaskellModule
  , parseHaskellImports
  , parseHaskellDecls
  , isTopLevelDecl
  , extractDeclName
  , extractImportName
  , haskellStubNodes
  , haskellStubNodesIO
  , haskellStubNodesWith
  , haskellStubEdges
  , resolveHaskellModule
  , resolveHaskellImport
  , resolveHaskellImportPath
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (SomeException, catch, try)
import Control.Monad (filterM)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (find, isPrefixOf, isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, maybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, takeDirectory, (</>), takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits ((.|.))
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (makeStubNode)

-- | Haskell-aware stub extraction
extractHaskellStub :: FilePath -> IO Extraction
extractHaskellStub filePath = catch (do
  content <- readFile filePath
  allNodes <- haskellStubNodesIO filePath content
  let edges = haskellStubEdges filePath allNodes
  pure (extractionFromLists allNodes edges)
  ) $ \(_ :: SomeException) -> pure (extractionFromLists [makeStubNode filePath] [])

-- | Build a canonical module node ID.
-- Module IDs are shared across all files that import or declare the same module
-- name, which enables cross-file import edges. The special module name 'Main' is
-- kept directory-scoped to avoid merging unrelated executable entry points.
canonicalModuleId :: T.Text -> FilePath -> NodeId
canonicalModuleId name filePath
  | name == T.pack "Main" =
      let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
          dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      in T.pack (show dirHash) <> T.pack "_" <> name
  | otherwise = T.pack "mod_" <> name

-- | Parse Haskell source for module name, imports, and top-level decl names
haskellStubNodes :: FilePath -> String -> [Node]
haskellStubNodes filePath content = unsafePerformIO (haskellStubNodesIO filePath content)

-- | IO-aware variant: resolves imported modules to their @.hs@/@.lhs@ files when
-- they exist, so an import target never carries another file's path.
haskellStubNodesIO :: FilePath -> String -> IO [Node]
haskellStubNodesIO filePath content = do
  let imports = parseHaskellImports content
  resolved <- mapM (resolveHaskellModule filePath) imports
  pure (haskellStubNodesWith filePath content resolved)

-- | Pure core, taking the resolution of each parsed import ('Nothing' means the
-- module is external / unresolved).
haskellStubNodesWith :: FilePath -> String -> [Maybe FilePath] -> [Node]
haskellStubNodesWith filePath content resolvedImports =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      modName = parseHaskellModule content
      imports = parseHaskellImports content
      decls = parseHaskellDecls content
      resolved = resolvedImports
      modNode = case modName of
        Just mn ->
          let mnText = T.pack mn
          in [ Node
               { nodeId           = canonicalModuleId mnText filePath
               , nodeLabel        = fromText mnText
               , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack filePath)
        , nodeSource       = Nothing
               , nodeLineStart    = Just 1
               , nodeCommunityId  = Nothing
               , nodeDegree       = Nothing
               , nodeIsBridge     = Nothing
               , nodeExtra        = Nothing
               , nodeLineEnd      = Nothing
               , nodeKind         = Just (fromText "Module")
               , nodeSignature    = Nothing
               , nodePresentBits  = bitNodeLineStart .|. bitNodeKind
               }
              ]
        Nothing -> []
      impNodes = [ Node
        { nodeId           = canonicalModuleId (T.pack imp) filePath
        , nodeLabel        = fromText (T.pack imp)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack (maybe "" id mpath))
        , nodeSource       = Nothing
   , nodeLineStart    = Nothing
   , nodeCommunityId  = Nothing
   , nodeDegree       = Nothing
   , nodeIsBridge     = Nothing
   , nodeExtra        = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just (fromText (maybe "External" (const "Module") mpath))
        , nodeSignature    = Nothing
        , nodePresentBits  = bitNodeKind
        }
        | (imp, mpath) <- zip imports resolved
        ]
      declNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_" <> T.pack declName
        , nodeLabel        = fromText (T.pack declName)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack filePath)
        , nodeSource       = Nothing
   , nodeLineStart    = Nothing
   , nodeCommunityId  = Nothing
   , nodeDegree       = Nothing
   , nodeIsBridge     = Nothing
   , nodeExtra        = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just (fromText declKind')
        , nodeSignature    = Nothing
        , nodePresentBits  = bitNodeKind
        }
        | (declName, declKind') <- decls
        ]
  in modNode ++ impNodes ++ declNodes

-- | Build edges from Haskell stub nodes: module→import, module→decl.
-- Uses "_import_" in the node ID to distinguish import targets from declarations.
haskellStubEdges :: FilePath -> [Node] -> [Edge]
haskellStubEdges _filePath nodes =
  let modNodeM = find (\n -> nodeKind n == Just "Module") nodes
  in case modNodeM of
    Just mn ->
      let importNodes = filter (\n -> (nodeKind n == Just "Module" || nodeKind n == Just "External") && nodeId n /= nodeId mn) nodes
          declNodes = filter (\n -> nodeKind n /= Just "Module" && nodeKind n /= Just "External") nodes
          mkEdge rel other =
            Edge
              { edgeId        = EdgeId (nodeId mn <> "->" <> nodeId other <> ":" <> relName rel)
              , edgeSource    = nodeId mn
              , edgeTarget    = nodeId other
              , edgeRelation  = rel
              , edgeConfidence = Confidence 0.7
              , edgeWeight    = 0.7
              , edgeExtra     = Nothing
              }
      in map (mkEdge Imports) importNodes ++ map (mkEdge Contains) declNodes
    Nothing -> []
  where
    relName Imports  = "imports"
    relName Contains = "contains"
    relName r        = T.pack (show r)

{- | Parse the module name from a Haskell source file -}
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

-- | Extract import name from a line
extractImportName :: String -> String
extractImportName line =
  let trimmed = dropWhile (== ' ') line
      afterImport = dropWhile (== ' ') $ drop 7 trimmed
      isQualified = "qualified " `isPrefixOf` afterImport
      afterQual = if isQualified then dropWhile (== ' ') $ drop 9 afterImport else afterImport
      name = takeWhile (\c -> isAlphaNum c || c `elem` ("._" :: String)) afterQual
  in if null name then "Unknown" else name

-- | Parse top-level declarations from a Haskell source file.
-- Returns @(name, kind)@ pairs for valid declarations only.
parseHaskellDecls :: String -> [(String, T.Text)]
parseHaskellDecls content =
  [ (name, declKind line)
  | line <- lines content
  , isTopLevelDecl line
  , Just name <- [extractDeclName line]
  ]

-- | Check if a line is a top-level declaration.
-- Only column-0 lines beginning with a letter or '(' are considered.
-- This excludes indented continuations, guards ('|'), braces, string literals,
-- comments, pragmas, and control keywords.
isTopLevelDecl :: String -> Bool
isTopLevelDecl line =
  case dropWhile (== ' ') line of
    [] -> False
    trimmed@(c:_) ->
      (isAlpha c || c == '(')
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

-- | Extract declaration name from a line. Returns 'Nothing' when the line
-- does not start with a recognizable declaration form.
extractDeclName :: String -> Maybe String
extractDeclName line =
  let trimmed = dropWhile (== ' ') line
      (_prefixLen, rest) = case trimmed of
        s | "data family " `isPrefixOf` s -> (12 :: Int, drop 12 s)
          | "type family " `isPrefixOf` s -> (12, drop 12 s)
          | "newtype " `isPrefixOf` s -> (8, drop 8 s)
          | "data " `isPrefixOf` s -> (5, drop 5 s)
          | "type " `isPrefixOf` s -> (5, drop 5 s)
          | "class " `isPrefixOf` s -> (6, drop 6 s)
          | "instance " `isPrefixOf` s -> (9, drop 9 s)
          | otherwise -> (0, s)
      nameRest = dropWhile (== ' ') rest
      name = takeWhile (\c -> isAlphaNum c || c `elem` ("'_" :: String)) nameRest
  in if null name then Nothing else Just name

-- | Classify a declaration line into a node kind.
-- All identifier declarations that are not data/newtype/type/class/instance
-- are treated as functions (bindings or type signatures).
declKind :: String -> T.Text
declKind line
  | "data family " `isPrefixOf` trimmed = "Type"
  | "type family " `isPrefixOf` trimmed = "Type"
  | "data " `isPrefixOf` trimmed = "Type"
  | "newtype " `isPrefixOf` trimmed = "Type"
  | "type " `isPrefixOf` trimmed = "Type"
  | "class " `isPrefixOf` trimmed = "Class"
  | "instance " `isPrefixOf` trimmed = "Instance"
  | otherwise = "Function"
  where trimmed = dropWhile (== ' ') line

resolveHaskellModule :: FilePath -> String -> IO (Maybe FilePath)
resolveHaskellModule importer modName = do
  let modRelHs  = dotToPath modName ++ ".hs"
      modRelLhs = dotToPath modName ++ ".lhs"
      dir = takeDirectory importer
      safeDir = if null dir then "." else dir
      ancestorBases = nub (safeDir : ancestors safeDir)
      ancestorCands = [ base </> rel | base <- ancestorBases, rel <- [modRelHs, modRelLhs] ]
  direct <- firstExistingIO ancestorCands
  case direct of
    Just p -> pure (Just p)
    Nothing -> do
      root <- projectRootFor (if null importer then "." else importer)
      srcDirs <- sourceDirsFromManifest root
      let manCands = [ d </> rel | d <- srcDirs, rel <- [modRelHs, modRelLhs] ]
      direct2 <- firstExistingIO manCands
      case direct2 of
        Just p -> pure (Just p)
        Nothing -> do
          files <- getFileList root
          let matches = [ f | f <- files
                            , let m = toModuleName (makeRelative root f)
                            , m == modName || ("." ++ modName) `isSuffixOf` m ]
          pure (listToMaybe matches)

resolveHaskellImport :: FilePath -> String -> IO (Maybe FilePath)
resolveHaskellImport = resolveHaskellModule

resolveHaskellImportPath :: FilePath -> String -> IO (Maybe FilePath)
resolveHaskellImportPath = resolveHaskellModule

projectRootFor :: FilePath -> IO FilePath
projectRootFor importer = do
  let start = takeDirectory importer
      s = if null start then "." else start
  mman <- findManifestDir s
  case mman of
    Just r -> pure r
    Nothing -> do
      mgit <- findGitRoot s
      case mgit of
        Just r -> pure r
        Nothing -> pure "."

findManifestDir :: FilePath -> IO (Maybe FilePath)
findManifestDir start = go (ancestors (if null start then "." else start))
  where
    go [] = pure Nothing
    go (d:ds) = do
      here <- manifestHere d
      if here then pure (Just d) else go ds

manifestHere :: FilePath -> IO Bool
manifestHere d = do
  r <- tryIO (listDirectory d)
  case r of
    Left _ -> pure False
    Right es -> pure (any (".cabal" `isSuffixOf`) es
                    || elem "package.yaml" es
                    || elem "package.yml" es
                    || elem "hpack.yaml" es)

findGitRoot :: FilePath -> IO (Maybe FilePath)
findGitRoot start = go (ancestors (if null start then "." else start))
  where
    go [] = pure Nothing
    go (d:ds) = do
      gd <- doesDirectoryExist (d </> ".git")
      gf <- doesFileExist (d </> ".git")
      if gd || gf then pure (Just d) else go ds

sourceDirsFromManifest :: FilePath -> IO [FilePath]
sourceDirsFromManifest root = do
  r <- tryIO (listDirectory root)
  case r of
    Left _ -> pure [root]
    Right es -> do
      let manifests = [ root </> e | e <- es
                      , ".cabal" `isSuffixOf` e
                      || elem e ["package.yaml", "package.yml", "hpack.yaml"] ]
      texts <- mapM (fmap (either (const "") id) . tryIO . readFile) manifests
      let dirs = nub (concatMap parseSourceDirs (map T.pack texts))
          absDirs = [ if isAbsolute p then p else root </> p | p <- dirs ]
      pure (root : absDirs)

parseSourceDirs :: T.Text -> [String]
parseSourceDirs src = concatMap tokens (collectFields (T.lines src))

collectFields :: [T.Text] -> [T.Text]
collectFields [] = []
collectFields (l:rest) =
  let s = T.stripStart l
      keysT = map T.pack ["hs-source-dirs", "source-dirs"]
      matches = any (\k -> T.isPrefixOf (k `T.append` ":") s) keysT
  in if matches
       then let after = T.strip (snd (T.breakOn ":" l))
                more = takeWhile (\x -> not (T.null (T.strip x)) && T.isPrefixOf " " x) rest
                val = if T.null after then T.unlines more else after
            in val : collectFields (drop (length more) rest)
       else collectFields rest

tokens :: T.Text -> [String]
tokens t = [ T.unpack (T.strip (T.replace "," "" x))
           | x <- T.words (T.replace "," " " t)
           , not (T.null (T.strip x)) ]

getFileList :: FilePath -> IO [FilePath]
getFileList root = do
  cached <- tryTakeCache root
  case cached of
    Just xs -> pure xs
    Nothing -> do
      xs <- walkFiles root 10
      putCache root xs
      pure xs

tryTakeCache :: FilePath -> IO (Maybe [FilePath])
tryTakeCache root = withMVar fileIndexCacheVar (pure . Map.lookup root)

putCache :: FilePath -> [FilePath] -> IO ()
putCache root xs = modifyMVar_ fileIndexCacheVar (pure . Map.insert root xs)

walkFiles :: FilePath -> Int -> IO [FilePath]
walkFiles root depth
  | depth < 0 = pure []
  | otherwise = do
      r <- tryIO (listDirectory root)
      case r of
        Left _ -> pure []
        Right es -> do
          let full = [ root </> e | e <- es, not (skipName e) ]
          files <- filterM doesFileExist full
          let hsFiles = [ f | f <- files, takeExtension f `elem` [".hs", ".lhs"] ]
          dirs <- filterM doesDirectoryExist full
          sub <- concat <$> mapM (\d -> walkFiles d (depth - 1)) dirs
          pure (sort (hsFiles ++ sub))

skipName :: FilePath -> Bool
skipName n =
  elem n [".git", "dist", "dist-newstyle", "node_modules", ".stack-work", "target"]
  || (not (null n) && take 1 n == ".")

firstExistingIO :: [FilePath] -> IO (Maybe FilePath)
firstExistingIO xs = fmap listToMaybe (filterM doesFileExist xs)

dotToPath :: String -> String
dotToPath = map (\c -> if c == '.' then '/' else c)

toModuleName :: FilePath -> String
toModuleName p = map (\c -> if c == '/' || c == '\\' then '.' else c) (dropExtension p)

ancestors :: FilePath -> [FilePath]
ancestors d =
  let p = takeDirectory d
  in if p == d || null d then [d] else d : ancestors p

tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

isAbsolute :: FilePath -> Bool
isAbsolute p = not (null p) && head p == '/'

{-# NOINLINE fileIndexCacheVar #-}
fileIndexCacheVar :: MVar (Map.Map FilePath [FilePath])
fileIndexCacheVar = unsafePerformIO (newMVar Map.empty)
