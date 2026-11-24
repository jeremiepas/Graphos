{-# LANGUAGE OverloadedStrings #-}
-- | Corpus-aware import resolution (bunch-real-projects D4).
--
-- 'resolveImport' runs per file and never sees the corpus, so an
-- extension-less relative target (@./b@), a root alias (@\@/lib/x@,
-- @$lib/api.js@) or a tsconfig @paths@ specifier (@app/config@) stays a
-- placeholder: an @External@ node, or a @Module@ node whose path is not an
-- extracted file. This pass runs once over the merged extraction, before
-- graph build, and rewrites every edge whose target is such a placeholder to
-- the module node of the corpus file it denotes:
--
--   1. extension-less and directory targets: @p.<ext>@ for the TS/JS family
--      extensions, in order, then @p/index.<ext>@ (first hit wins, as in
--      TypeScript's own lookup);
--   2. root aliases @\@/@, @~/@, @#/@, @$lib/@: unique path-suffix match of
--      the rest (@$lib/x@ matches the suffix @lib/x@);
--   3. tsconfig / jsconfig @compilerOptions.paths@ of the nearest enclosing
--      config; otherwise a specifier containing @/@ resolves by a unique
--      path-suffix match.
--
-- Only edges whose source file is in the TS/JS family are considered. A
-- suffix match resolves only when exactly one corpus file matches. Scoped
-- packages, @node:@ builtins and bare names stay external. A rewritten edge
-- keeps its 'EdgeId'; a placeholder node left with no incoming edge is
-- dropped. The pass reads nothing but the extraction and the parsed configs,
-- and executes nothing.
--
-- These are the built-in @path-alias@ / @tsconfig-paths@ resolvers that the
-- @resolve:@ framework of @avi-1766-custom-extractors@ will register; until it
-- lands there is no user override to consult.
module Graphos.Infrastructure.Extract.ImportResolution
  ( PathsConfig(..)
  , ResolutionStats(..)
  , resolveCorpusImports
  , parsePathsConfig
  , loadPathsConfigs
  , jsFamilyExtensions
  , stripJsonComments
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Value(..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText)
import qualified Data.Vector as V
import System.FilePath (takeDirectory, takeExtension, takeFileName, dropExtension, splitDirectories)

import Graphos.Domain.Types (Node(..), Edge(..), Extraction(..), NodeId)
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveDots)

-- | One @tsconfig.json@ / @jsconfig.json@ with @compilerOptions.paths@.
data PathsConfig = PathsConfig
  { pcDir     :: !FilePath                -- ^ directory holding the config (corpus path form)
  , pcBaseUrl :: !FilePath                -- ^ @compilerOptions.baseUrl@, relative to 'pcDir' ("." when absent)
  , pcPaths   :: ![(String, [String])]    -- ^ pattern → mapped locations, in file order
  } deriving (Eq, Show)

-- | What the pass did, for the build log.
data ResolutionStats = ResolutionStats
  { rsExtension :: !Int  -- ^ edges resolved by rule 1 (extension / index)
  , rsAlias     :: !Int  -- ^ edges resolved by rule 2 (root alias)
  , rsPaths     :: !Int  -- ^ edges resolved by rule 3 (tsconfig paths or suffix)
  , rsDropped   :: !Int  -- ^ placeholder nodes dropped
  } deriving (Eq, Show)

data Rule = RExt | RAlias | RPaths deriving (Eq, Show)

-- | Extensions of the TS/JS family, in lookup order.
jsFamilyExtensions :: [String]
jsFamilyExtensions =
  [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".mts", ".cts", ".vue", ".svelte"]

isJsFamily :: FilePath -> Bool
isJsFamily p = takeExtension p `elem` jsFamilyExtensions

-- ───────────────────────────────────────────────
-- Corpus index
-- ───────────────────────────────────────────────

data Corpus = Corpus
  { cModules :: !(Map.Map FilePath NodeId)
    -- ^ extracted file (normalised: no leading @./@) → its module node
  , cByLast  :: !(Map.Map String [(String, FilePath)])
    -- ^ last component of a logical path → (logical path, file). A file's
    -- logical paths are its path without extension and, for @index.*@, its
    -- directory.
  }

-- | The real module nodes: kind @Module@ with a line span (a placeholder
-- emitted for an import target has none) and a non-external path.
buildCorpus :: Extraction -> Corpus
buildCorpus ext =
  let mods = Map.fromListWith min
        [ (fp, nodeId n)
        | n <- Map.elems (extractionNodes ext)
        , fmap toText (nodeKind n) == Just "Module"
        , nodeLineStart n /= Nothing
        , let raw = T.unpack (toText (nodeSourceFile n))
        , not ("external:" `isPrefixOf` raw)
        , let fp = normalisePath raw
        ]
      logical fp =
        let noExt = dropExtension fp
        in noExt : [ takeDirectory fp | takeFileName noExt == "index", takeDirectory fp /= "." ]
      byLast = Map.fromListWith (++)
        [ (lastComponent lp, [(lp, fp)])
        | fp <- Map.keys mods, isJsFamily fp, lp <- logical fp ]
  in Corpus mods byLast

lastComponent :: FilePath -> String
lastComponent = reverse . takeWhile (/= '/') . reverse

-- | Rule 1: @p@ as written, then @p.<ext>@, then @p/index.<ext>@. A code
-- extension already on @p@ (including the @.ts@ that 'resolveImport' writes
-- for a @.js@ specifier) is dropped first.
lookupPath :: Corpus -> FilePath -> Maybe FilePath
lookupPath c p0 =
  let p = normalisePath p0
      q = if isJsFamily p then dropExtension p else p
      cands = [p] ++ [ q ++ e | e <- jsFamilyExtensions ]
                  ++ [ q ++ "/index" ++ e | e <- jsFamilyExtensions ]
  in listToMaybe [ f | f <- cands, Map.member f (cModules c) ]

-- | Unique path-suffix match of @x@ (code extension ignored).
suffixMatch :: Corpus -> String -> Maybe FilePath
suffixMatch c x0 =
  let x1 = normalisePath x0
      x = if isJsFamily x1 then dropExtension x1 else x1
      cands = nub
        [ fp | (lp, fp) <- Map.findWithDefault [] (lastComponent x) (cByLast c)
             , lp == x || ("/" ++ x) `isSuffixOf` lp ]
  in if null x then Nothing else case cands of
       [f] -> Just f
       _   -> Nothing

normalisePath :: FilePath -> FilePath
normalisePath p =
  let stripped = dropTrailing (dropDotSlash p)
  in if null stripped then stripped else resolveDots stripped
  where
    dropDotSlash s = if "./" `isPrefixOf` s then dropDotSlash (drop 2 s) else s
    dropTrailing s = if length s > 1 && "/" `isSuffixOf` s then dropTrailing (init s) else s

-- ───────────────────────────────────────────────
-- tsconfig / jsconfig
-- ───────────────────────────────────────────────

-- | Parse a config (JSON with comments and trailing commas). @path@ is the
-- config's own corpus path. 'Nothing' when it has no @compilerOptions.paths@
-- or cannot be parsed.
parsePathsConfig :: FilePath -> BS.ByteString -> Maybe PathsConfig
parsePathsConfig path bytes = do
  v <- either (const Nothing) Just
         (eitherDecodeStrict' (TE.encodeUtf8 (stripJsonComments (TE.decodeUtf8With (\_ _ -> Just '?') bytes))))
  Object top <- Just v
  Object co <- KM.lookup "compilerOptions" top
  Object ps <- KM.lookup "paths" co
  let base = case KM.lookup "baseUrl" co of
        Just (String b) -> T.unpack b
        _               -> "."
      entries =
        [ (Key.toString k, [ T.unpack t | String t <- V.toList arr ])
        | (k, Array arr) <- KM.toList ps ]
  if null entries then Nothing else
    Just PathsConfig { pcDir = takeDirectory path, pcBaseUrl = base, pcPaths = entries }

-- | Remove @//@ and @/* */@ comments outside strings, and commas that
-- directly precede @}@ or @]@.
stripJsonComments :: T.Text -> T.Text
stripJsonComments = T.pack . dropTrailingCommas . go . T.unpack
  where
    go [] = []
    go ('"' : rest) = '"' : str rest
    go ('/' : '/' : rest) = go (dropWhile (/= '\n') rest)
    go ('/' : '*' : rest) = go (block rest)
    go (ch : rest) = ch : go rest
    str [] = []
    str ('\\' : ch : rest) = '\\' : ch : str rest
    str ('"' : rest) = '"' : go rest
    str (ch : rest) = ch : str rest
    block ('*' : '/' : rest) = rest
    block (_ : rest) = block rest
    block [] = []
    dropTrailingCommas [] = []
    dropTrailingCommas ('"' : rest) = '"' : inStr rest
    dropTrailingCommas (',' : rest)
      | (nxt : _) <- dropWhile isSpace rest, nxt `elem` ("}]" :: String) = dropTrailingCommas rest
    dropTrailingCommas (ch : rest) = ch : dropTrailingCommas rest
    inStr [] = []
    inStr ('\\' : ch : rest) = '\\' : ch : inStr rest
    inStr ('"' : rest) = '"' : dropTrailingCommas rest
    inStr (ch : rest) = ch : inStr rest

-- | Find and parse the @tsconfig.json@ / @jsconfig.json@ files of the corpus:
-- every directory holding an extracted TS/JS file, and its ancestors up to
-- the common root of those files, is probed. Unreadable or unparsable
-- configs are skipped.
loadPathsConfigs :: Extraction -> IO [PathsConfig]
loadPathsConfigs ext = fmap concat $ mapM load
  [ joinRel d name | d <- configDirs (Map.keys (cModules (buildCorpus ext)))
                   , name <- ["tsconfig.json", "jsconfig.json"] ]
  where
    load p = do
      r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Right bs -> maybe [] pure (parsePathsConfig p bs)
        Left _   -> []

-- | Directories to probe for configs: those of the TS/JS files and their
-- ancestors, never above the files' common root.
configDirs :: [FilePath] -> [FilePath]
configDirs files =
  let dirs = Set.toList (Set.fromList [ takeDirectory f | f <- files, isJsFamily f ])
      root = commonDir dirs
      ancestors d = takeWhile (\a -> length a >= length root) (iterate parentOf d)
      parentOf d = let p = takeDirectory d in if p == d then "" else p
  in if null dirs then [] else
       Set.toList (Set.fromList (concatMap (nub . takeWhile (not . null) . ancestors) dirs) `Set.union` Set.singleton root)

commonDir :: [FilePath] -> FilePath
commonDir [] = "."
commonDir ds =
  let parts = map splitDirectories ds
      common = foldr1 (\a b -> map fst (takeWhile (uncurry (==)) (zip a b))) parts
  in case common of
       [] -> "."
       cs -> foldr1 (\a b -> if a == "/" then a ++ b else a ++ "/" ++ b) cs

-- | Configs governing @importer@, nearest directory first.
configsFor :: [PathsConfig] -> FilePath -> [PathsConfig]
configsFor cfgs importer =
  sortOn (Down . length . splitDirectories . pcDir)
    [ c | c <- cfgs, under (pcDir c) importer ]
  where
    under "." _ = True
    under "" _  = True
    under d f   = (d ++ "/") `isPrefixOf` f

-- | Mapped locations for @spec@ under one config, best pattern first
-- (exact match, then the longest wildcard prefix).
pathsCandidates :: PathsConfig -> String -> [FilePath]
pathsCandidates c spec =
  let matches =
        [ (score, [ substitute t star | t <- targets ])
        | (pat, targets) <- pcPaths c
        , Just (score, star) <- [matchPattern pat spec] ]
      root = joinRel (pcDir c) (pcBaseUrl c)
  in [ joinRel root t | (_, ts) <- sortOn (Down . fst) matches, t <- ts ]
  where
    substitute t star = case break (== '*') t of
      (pre, '*' : post) -> pre ++ star ++ post
      _                 -> t

-- | @(score, wildcard text)@ when @spec@ matches @pat@.
matchPattern :: String -> String -> Maybe (Int, String)
matchPattern pat spec = case break (== '*') pat of
  (pre, '*' : post)
    | pre `isPrefixOf` spec && post `isSuffixOf` spec
      && length spec >= length pre + length post ->
        Just (length pre, take (length spec - length pre - length post) (drop (length pre) spec))
    | otherwise -> Nothing
  _ | pat == spec -> Just (maxBound, "")
    | otherwise -> Nothing

joinRel :: FilePath -> FilePath -> FilePath
joinRel d p
  | "/" `isPrefixOf` p = normalisePath p
  | d == "." || null d = normalisePath p
  | otherwise = normalisePath (d ++ "/" ++ p)

-- ───────────────────────────────────────────────
-- The pass
-- ───────────────────────────────────────────────

-- | What a placeholder node stands for.
data Placeholder
  = PRelative !FilePath  -- ^ unresolved path from a relative import
  | PExternal !String    -- ^ @external:@ specifier

placeholderOf :: Corpus -> Node -> Maybe Placeholder
placeholderOf c n =
  let fp = T.unpack (toText (nodeSourceFile n))
      kind = fmap toText (nodeKind n)
  in case () of
       _ | kind == Just "External", Just s <- stripPrefix' "external:" fp -> Just (PExternal s)
         -- A line-less module node is a placeholder unless it is the very
         -- node of its file: the same file under another path form
         -- (`src/x.ts` vs `./src/x.ts`) has another id and must be rewired.
         | kind == Just "Module", nodeLineStart n == Nothing
         , not ("external:" `isPrefixOf` fp)
         , Map.lookup (normalisePath fp) (cModules c) /= Just (nodeId n) -> Just (PRelative fp)
         | otherwise -> Nothing
  where
    stripPrefix' pre s = if pre `isPrefixOf` s then Just (drop (length pre) s) else Nothing

resolvePlaceholder :: Corpus -> [PathsConfig] -> FilePath -> Placeholder -> Maybe (FilePath, Rule)
resolvePlaceholder c _ _ (PRelative p) = fmap (\f -> (f, RExt)) (lookupPath c p)
resolvePlaceholder c cfgs importer (PExternal s)
  | "node:" `isPrefixOf` s = Nothing
  | otherwise = case viaPaths of
      Just f -> Just (f, RPaths)
      Nothing -> case alias of
        Just x  -> fmap (\f -> (f, RAlias)) (suffixMatch c x)
        Nothing
          | '/' `elem` s, not ("@" `isPrefixOf` s) -> fmap (\f -> (f, RPaths)) (suffixMatch c s)
          | otherwise -> Nothing
  where
    viaPaths = listToMaybe
      [ f | cfg <- configsFor cfgs importer, cand <- pathsCandidates cfg s
          , Just f <- [lookupPath c cand] ]
    alias
      | any (`isPrefixOf` s) ["@/", "~/", "#/"] = Just (drop 2 s)
      | "$lib/" `isPrefixOf` s = Just ("lib/" ++ drop 5 s)
      | otherwise = Nothing

-- | Rewrite placeholder targets to corpus module nodes (see module header).
resolveCorpusImports :: [PathsConfig] -> Extraction -> (Extraction, ResolutionStats)
resolveCorpusImports cfgs ext =
  let corpus = buildCorpus ext
      nodes = extractionNodes ext
      placeholders = Map.mapMaybe (placeholderOf corpus) nodes
      fileOf nid = fmap (normalisePath . T.unpack . toText . nodeSourceFile) (Map.lookup nid nodes)
      decide e = do
        ph <- Map.lookup (edgeTarget e) placeholders
        importer <- fileOf (edgeSource e)
        if not (isJsFamily importer) then Nothing else do
          (f, rule) <- resolvePlaceholder corpus cfgs importer ph
          tgt <- Map.lookup f (cModules corpus)
          if tgt == edgeSource e then Nothing else Just (tgt, rule)
      decided = Map.mapMaybe (\e -> fmap (\d -> (e, d)) (decide e)) (extractionEdges ext)
      rewritten = Map.map (\(e, (tgt, _)) -> e { edgeTarget = tgt }) decided
      edges' = Map.union rewritten (extractionEdges ext)
      touched = Set.fromList [ edgeTarget e | (e, _) <- Map.elems decided ]
      stillTargeted = Set.fromList (map edgeTarget (Map.elems edges'))
      stillSourced = Set.fromList (map edgeSource (Map.elems edges'))
      dropped = Set.filter (\nid -> not (Set.member nid stillTargeted) && not (Set.member nid stillSourced)) touched
      nodes' = Map.withoutKeys nodes dropped
      count r = length [ () | (_, (_, r')) <- Map.elems decided, r' == r ]
      stats = ResolutionStats
        { rsExtension = count RExt
        , rsAlias = count RAlias
        , rsPaths = count RPaths
        , rsDropped = Set.size dropped }
  in if Map.null placeholders
       then (ext, ResolutionStats 0 0 0 0)
       else (Extraction { extractionNodes = nodes', extractionEdges = edges', extractionErrors = extractionErrors ext }, stats)
