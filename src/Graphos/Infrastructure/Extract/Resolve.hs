{-# LANGUAGE OverloadedStrings #-}
-- | Declarative import resolution (custom-extractors 6.1–6.4).
--
-- The @resolve:@ map of a @graphos.yaml@ names, per extension, one of the
-- built-in resolver kinds:
--
--   * @package@     — dotted package/type name → source file under a root
--     (Java/Kotlin/Scala). An empty @roots@ list means the conventional roots
--     of the importing file (@src/main/java@, @src/main/kotlin@, …), which is
--     exactly what the compiled-in JVM handling uses.
--   * @psr4@        — PHP namespace → file under a PSR-4 root.
--   * @module-path@ — module name → path, driven by a source-of-truth index
--     file (@settings.gradle@, @pom.xml@, or a plain @name dir@ list).
--   * @path-prefix@ — import path → file by prefix concatenation (the
--     relative-path behaviour, made explicit).
--
-- A config entry overrides the built-in resolver registered for the same
-- extension; 'resolveOverrideWarnings' names the extension and the overridden
-- kind so the build can log it (6.3).
--
-- Relative @roots@ and @source-of-truth@ paths are read against the project
-- root the extraction was started from (the corpus keys are the extraction's
-- own, usually absolute, source paths).
--
-- The pass mirrors 'Graphos.Infrastructure.Extract.ImportResolution': it walks
-- the merged extraction once, rewrites the @imports@ edges whose target is an
-- @external:@ placeholder to the module node of the corpus file the specifier
-- denotes, and drops placeholders left without an edge. It reads the corpus
-- and the (pre-parsed) module indexes only — no extraction-time IO.
module Graphos.Infrastructure.Extract.Resolve
  ( -- * Built-ins and precedence
    builtinResolveConfigs
  , effectiveResolveConfigs
  , resolveOverrideWarnings
  , resolveKindName

    -- * Module index (module-path)
  , ModuleIndex
  , parseModuleIndex
  , loadModuleIndexes

    -- * The pass
  , ResolveStats(..)
  , resolveTotal
  , resolveSpec
  , resolveDeclarativeImports
  , resolveExtraction
  ) where

import Control.Exception (IOException, try)
import Control.Monad (guard)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf, nub, sort, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Short (toText)
import qualified Data.Text.IO as TIO
import System.FilePath (isAbsolute, takeDirectory, takeExtension, (</>))

import Graphos.Domain.Config (ResolveConfig(..), ResolveKind(..))
import Graphos.Domain.Types (Edge(..), Extraction(..), Node(..), NodeId)
import Graphos.Infrastructure.Extract.ImportResolution
  ( jsFamilyExtensions, normalisePath )

-- ───────────────────────────────────────────────
-- Built-ins and precedence
-- ───────────────────────────────────────────────

-- | The compiled-in resolver registrations exposed to @resolve:@ overrides.
--
-- These mirror the hardcoded per-language handling: JVM files resolve dotted
-- package names against the conventional Maven/Gradle source roots (an empty
-- @roots@ list selects that behaviour). A user entry for one of these
-- extensions replaces the built-in one and is reported by
-- 'resolveOverrideWarnings'.
builtinResolveConfigs :: Map String ResolveConfig
builtinResolveConfigs = Map.fromList
  [ (ext, ResolveConfig ResolvePackage [] Nothing)
  | ext <- [".java", ".kt", ".kts", ".scala"]
  ]

-- | Config entries win over the built-in registration for the same extension.
effectiveResolveConfigs :: Map String ResolveConfig -> Map String ResolveConfig
effectiveResolveConfigs user = Map.union user builtinResolveConfigs

-- | One warning per config entry that shadows a built-in resolver, naming the
-- extension, the resolver that wins and the built-in one it overrides
-- (import-resolution spec: "Resolution precedence"). The @overridden@ kind is
-- the built-in's, not the config entry's: a @path-prefix@ entry over a
-- @.java@ extension overrides the built-in @package@ resolver.
resolveOverrideWarnings :: Map String ResolveConfig -> [String]
resolveOverrideWarnings user =
  [ "resolve[\"" ++ ext ++ "\"]: config " ++ resolveKindName (rcKind rc)
      ++ " resolver overrides the built-in "
      ++ resolveKindName (rcKind builtin) ++ " resolver"
  | (ext, rc) <- Map.toAscList user
  , Just builtin <- [Map.lookup ext builtinResolveConfigs] ]

-- | The @resolve:@ spelling of a kind (@package@, @psr4@, @module-path@,
-- @path-prefix@).
resolveKindName :: ResolveKind -> String
resolveKindName = \case
  ResolvePackage    -> "package"
  ResolvePSR4       -> "psr4"
  ResolveModulePath -> "module-path"
  ResolvePathPrefix -> "path-prefix"

-- ───────────────────────────────────────────────
-- Module index (module-path)
-- ───────────────────────────────────────────────

-- | Module name → module directory, parsed from a source-of-truth file.
type ModuleIndex = Map String FilePath

-- | Parse a source-of-truth index file. Supported forms:
--
--   * Gradle: @include ':app', ':core'@ → modules @app@ and @core@ (their
--     directory is the module name, Gradle's default);
--   * Maven: @\<module\>app\</module\>@;
--   * plain: a line of the form @name dir@ (or just @name@).
--
-- Lines are comment-stripped (@//@) and blank forms ignored.
parseModuleIndex :: String -> [(String, FilePath)]
parseModuleIndex src =
  [ (name, dir)
  | rawLine <- lines src
  , (name, dir) <- entry (trim (stripLineComment rawLine))
  , not (null name)
  ]
  where
    entry line
      | "include" `isPrefixOf` map toLower line =
          [ (noColon q, noColon q) | q <- quotedStrings line ]
      | otherwise =
          [ (m, m) | m <- mavenModules line ]
          ++ plain
      where
        plain = case words line of
          (name : rest)
            | plainName name, not (null rest), all plainWord rest
            , not (any (`elem` ("=(){}\"" :: String)) line) -> [(name, unwords rest)]
          [name] | plainName name -> [(name, name)]
          _ -> []

    plainName name = not (null name) && all (\c -> isAlphaNum c || c `elem` ("._-" :: String)) name
    plainWord w = plainName w || w == "/"

    noColon q = dropWhile (== ':') (dropWhile isSpace q)

-- | Every quoted text of a line (single or double quotes).
quotedStrings :: String -> [String]
quotedStrings [] = []
quotedStrings ('"' : rest) = let (s, r) = break (== '"') rest in s : quotedStrings (drop 1 r)
quotedStrings ('\'' : rest) = let (s, r) = break (== '\'') rest in s : quotedStrings (drop 1 r)
quotedStrings (_ : rest) = quotedStrings rest

-- | Contents of every @\<module\>…\</module\>@ tag on a line.
mavenModules :: String -> [String]
mavenModules line = go line
  where
    go s = case breakOn "<module>" s of
      Nothing -> []
      Just (_, rest) ->
        let (m, rest') = break (== '<') rest
        in trim m : go rest'

breakOn :: String -> String -> Maybe (String, String)
breakOn pat = go ""
  where
    go _ [] = Nothing
    go acc s@(c : cs)
      | pat `isPrefixOf` s = Just (reverse acc, drop (length pat) s)
      | otherwise          = go (c : acc) cs

stripLineComment :: String -> String
stripLineComment l = maybe l fst (breakOn "//" l)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Read the source-of-truth index of every @module-path@ entry, keyed by
-- extension. A relative @source-of-truth@ is taken against the project root
-- (@root@), the same base the @roots@ of a resolver use. Missing or unreadable
-- files yield an empty index (resolution simply leaves the specifier external).
loadModuleIndexes :: FilePath -> Map String ResolveConfig -> IO (Map String ModuleIndex)
loadModuleIndexes root cfgs = fmap Map.fromList $ fmap concat $ mapM loadOne (Map.toList cfgs)
  where
    loadOne (ext, rc) = case (rcKind rc, rcSourceOfTruth rc) of
      (ResolveModulePath, Just f) | not (null f) -> do
        r <- try (TIO.readFile (absAgainst root f)) :: IO (Either IOException T.Text)
        pure $ case r of
          Right src ->
            let entries = parseModuleIndex (T.unpack src)
            in [ (ext, Map.fromList [ (n, normalisePath d) | (n, d) <- entries ])
               | not (null entries) ]
          Left _ -> []
      _ -> pure []

-- ───────────────────────────────────────────────
-- Corpus
-- ───────────────────────────────────────────────

-- | Corpus-relative file path (normalised) → its module node.
newtype Corpus = Corpus { cFiles :: Map FilePath NodeId }

buildCorpus :: Extraction -> Corpus
buildCorpus ext = Corpus $ Map.fromListWith min
  [ (normalisePath raw, nodeId n)
  | n <- Map.elems (extractionNodes ext)
  , fmap toText (nodeKind n) == Just "Module"
  , nodeLineStart n /= Nothing
  , let raw = T.unpack (toText (nodeSourceFile n))
  , not ("external:" `isPrefixOf` raw)
  ]

-- | The @external:@ specifier a placeholder node stands for, if any.
externalSpec :: Node -> Maybe String
externalSpec n
  | fmap toText (nodeKind n) == Just "External"
  , let raw = T.unpack (toText (nodeSourceFile n))
  , "external:" `isPrefixOf` raw
  , not (null (drop (length ("external:" :: String)) raw))
  = Just (drop (length ("external:" :: String)) raw)
  | otherwise = Nothing

-- ───────────────────────────────────────────────
-- Resolvers
-- ───────────────────────────────────────────────

-- | Resolve one specifier for one importer under one rule. Pure: the answer
-- is the first candidate present in the corpus. @root@ is the project root
-- that relative @roots@ / module directories are joined against (the corpus
-- keys are the extraction's own, possibly absolute, source paths).
resolveSpec
  :: FilePath -> Corpus -> ModuleIndex -> ResolveConfig -> FilePath -> String -> Maybe FilePath
resolveSpec root corpus index rc importer spec = case rcKind rc of
  ResolvePackage    -> packageResolver corpus (rootsFor root rc importer) (takeExtension importer) spec
  ResolvePSR4       -> psr4Resolver corpus (rooted root (rcRoots rc)) spec
  ResolveModulePath -> modulePathResolver corpus (Map.map (absAgainst root) index) (rooted root (rcRoots rc)) importer spec
  ResolvePathPrefix -> pathPrefixResolver root corpus (rooted root (rcRoots rc)) importer spec

-- | Configured roots, or the importing file's conventional JVM roots when the
-- entry lists none (the built-in behaviour).
rootsFor :: FilePath -> ResolveConfig -> FilePath -> [FilePath]
rootsFor root rc importer = case rooted root (rcRoots rc) of
  [] -> conventionalJvmRoots importer
  rs -> rs

jvmExtensions :: [String]
jvmExtensions = [".java", ".kt", ".scala"]

-- | Ancestors of the importer (nearest first) plus their conventional
-- @src/main|test/<lang>@ roots. Matches the compiled-in JVM handling.
conventionalJvmRoots :: FilePath -> [FilePath]
conventionalJvmRoots importer = nub (concatMap rootsOf (ancestorsOf (takeDirectory (normalisePath importer))))
  where
    rootsOf d = d :
      [ d </> suffix
      | suffix <- [ "src/main/java", "src/test/java"
                  , "src/main/kotlin", "src/test/kotlin"
                  , "src/main/scala", "src/test/scala" ] ]

ancestorsOf :: FilePath -> [FilePath]
ancestorsOf d =
  let p = takeDirectory d
  in if p == d || null d then [d] else d : ancestorsOf p

-- | @package@: dotted type name → type file. Static-import member suffixes
-- are retried with the last segment dropped, and @a.b.*@ expands to every
-- corpus file of that package.
packageResolver :: Corpus -> [FilePath] -> String -> String -> Maybe FilePath
packageResolver corpus roots importerExt spec =
  let static = dropStatic spec
      star = "*" `isSuffixOf` static
      dotted = if star then take (max 0 (length static - 2)) static else static
      segs = filter (not . null) (splitOnChar '.' dotted)
      exts = nub (importerExt : jvmExtensions)
      rel = intercalate "/" segs
      parentRel = if length segs > 1 then intercalate "/" (init segs) else ""
      candidates r
        | star =
            [ f
            | f <- sort (Map.keys (cFiles corpus))
            , takeDirectory f == joinRoot r rel
            , takeExtension f `elem` exts ]
        | otherwise =
            [ joinRoot r (rel ++ e) | e <- exts ]
              ++ [ joinRoot r (parentRel ++ e) | e <- exts, not (null parentRel) ]
  in listToMaybe
       [ f | r <- roots, f <- candidates r, Map.member f (cFiles corpus) ]

-- | @psr4@: PHP namespace → @.php@ file under a PSR-4 root.
psr4Resolver :: Corpus -> [FilePath] -> String -> Maybe FilePath
psr4Resolver corpus roots spec =
  let ns = dropWhile (== '\\') (dropStatic spec)
      segs = filter (not . null) (splitOnChar '\\' ns)
      rel = intercalate "/" segs
      parentRel = if length segs > 1 then intercalate "/" (init segs) else ""
      candidates r =
        [ joinRoot r (rel ++ ".php") ]
          ++ [ joinRoot r (parentRel ++ ".php") | not (null parentRel) ]
  in listToMaybe
       [ f | r <- roots, f <- candidates r, Map.member f (cFiles corpus) ]

-- | @module-path@: the longest dotted prefix naming a module of the
-- source-of-truth index selects the module directory; the remaining segments
-- form the path inside it (module directory, configured roots, then the
-- module's conventional source roots).
modulePathResolver :: Corpus -> ModuleIndex -> [FilePath] -> FilePath -> String -> Maybe FilePath
modulePathResolver corpus index roots importer spec =
  let segs = filter (not . null) (splitOnChar '.' (dropStatic spec))
      exts = nub (takeExtension importer : jvmExtensions)
      matches =
        [ (dir, rest)
        | n <- reverse [1 .. length segs]
        , Just dir <- [Map.lookup (intercalate "." (take n segs)) index]
        , let rest = drop n segs
        , not (null rest) ]
      bases = case matches of
        ((dir, _) : _) ->
          nub (dir : [ dir </> suffix
                     | suffix <- ["src/main/java", "src/main/kotlin", "src/main/scala"] ]
               ++ roots)
        [] -> roots
  in case matches of
       ((_, rest) : _) ->
         let rel = intercalate "/" rest
             candidates = [ joinRoot b (rel ++ e) | b <- bases, e <- exts ]
         in listToMaybe [ f | f <- candidates, Map.member f (cFiles corpus) ]
       [] -> Nothing

-- | @path-prefix@: the specifier itself is the path, an extension is appended
-- when it has none (importer extension, then the TS/JS family, then an
-- @index@ file).
pathPrefixResolver :: FilePath -> Corpus -> [FilePath] -> FilePath -> String -> Maybe FilePath
pathPrefixResolver root corpus roots importer spec =
  let p = normalisePath (dropStatic spec)
      rs = case roots of
        [] -> [absAgainst root "."]
        r -> r
      exts = nub (takeExtension importer : jsFamilyExtensions)
      hasExt = not (null (takeExtension p))
      candidates r =
        (if hasExt then [joinRoot r p] else [])
          ++ [ joinRoot r (p ++ e) | e <- exts ]
          ++ [ joinRoot r (p ++ "/index" ++ e) | e <- exts ]
  in listToMaybe
       [ f | r <- rs, f <- candidates r, Map.member f (cFiles corpus) ]

-- ───────────────────────────────────────────────
-- The pass
-- ───────────────────────────────────────────────

-- | What the declarative pass did, per kind, for the build log.
data ResolveStats = ResolveStats
  { rsPackage    :: !Int
  , rsPSR4       :: !Int
  , rsModulePath :: !Int
  , rsPathPrefix :: !Int
  , rsDropped    :: !Int
  } deriving (Eq, Show)

resolveTotal :: ResolveStats -> Int
resolveTotal s = rsPackage s + rsPSR4 s + rsModulePath s + rsPathPrefix s

-- | Rewrite @external:@ import targets to the corpus module nodes the
-- @resolve:@ rules denote (see the module header). @root@ is the project root
-- relative @roots@ and module @source-of-truth@ files are resolved against.
-- Returns the extraction unchanged when it has no @external:@ placeholder at
-- all.
resolveDeclarativeImports
  :: FilePath -> Map String ResolveConfig -> Map String ModuleIndex -> Extraction -> (Extraction, ResolveStats)
resolveDeclarativeImports root userCfg indexes ext =
  let rules = effectiveResolveConfigs userCfg
      corpus = buildCorpus ext
      nodes = extractionNodes ext
      placeholders = Map.mapMaybe externalSpec nodes
      importerOf nid =
        normalisePath . T.unpack . toText . nodeSourceFile <$> Map.lookup nid nodes
      decide e = do
        spec <- Map.lookup (edgeTarget e) placeholders
        importer <- importerOf (edgeSource e)
        let extOf = takeExtension importer
        rule <- Map.lookup extOf rules
        targetPath <- resolveSpec root corpus (Map.findWithDefault Map.empty extOf indexes) rule importer spec
        target <- Map.lookup targetPath (cFiles corpus)
        guard (target /= edgeSource e)
        pure (target, rcKind rule)
      decided = Map.mapMaybe (\e -> fmap (\d -> (e, d)) (decide e)) (extractionEdges ext)
      rewritten = Map.map (\(e, (target, _)) -> e { edgeTarget = target }) decided
      edges' = Map.union rewritten (extractionEdges ext)
      touched = Set.fromList [ edgeTarget e | (e, _) <- Map.elems decided ]
      stillTargeted = Set.fromList (map edgeTarget (Map.elems edges'))
      stillSourced = Set.fromList (map edgeSource (Map.elems edges'))
      dropped = Set.filter
        (\nid -> not (Set.member nid stillTargeted) && not (Set.member nid stillSourced)) touched
      nodes' = Map.withoutKeys nodes dropped
      count k = length [ () | (_, (_, k')) <- Map.elems decided, k' == k ]
      stats = ResolveStats
        { rsPackage = count ResolvePackage
        , rsPSR4 = count ResolvePSR4
        , rsModulePath = count ResolveModulePath
        , rsPathPrefix = count ResolvePathPrefix
        , rsDropped = Set.size dropped }
  in if Map.null placeholders
       then (ext, ResolveStats 0 0 0 0 0)
       else (ext { extractionNodes = nodes', extractionEdges = edges' }, stats)

-- | Load the module indexes the config needs, then run the pass. This is the
-- entry point the extraction orchestration uses; @root@ is the canonical
-- project root the config-relative paths are based on.
resolveExtraction
  :: FilePath -> Map String ResolveConfig -> Extraction -> IO (Extraction, ResolveStats)
resolveExtraction root userCfg ext = do
  indexes <- loadModuleIndexes root (effectiveResolveConfigs userCfg)
  pure (resolveDeclarativeImports root userCfg indexes ext)

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

dropStatic :: String -> String
dropStatic s =
  let t = dropWhile isSpace s
  in maybe t id (stripPrefix "static " t)

splitOnChar :: Char -> String -> [String]
splitOnChar ch = go
  where
    go s = case break (== ch) s of
      (a, [])       -> [a]
      (a, _ : rest) -> a : go rest

-- | Join a (possibly @.@) root with a corpus-relative path, normalised.
joinRoot :: FilePath -> FilePath -> FilePath
joinRoot r p
  | r == "." || null r = normalisePath p
  | otherwise          = normalisePath (r ++ "/" ++ p)

-- | Drop empty entries of a config list (YAML may leave them) and make each
-- entry relative to the project @root@ unless it is already absolute.
rooted :: FilePath -> [FilePath] -> [FilePath]
rooted root = map (absAgainst root) . filter (not . null)

-- | @p@ against @root@: absolute (and @.@-rooted) paths are kept as written,
-- otherwise the project root prefixes the entry.
absAgainst :: FilePath -> FilePath -> FilePath
absAgainst root p
  | isAbsolute p || root == "." || null root = p
  | otherwise                                = normalisePath (root ++ "/" ++ p)
