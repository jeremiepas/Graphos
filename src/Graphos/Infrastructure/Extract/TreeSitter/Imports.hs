{-# LANGUAGE OverloadedStrings #-}
-- | @imports@ edges for Go, Rust and Ruby (tree-sitter mode).
--
-- TypeScript import edges are derived purely from the specifier text
-- ('Graphos.Infrastructure.Extract.TreeSitter.Resolver'). Go, Rust and Ruby
-- need repository context to tell an internal import from an external one —
-- the nearest @go.mod@, the Cargo manifests of the crate and its workspace,
-- the files on disk — so resolution here runs in IO, after the pure AST
-- conversion, and appends its edges to the file's 'Extraction'.
--
-- Two steps:
--
--   1. 'collectImportSites' (pure) reads the import sites off the AST: Go
--      @import_spec@s, Rust @use_declaration@s (anywhere, including function
--      bodies and inline modules), Ruby @require@ / @require_relative@ calls
--      with a literal string argument.
--   2. 'resolveImportSites' (IO) maps each site to its imported units and
--      'importEdgesFor' turns them into edges of the TypeScript shape:
--      @<file module node> -imports-> <placeholder>@, where the placeholder's
--      @source_file@ is the imported repo file (internal) or
--      @external:<spec>@ (external).
--
-- Go imports a /package/ (a directory). An internal package import becomes
-- one edge per non-test @.go@ file of the package directory, each with
-- weight @1/n@ and @package_files = n@ in the edge metadata, so that every
-- edge target is a file and file-level consumers keep working.
module Graphos.Infrastructure.Extract.TreeSitter.Imports
  ( -- * Import sites (pure)
    ImportLang(..)
  , ImportSite(..)
  , importLangForGrammar
  , collectImportSites
    -- * Resolution (IO)
  , ImportTarget(..)
  , ResolvedImport(..)
  , resolveImportSites
    -- * Edges (pure)
  , importEdgesFor
  , withImportEdges
    -- * Exposed for tests
  , UsePath(..)
  , parseUseTree
  , parseGoModule
  , CargoManifest(..)
  , parseCargoManifest
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum, isSpace, isAlpha)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Short (fromText)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, takeFileName, (</>), (<.>))

import Graphos.Domain.Types
  ( Node(..), Edge(..), Extraction(..), NodeId, FileType(..), Relation(..)
  , Confidence(..), EdgeId(..), bitNodeKind
  )
import Graphos.Infrastructure.Extract.TreeSitter.Core (TSNodeInfo(..))
import Graphos.Infrastructure.Extract.TreeSitter.Convert (makeNodeId, fileModuleNode)
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveDots)

-- ───────────────────────────────────────────────
-- Import sites
-- ───────────────────────────────────────────────

data ImportLang = LangGo | LangRust | LangRuby
  deriving (Eq, Show)

-- | Languages handled here, by tree-sitter grammar name.
importLangForGrammar :: String -> Maybe ImportLang
importLangForGrammar "go"   = Just LangGo
importLangForGrammar "rust" = Just LangRust
importLangForGrammar "ruby" = Just LangRuby
importLangForGrammar _      = Nothing

-- | One import statement (Go: one @import_spec@).
data ImportSite = ImportSite
  { siteLang       :: !ImportLang
  , siteVia        :: !Text          -- ^ @import@, @use@, @require@, @require_relative@
  , siteSpec       :: !Text          -- ^ Go path / Rust use-tree text / Ruby string, unquoted
  , siteAlias      :: !(Maybe Text)  -- ^ Go import name (@m@, @_@, @.@)
  , siteLine       :: !Int           -- ^ 1-based
  , siteInlineMods :: ![Text]        -- ^ Rust: enclosing inline @mod x { }@, outermost first
  } deriving (Eq, Show)

-- | Read the import sites of one file's AST.
collectImportSites :: ImportLang -> [TSNodeInfo] -> [ImportSite]
collectImportSites lang = concatMap (go [])
  where
    go mods n = case (lang, tsnType n) of
      (LangGo, "import_spec") -> maybe [] pure (goSite n)
      (LangRust, "use_declaration") -> maybe [] pure (rustSite mods n)
      (LangRust, "mod_item")
        | Just name <- inlineModName n -> concatMap (go (mods ++ [name])) (tsnChildren n)
      (LangRuby, t)
        | t `elem` ["call", "method_call"], Just s <- rubySite n ->
            s : concatMap (go mods) (tsnChildren n)
      _ -> concatMap (go mods) (tsnChildren n)

    line n = tsnStartRow n + 1

    goSite n =
      let named = filter tsnIsNamed (tsnChildren n)
          pathNode = listToMaybe [c | c <- named, tsnType c `elem` ["interpreted_string_literal", "raw_string_literal"]]
          alias = listToMaybe [tsnText c | c <- named, tsnType c `elem` ["package_identifier", "blank_identifier", "dot"]]
      in case pathNode of
           Just p | let spec = unquote (tsnText p), validSpec spec ->
             Just (ImportSite LangGo "import" spec alias (line n) [])
           _ -> Nothing

    rustSite mods n =
      case [c | c <- tsnChildren n, tsnIsNamed c, tsnType c `notElem` ["visibility_modifier", "attribute_item"]] of
        (arg : _) -> Just (ImportSite LangRust "use" (normalizeSpace (tsnText arg)) Nothing (line n) mods)
        [] -> Nothing

    inlineModName n
      | any ((== "declaration_list") . tsnType) (tsnChildren n) =
          listToMaybe [stripRaw (tsnText c) | c <- tsnChildren n, tsnType c == "identifier"]
      | otherwise = Nothing

    rubySite n = case tsnChildren n of
      (fn : rest)
        | tsnType fn == "identifier", tsnText fn `elem` ["require", "require_relative"]
        , not (any (\c -> tsnType c `elem` [".", "&.", "::"]) rest) ->
            case [a | al <- rest, tsnType al == "argument_list", a <- filter tsnIsNamed (tsnChildren al)] of
              (s : _) | tsnType s == "string"
                      , not (any ((== "interpolation") . tsnType) (tsnChildren s))
                      , let spec = unquote (tsnText s), validSpec spec ->
                  Just (ImportSite LangRuby (tsnText fn) spec Nothing (line n) [])
              _ -> Nothing
      _ -> Nothing

-- | Strip one pair of surrounding quotes / backticks.
unquote :: Text -> Text
unquote t
  | T.length t >= 2, T.head t `elem` q, T.last t == T.head t = T.init (T.tail t)
  | otherwise = t
  where q = "'\"`" :: String

-- | A specifier usable as a target: non-empty, no whitespace, quotes or
-- control characters.
validSpec :: Text -> Bool
validSpec s = not (T.null s) && T.all ok s
  where ok c = not (isSpace c) && c `notElem` ("'\"`{}#$" :: String) && c >= ' '

normalizeSpace :: Text -> Text
normalizeSpace = T.unwords . T.words

stripRaw :: Text -> Text
stripRaw t = fromMaybe t (T.stripPrefix "r#" t)

-- ───────────────────────────────────────────────
-- Resolution
-- ───────────────────────────────────────────────

-- | What an imported unit points at.
data ImportTarget
  = TargetFiles ![FilePath]  -- ^ repo file(s); several for a Go package
  | TargetExternal !Text     -- ^ package / crate / gem / stdlib
  deriving (Eq, Show)

data ResolvedImport = ResolvedImport
  { riSite   :: !ImportSite
  , riUnit   :: !Text           -- ^ the imported unit (Go path, Rust path, Ruby spec)
  , riTarget :: !ImportTarget
  } deriving (Eq, Show)

-- | Resolve the sites of @file@. Units that cannot be classified (a Rust
-- path whose first segment is a local item, a relative require to a missing
-- file) are dropped rather than turned into placeholder targets.
resolveImportSites :: FilePath -> [ImportSite] -> IO [ResolvedImport]
resolveImportSites _ [] = pure []
resolveImportSites file sites@(s0 : _) = case siteLang s0 of
  LangGo   -> do
    gm <- findGoModule (takeDirectory file)
    concat <$> mapM (resolveGo gm) sites
  LangRust -> do
    mctx <- loadRustContext file
    case mctx of
      Nothing  -> pure []
      Just ctx -> concat <$> mapM (resolveRust ctx file) sites
  LangRuby -> do
    root <- findRubyRoot (takeDirectory file)
    catMaybes <$> mapM (resolveRuby root file) sites

-- | Walk up from @dir@ (inclusive) to the filesystem root.
ancestors :: FilePath -> [FilePath]
ancestors d =
  let p = takeDirectory d
  in if p == d || null d then [d] else d : ancestors p

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting = fmap listToMaybe . filterM doesFileExist

readTextSafe :: FilePath -> IO (Maybe Text)
readTextSafe p = do
  r <- try (TIO.readFile p) :: IO (Either SomeException Text)
  pure (either (const Nothing) Just r)

-- Go ──────────────────────────────────────────────

-- | The @module@ path declared by a go.mod file.
parseGoModule :: Text -> Maybe Text
parseGoModule src = listToMaybe
  [ unquote rest
  | l <- T.lines src
  , Just r <- [T.stripPrefix "module" (T.strip l)]
  , maybe False (\(c, _) -> isSpace c || c == '"') (T.uncons r)
  , let rest = T.strip (fst (T.breakOn "//" r))
  , not (T.null rest)
  ]

-- | Nearest go.mod at or above @dir@: (module directory, module path).
findGoModule :: FilePath -> IO (Maybe (FilePath, Text))
findGoModule dir = go (ancestors dir)
  where
    go [] = pure Nothing
    go (d : ds) = do
      let gm = d </> "go.mod"
      ex <- doesFileExist gm
      if not ex then go ds else do
        src <- readTextSafe gm
        case src >>= parseGoModule of
          Just m  -> pure (Just (d, m))
          Nothing -> go ds

resolveGo :: Maybe (FilePath, Text) -> ImportSite -> IO [ResolvedImport]
resolveGo gm site =
  let spec = siteSpec site
      ext = pure [ResolvedImport site spec (TargetExternal spec)]
  in case gm of
       Just (modDir, modPath)
         | spec == modPath || (modPath <> "/") `T.isPrefixOf` spec -> do
             let rel = T.unpack (T.drop (T.length modPath + 1) spec)
                 pkgDir = if null rel then modDir else modDir </> rel
             files <- goPackageFiles pkgDir
             pure [ResolvedImport site spec (TargetFiles files) | not (null files)]
       _ -> ext

-- | Non-test .go files of a package directory, sorted.
goPackageFiles :: FilePath -> IO [FilePath]
goPackageFiles dir = do
  isDir <- doesDirectoryExist dir
  if not isDir then pure [] else do
    entries <- listDirectory dir
    let cands = [dir </> e | e <- sort entries, takeExtension e == ".go", not ("_test.go" `isSuffixOf` e)]
    filterM doesFileExist cands

-- Ruby ────────────────────────────────────────────

-- | Project root for @require@: the nearest directory holding a Gemfile, a
-- gemspec or a .git entry.
findRubyRoot :: FilePath -> IO (Maybe FilePath)
findRubyRoot dir = go (ancestors dir)
  where
    go [] = pure Nothing
    go (d : ds) = do
      gemfile <- doesFileExist (d </> "Gemfile")
      git <- (||) <$> doesDirectoryExist (d </> ".git") <*> doesFileExist (d </> ".git")
      spec <- if gemfile || git then pure False else hasGemspec d
      if gemfile || git || spec then pure (Just d) else go ds
    hasGemspec d = do
      r <- try (listDirectory d) :: IO (Either SomeException [FilePath])
      pure (either (const False) (any ((== ".gemspec") . takeExtension)) r)

resolveRuby :: Maybe FilePath -> FilePath -> ImportSite -> IO (Maybe ResolvedImport)
resolveRuby mRoot file site
  | siteVia site == "require_relative" || "./" `T.isPrefixOf` spec || "../" `T.isPrefixOf` spec =
      fmap internal <$> firstExisting [resolveDots (takeDirectory file </> withRb)]
  | "/" `T.isPrefixOf` spec = fmap internal <$> firstExisting [withRb]
  | otherwise = do
      -- Load-path candidates, in order: lib/ directories enclosing the file
      -- (a nested gem requiring its own files), <root>/lib, <root>, then a
      -- gem vendored as <root>/lib/<name>/ (its own load root, or with an
      -- inner lib/), <name> being the first segment of the specifier.
      let gemDir = takeWhile (/= '/') (T.unpack spec)
          libDirs = [d | d <- ancestors (takeDirectory file), takeFileName d == "lib", inRoot d]
                    ++ maybe [] (\r -> [ r </> "lib", r
                                       , r </> "lib" </> gemDir, r </> "lib" </> gemDir </> "lib" ]) mRoot
      found <- firstExisting [d </> withRb | d <- libDirs]
      pure $ Just $ maybe (ResolvedImport site spec (TargetExternal spec)) internal found
  where
    spec = siteSpec site
    withRb = let p = T.unpack spec in if takeExtension p == ".rb" then p else p <.> "rb"
    internal p = ResolvedImport site spec (TargetFiles [p])
    inRoot d = maybe True (\r -> d == r || under r d) mRoot

-- Rust ────────────────────────────────────────────

-- | One path of an expanded use tree.
data UsePath = UsePath
  { upAbsolute :: !Bool    -- ^ leading @::@
  , upSegs     :: ![Text]  -- ^ segments, without a trailing @self@ or @*@
  , upGlob     :: !Bool
  , upAlias    :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Expand a use-tree (@a::{b, c::d as e, f::*}@) into its paths. Returns
-- @[]@ for anything that does not tokenize as a use tree.
parseUseTree :: Text -> [UsePath]
parseUseTree raw = case tokenize (stripComments raw) of
  Nothing -> []
  Just ("::" : ts) -> map (\u -> u { upAbsolute = True }) (fst (pTree [] ts))
  Just ts -> fst (pTree [] ts)
  where
    mk segs glob alias =
      let segs' = if not (null segs) && last segs == "self" && length segs > 1 then init segs else segs
      in UsePath False segs' glob alias
    pTree pre ("{" : ts) = pList pre ts
    pTree pre ("*" : ts) = ([mk pre True Nothing], ts)
    pTree pre (x : ts) | isIdent x = pAfter (pre ++ [stripRaw x]) ts
    pTree _ ts = ([], ts)
    pAfter segs ("::" : ts) = pTree segs ts
    pAfter segs ("as" : a : ts) | isIdent a = ([mk segs False (Just a)], ts)
    pAfter segs ts = ([mk segs False Nothing], ts)
    pList pre = goL []
      where
        goL acc ("}" : rest) = (acc, rest)
        goL acc ts = case pTree pre ts of
          (ps, "," : rest) -> goL (acc ++ ps) rest
          (ps, "}" : rest) -> (acc ++ ps, rest)
          (ps, _)          -> (acc ++ ps, [])
    isIdent x = case T.uncons x of
      Just (c, _) -> isAlpha c || c == '_' || c == 'r'
      Nothing     -> False

-- | Split a use tree into @::@, @{@, @}@, @,@, @*@ and identifiers.
tokenize :: Text -> Maybe [Text]
tokenize t0 = go (T.stripStart t0)
  where
    go t
      | T.null t = Just []
      | "::" `T.isPrefixOf` t = ("::" :) <$> go (T.stripStart (T.drop 2 t))
      | T.head t `elem` ("{},*" :: String) = (T.singleton (T.head t) :) <$> go (T.stripStart (T.tail t))
      | isIdentChar (T.head t) =
          let (w, rest) = T.span (\c -> isIdentChar c || c == '#') t
          in (w :) <$> go (T.stripStart rest)
      | otherwise = Nothing
    isIdentChar c = isAlphaNum c || c == '_'

-- | Remove @//@ and @/* */@ comments.
stripComments :: Text -> Text
stripComments t
  | T.null t = t
  | "//" `T.isPrefixOf` t = stripComments (T.dropWhile (/= '\n') t)
  | "/*" `T.isPrefixOf` t = stripComments (T.drop 2 (snd (T.breakOn "*/" (T.drop 2 t))))
  | otherwise = let (a, b) = T.break (== '/') t
                in if T.null b then a
                   else if "//" `T.isPrefixOf` b || "/*" `T.isPrefixOf` b
                     then a <> " " <> stripComments b
                     else a <> "/" <> stripComments (T.tail b)

-- | The parts of a Cargo.toml this resolver needs.
data CargoManifest = CargoManifest
  { cmPackageName :: !(Maybe Text)
  , cmLibName     :: !(Maybe Text)
  , cmLibPath     :: !(Maybe Text)
  , cmIsWorkspace :: !Bool
  , cmDeps        :: !(Map.Map Text DepSpec)  -- ^ crate deps, keyed by code name (@-@ → @_@)
  , cmWsDeps      :: !(Map.Map Text DepSpec)  -- ^ @[workspace.dependencies]@
  } deriving (Eq, Show)

data DepSpec = DepSpec
  { dsPath      :: !(Maybe Text)
  , dsWorkspace :: !Bool
  } deriving (Eq, Show)

-- | Line-oriented reading of the Cargo.toml keys used for resolution.
parseCargoManifest :: Text -> CargoManifest
parseCargoManifest src = snd (foldl step ("", CargoManifest Nothing Nothing Nothing False Map.empty Map.empty) (T.lines src))
  where
    step (sec, cm) l0 =
      let l = T.strip (stripTomlComment l0)
      in if "[" `T.isPrefixOf` l
           then let s = T.strip (T.dropAround (\c -> c == '[' || c == ']') l)
                    cm' = case depTableName s of
                      Just (ws, name) -> addDep ws name (DepSpec Nothing False) cm
                      Nothing -> cm
                in (s, if s == "workspace" then cm' { cmIsWorkspace = True } else cm')
           else case T.breakOn "=" l of
             (k0, v0) | not (T.null v0) ->
               let k = T.strip (unquote (T.strip k0))
                   v = T.strip (T.drop 1 v0)
               in (sec, assign sec k v cm)
             _ -> (sec, cm)

    assign sec k v cm
      | sec == "package", k == "name" = cm { cmPackageName = Just (unquote v) }
      | sec == "lib", k == "name" = cm { cmLibName = Just (unquote v) }
      | sec == "lib", k == "path" = cm { cmLibPath = Just (unquote v) }
      | isDepSection sec = addDep (sec == "workspace.dependencies") k (inlineDep v) cm
      | Just (ws, name) <- depTableName sec = case k of
          "path"      -> updDep ws name (\d -> d { dsPath = Just (unquote v) }) cm
          "workspace" -> updDep ws name (\d -> d { dsWorkspace = v == "true" }) cm
          _           -> cm
      | otherwise = cm

    isDepSection s = any (`T.isSuffixOf` s) ["dependencies"] && not ("." `T.isSuffixOf` s)
    -- "[dependencies.foo]" / "[target.'x'.dev-dependencies.foo]" / "[workspace.dependencies.foo]"
    depTableName s =
      case T.breakOnEnd "dependencies." s of
        (pre, name) | not (T.null pre), not (T.null name), not (T.any (== '.') name) ->
          Just ("workspace.dependencies." `T.isPrefixOf` s, unquote name)
        _ -> Nothing

    inlineDep v
      | "{" `T.isPrefixOf` v = DepSpec (tomlKey "path" v) (tomlKey "workspace" v == Just "true")
      | otherwise = DepSpec Nothing False

    tomlKey key v =
      let body = T.dropAround (\c -> c == '{' || c == '}') v
          kvs = [ (T.strip a, T.strip (T.drop 1 b))
                | part <- T.splitOn "," body, let (a, b) = T.breakOn "=" part, not (T.null b) ]
      in unquote <$> lookup key kvs

    addDep ws name d cm =
      let k = T.replace "-" "_" name
      in if ws then cm { cmWsDeps = Map.insertWith (\_ old -> old) k d (cmWsDeps cm) }
               else cm { cmDeps = Map.insertWith (\_ old -> old) k d (cmDeps cm) }
    updDep ws name f cm =
      let k = T.replace "-" "_" name
      in if ws then cm { cmWsDeps = Map.adjust f k (cmWsDeps cm) }
               else cm { cmDeps = Map.adjust f k (cmDeps cm) }

    stripTomlComment l = case T.breakOn "#" l of
      (a, b) | T.null b -> a
             | even (T.count "\"" a) -> a
             | otherwise -> l

-- | A module: the file holding it, the directory its @mod x;@ children live
-- in, and its parent (Nothing for a crate root).
data RMod = RMod
  { rmFile     :: !FilePath
  , rmChildDir :: !FilePath
  , rmParent   :: !(Maybe RMod)
  }

data RustContext = RustContext
  { rcRoot       :: !RMod                        -- ^ crate root of the current file
  , rcFileMod    :: !RMod                        -- ^ module of the current file
  , rcCrates     :: !(Map.Map Text FilePath)     -- ^ workspace/path crates → lib root file
  , rcExternals  :: ![Text]                      -- ^ known external crate names
  }

stdCrates :: [Text]
stdCrates = ["std", "core", "alloc", "proc_macro", "test"]

-- | Crate context of a Rust file: its Cargo package, crate root, module,
-- and the path/workspace crates it can name.
loadRustContext :: FilePath -> IO (Maybe RustContext)
loadRustContext file = do
  mCrate <- findFileUp "Cargo.toml" (takeDirectory file)
  case mCrate of
    Nothing -> pure Nothing
    Just crateToml -> do
      cm <- maybe (parseCargoManifest "") parseCargoManifest <$> readTextSafe crateToml
      let crateDir = takeDirectory crateToml
          srcDir = crateDir </> "src"
      wsDeps <- if cmIsWorkspace cm then pure (cmWsDeps cm, crateDir) else do
        mWs <- findWorkspace (takeDirectory crateDir)
        pure (fromMaybe (Map.empty, crateDir) mWs)
      ownLib <- libRootOf crateDir cm
      let ownNames = mapMaybe id [ T.replace "-" "_" <$> cmLibName cm
                                 , T.replace "-" "_" <$> cmPackageName cm ]
      depCrates <- fmap catMaybes $ mapM (depCrate crateDir wsDeps) (Map.toList (cmDeps cm))
      root <- crateRootOf srcDir file ownLib
      fm <- fileModule srcDir root file
      let crates = Map.fromList ([(n, l) | Just l <- [ownLib], n <- ownNames] ++ depCrates)
      pure $ Just RustContext
        { rcRoot = root
        , rcFileMod = fm
        , rcCrates = crates
        , rcExternals = Map.keys (cmDeps cm) ++ Map.keys (fst wsDeps)
        }
  where
    -- nearest Cargo.toml with a [workspace] table above the crate
    findWorkspace d = go (ancestors d)
      where
        go [] = pure Nothing
        go (a : as) = do
          cm <- fmap parseCargoManifest <$> readTextSafe (a </> "Cargo.toml")
          case cm of
            Just m | cmIsWorkspace m -> pure (Just (cmWsDeps m, a))
            _ -> go as
    depCrate crateDir (wsDeps, wsDir) (name, ds) = do
      let mDir = case (dsPath ds, dsWorkspace ds) of
            (Just p, _) -> Just (resolveDots (crateDir </> T.unpack p))
            (Nothing, True) -> case Map.lookup name wsDeps >>= dsPath of
              Just p  -> Just (resolveDots (wsDir </> T.unpack p))
              Nothing -> Nothing
            _ -> Nothing
      case mDir of
        Nothing -> pure Nothing
        Just dir -> do
          cm <- maybe (parseCargoManifest "") parseCargoManifest <$> readTextSafe (dir </> "Cargo.toml")
          fmap (\l -> (name, l)) <$> libRootOf dir cm

findFileUp :: FilePath -> FilePath -> IO (Maybe FilePath)
findFileUp name dir = firstExisting [d </> name | d <- ancestors dir]

libRootOf :: FilePath -> CargoManifest -> IO (Maybe FilePath)
libRootOf dir cm = firstExisting (maybe [] (\p -> [resolveDots (dir </> T.unpack p)]) (cmLibPath cm) ++ [dir </> "src" </> "lib.rs"])

rootMod :: FilePath -> RMod
rootMod f = RMod f (takeDirectory f) Nothing

-- | The crate root a file belongs to: itself when it is a root (lib.rs,
-- main.rs, src/bin/*.rs, a file outside src/ such as tests/x.rs or build.rs),
-- otherwise the library root, else src/main.rs.
crateRootOf :: FilePath -> FilePath -> Maybe FilePath -> IO RMod
crateRootOf srcDir file ownLib
  | isRootFile srcDir file = pure (rootMod file)
  | otherwise = do
      m <- firstExisting (maybe [] pure ownLib ++ [srcDir </> "lib.rs", srcDir </> "main.rs"])
      pure (rootMod (fromMaybe file m))

isRootFile :: FilePath -> FilePath -> Bool
isRootFile srcDir file =
  not (under srcDir file)
    || file `elem` [srcDir </> "lib.rs", srcDir </> "main.rs"]
    || takeDirectory file == srcDir </> "bin"
    || (takeFileName file == "main.rs" && takeDirectory (takeDirectory file) == srcDir </> "bin")

under :: FilePath -> FilePath -> Bool
under dir f = take (length dir + 1) f == dir ++ "/"

-- | Module of a file, with its parent chain derived from the directory layout.
fileModule :: FilePath -> RMod -> FilePath -> IO RMod
fileModule srcDir root file
  | isRootFile srcDir file = pure (rootMod file)
  | takeFileName file == "mod.rs" = do
      let d = takeDirectory file
      parent <- dirModule (takeDirectory d)
      pure (RMod file d (Just parent))
  | otherwise = do
      let d = takeDirectory file
      parent <- dirModule d
      pure (RMod file (d </> takeBaseName file) (Just parent))
  where
    -- the module whose children live in directory @d@
    dirModule d
      | d == srcDir || not (under srcDir d) = pure root
      | otherwise = do
          m <- firstExisting [d <.> "rs", d </> "mod.rs"]
          case m of
            Just f -> fileModule srcDir root f
            Nothing -> do
              parent <- dirModule (takeDirectory d)
              pure (RMod (rmFile parent) d (Just parent))

childModule :: RMod -> Text -> IO (Maybe RMod)
childModule m seg = do
  let base = rmChildDir m </> T.unpack seg
  f <- firstExisting [base <.> "rs", base </> "mod.rs"]
  pure ((\p -> RMod p base (Just m)) <$> f)

-- | Module at a position: the file module plus enclosing inline modules.
moduleAt :: RustContext -> [Text] -> RMod
moduleAt ctx = foldl (\m name -> RMod (rmFile m) (rmChildDir m </> T.unpack name) (Just m)) (rcFileMod ctx)

resolveRust :: RustContext -> FilePath -> ImportSite -> IO [ResolvedImport]
resolveRust ctx file site = catMaybes <$> mapM one (parseUseTree (siteSpec site))
  where
    here = moduleAt ctx (siteInlineMods site)
    unitOf up = (if upAbsolute up then "::" else "") <> T.intercalate "::" (upSegs up) <> (if upGlob up then "::*" else "")
    ext up name = pure (Just (ResolvedImport site (unitOf up) (TargetExternal name)))
    one up = case upSegs up of
      [] -> pure Nothing
      (first : rest)
        | not (all validSpec (upSegs up)) -> pure Nothing
        | upAbsolute up -> case Map.lookup first (rcCrates ctx) of
            Just lib -> walkFrom up (rootMod lib) rest
            Nothing  -> ext up first
        | first == "crate" -> walkFrom up (rcRoot ctx) rest
        | first == "self"  -> walkFrom up here rest
        | first == "super" -> let (n, rest') = countSupers (first : rest)
                              in case nthParent n here of
                                   Just m  -> walkFrom up m rest'
                                   Nothing -> pure Nothing
        | otherwise -> do
            child <- childModule here first
            case child of
              Just _ -> walkFrom up here (first : rest)
              Nothing -> case Map.lookup first (rcCrates ctx) of
                Just lib -> walkFrom up (rootMod lib) rest
                Nothing
                  | first `elem` stdCrates || first `elem` rcExternals ctx -> ext up first
                  | otherwise -> pure Nothing   -- a local item (enum variant, type in scope)
    countSupers segs = let (s, r) = span (== "super") segs in (length s, r)
    nthParent :: Int -> RMod -> Maybe RMod
    nthParent 0 m = Just m
    nthParent n m = rmParent m >>= nthParent (n - 1)
    walkFrom up m segs = do
      target <- walk m segs
      pure $ if rmFile target == file
        then Nothing
        else Just (ResolvedImport site (unitOf up) (TargetFiles [rmFile target]))
    walk m [] = pure m
    walk m ("self" : _) = pure m
    walk m (s : ss) = do
      c <- childModule m s
      maybe (pure m) (`walk` ss) c

-- ───────────────────────────────────────────────
-- Edges
-- ───────────────────────────────────────────────

-- | Edges (and their target placeholders) from the module node @src@.
-- Shape matches the TypeScript import edges: internal targets are keyed by
-- the resolved file and carry it as @source_file@; external targets are
-- keyed under @external@ with @source_file = external:<name>@.
importEdgesFor :: NodeId -> [ResolvedImport] -> ([Edge], [Node])
importEdgesFor src rs = unzip (concatMap edgesOf rs)
  where
    edgesOf ri = case riTarget ri of
      TargetExternal name ->
        [mk ri 1.0 Nothing (makeNodeId "external" name) name ("external:" <> name)]
      TargetFiles fs ->
        let n = length fs
            w = 1 / fromIntegral n
        in [ mk ri w (if n > 1 then Just n else Nothing)
                (makeNodeId f (T.pack (takeBaseName f))) (T.pack (takeBaseName f)) (T.pack f)
           | f <- fs ]
    mk ri w mN tid label sf =
      ( Edge
          { edgeId = EdgeId (src <> "->" <> tid <> ":imports")
          , edgeSource = src
          , edgeTarget = tid
          , edgeRelation = Imports
          , edgeConfidence = Confidence 1.0
          , edgeWeight = w
          , edgeExtra = Just (extraOf ri mN)
          }
      , Node
          { nodeId = tid
          , nodeLabel = fromText label
          , nodeFileType = CodeFile
          , nodeSourceFile = fromText sf
          , nodeSource = Nothing
          , nodeLineStart = Nothing
          , nodeLineEnd = Nothing
          , nodeSignature = Nothing
          , nodeCommunityId = Nothing
          , nodeKind = Just (fromText "External")
          , nodeDegree = Nothing
          , nodeIsBridge = Nothing
          , nodeExtra = Nothing
          , nodePresentBits = bitNodeKind
          }
      )
    extraOf :: ResolvedImport -> Maybe Int -> Value
    extraOf ri mN = object $
      [ "lang" .= langName (siteLang (riSite ri))
      , "via" .= siteVia (riSite ri)
      , "specifier" .= riUnit ri
      ] ++ [ "alias" .= a | Just a <- [siteAlias (riSite ri)] ]
        ++ [ "package_files" .= n | Just n <- [mN] ]
    langName LangGo = "go" :: Text
    langName LangRust = "rust"
    langName LangRuby = "ruby"

-- | Append the import edges of a Go/Rust/Ruby file to its extraction.
-- Other grammars, and files without a module node, are returned unchanged.
withImportEdges :: String -> FilePath -> [TSNodeInfo] -> Extraction -> IO Extraction
withImportEdges grammar file roots ex = case (importLangForGrammar grammar, fileModuleNode file roots) of
  (Just lang, Just modNode) -> do
    let sites = collectImportSites lang roots
    resolved <- resolveImportSites file sites
    let (es, ns0) = importEdgesFor (nodeId modNode) resolved
        -- the module node is already there unless the root failed to parse
        ns = [modNode | not (null es)] ++ ns0
        -- placeholders never replace a real node that happens to share an id
        nodes' = foldl (\m n -> Map.insertWith (\_ old -> old) (nodeId n) n m) (extractionNodes ex) ns
        edges' = foldl (\m e -> Map.insert (edgeId e) e m) (extractionEdges ex) es
    pure ex { extractionNodes = nodes', extractionEdges = edges' }
  _ -> pure ex
