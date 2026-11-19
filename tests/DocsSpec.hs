-- | Documentation drift guard.
--
-- Runs in the normal @cabal test@ suite and fails when the documentation
-- drifts from the tree or from compiled truth:
--
--   * relative links and intra-document anchors resolve (GitHub slug rules)
--   * the @docs\/@ index covers every audience-facing page
--   * the workflow index matches the @NN-*.md@ files on disk 1:1
--   * the MCP tool reference matches 'allTools'
--   * the CLI reference covers every parser subcommand
--   * the README has one LSP security section and no duplicate headings
--   * CONTRIBUTING defers to GITFLOW.md and the release policy
module DocsSpec (spec) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isAlphaNum, isSpace)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath
  ( takeDirectory
  , takeExtension
  , (</>)
  )
import Test.Hspec

import Graphos.CLI.Parser (subcommandNames)
import Graphos.Infrastructure.Server.MCP (allTools)

-- ── Markdown helpers ─────────────────────────────────────────────────────────

readText :: FilePath -> IO Text
readText = TIO.readFile

-- | Extract inline-link targets from one line: @[label](target)@.
lineLinks :: Text -> [Text]
lineLinks = go
  where
    go t =
      let (_, rest) = T.breakOn "](" t
      in if T.null rest
           then []
           else
             let restUrl = T.drop 2 rest
                 (url, rest') = T.breakOn ")" restUrl
             in url : go rest'

-- | @(line number, target)@ pairs for a whole document.
linksWithLines :: Text -> [(Int, Text)]
linksWithLines body =
  [ (n, url)
  | (n, line) <- zip [1 ..] (T.lines body)
  , url <- lineLinks line
  ]

-- | GitHub heading anchor: lowercase, spaces to hyphens, keep alphanumerics,
-- underscores and hyphens, drop everything else.
slug :: Text -> Text
slug = T.concatMap step . T.toLower
  where
    step c
      | isAlphaNum c || c == '_' || c == '-' = T.singleton c
      | isSpace c = "-"
      | otherwise = ""

headings :: Text -> [Text]
headings body =
  [ T.strip (T.dropWhileEnd (== '#') (T.strip (T.dropWhile (== '#') line)))
  | line <- T.lines body
  , let stripped = T.stripStart line
  , T.isPrefixOf "#" stripped
  , T.isPrefixOf " " (T.dropWhile (== '#') stripped)
  ]

anchors :: Text -> [Text]
anchors = map slug . headings

isExternal :: Text -> Bool
isExternal url =
  any (`T.isPrefixOf` url)
    [ "http://", "https://", "mailto:", "/", "tel:" ]

-- | @(path part, optional anchor)@ of a link target.
splitAnchor :: Text -> (Text, Maybe Text)
splitAnchor url =
  case T.breakOn "#" url of
    (p, rest)
      | T.null rest -> (p, Nothing)
      | otherwise -> (p, Just (T.drop 1 rest))

-- ── File walk ────────────────────────────────────────────────────────────────

walkMarkdown :: FilePath -> IO [FilePath]
walkMarkdown dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then walkMarkdown path
      else pure [path | takeExtension path == ".md"]

documentationFiles :: IO [FilePath]
documentationFiles = do
  docs <- walkMarkdown "docs"
  pure (["README.md", "CONTRIBUTING.md"] ++ sort docs)

-- ── Checks ───────────────────────────────────────────────────────────────────

brokenLinks :: FilePath -> IO [(Int, Text, String)]
brokenLinks src = do
  body <- readText src
  fmap concat $ forM (linksWithLines body) $ \(n, url) -> do
    if isExternal url || T.null url
      then pure []
      else do
        let (pathPart, mAnchor) = splitAnchor url
            target
              | T.null pathPart = src
              | otherwise = takeDirectory src </> T.unpack pathPart
        fileExists <- doesFileExist target
        dirExists <- doesDirectoryExist target
        if not fileExists && not dirExists
          then pure [(n, url, target)]
          else case mAnchor of
            Nothing -> pure []
            Just _ | takeExtension target /= ".md" -> pure []
            Just a -> do
              targetAnchors <- anchors <$> readText target
              if slug a `elem` targetAnchors
                then pure []
                else pure [(n, url, target ++ " anchor #" ++ T.unpack a)]

duplicateHeadings :: Text -> [Text]
duplicateHeadings body =
  [ h
  | (h, count) <- Map.toList (Map.fromListWith (+) [(h, 1 :: Int) | h <- headings body])
  , count > 1
  ]

-- | Audience-facing pages: every doc except the index itself, internal notes,
-- and the two formal-foundations directories.
isAudiencePage :: FilePath -> Bool
isAudiencePage path =
  path /= "docs/README.md"
    && not (any (`T.isPrefixOf` T.pack path) ["docs/internal/", "docs/math/", "docs/math-requirements/"])

indexTargets :: Text -> [FilePath]
indexTargets body =
  sort
    [ T.unpack (T.pack "docs") </> T.unpack (T.dropWhile (== '/') p)
    | (_, url) <- linksWithLines body
    , not (isExternal url)
    , let (p, _) = splitAnchor url
    , not (T.null p)
    ]

workflowFiles :: IO [FilePath]
workflowFiles = do
  entries <- listDirectory "docs/workflows"
  pure (sort [e | e <- entries, takeExtension e == ".md", e /= "README.md"])

indexRows :: Text -> [(String, FilePath)]
indexRows body =
  [ (T.unpack number, "docs/workflows" </> T.unpack target)
  | line <- T.lines body
  , T.isPrefixOf "|" (T.stripStart line)
  , let cells = map T.strip (T.splitOn "|" line)
  , (number : linkCell : _) <- [drop 1 cells]
  , T.length number == 2
  , T.all (`elem` ['0' .. '9']) number
  , Just target <- [firstLink linkCell]
  ]
  where
    firstLink cell = case lineLinks cell of
      (u : _) -> Just u
      [] -> Nothing

third :: (a, b, c) -> a
third (a, _, _) = a

mcpDocTools :: Text -> [Text]
mcpDocTools body =
  [ name
  | line <- T.lines body
  , Just name <- [toolHeading (T.strip line)]
  ]
  where
    toolHeading line = do
      rest <- T.stripPrefix "### " line
      let (num, afterNum) = T.breakOn "." rest
      if T.all (`elem` ['0' .. '9']) num && not (T.null num)
        then do
          let r = T.strip (T.drop 1 afterNum)
          inner <- T.stripPrefix "`" r
          let (name, _) = T.breakOn "`" inner
          pure name
        else Nothing

mentioned :: Text -> Text -> Bool
mentioned name body =
  T.isInfixOf ("`" <> name <> "`") body || T.isInfixOf ("graphos " <> name) body

spec :: Spec
spec = describe "documentation drift guard" $ do
  describe "documentation link integrity" $ do
    files <- runIO documentationFiles
    forM_ files $ \src -> it (src ++ " relative links and anchors resolve") $ do
      bad <- brokenLinks src
      unless (null bad) $
        expectationFailure $
          "broken documentation links:\n"
            ++ unlines
              [ src ++ ":" ++ show n ++ " -> " ++ T.unpack url ++ " (resolved: " ++ target ++ ")"
              | (n, url, target) <- bad
              ]

  describe "README front door" $ do
    body <- runIO (readText "README.md")
    it "has exactly one LSP security notes heading" $ do
      let matches = [h | h <- headings body, T.toLower h == "lsp security notes"]
      length matches `shouldBe` 1
    it "has no duplicate headings" $ do
      duplicateHeadings body `shouldBe` []

  describe "documentation index" $ do
    index <- runIO (readText "docs/README.md")
    files <- runIO (walkMarkdown "docs")
    let targets = indexTargets index
        audience = filter isAudiencePage files
        missing = [f | f <- audience, f `notElem` targets]
        internal = [f | f <- files, "docs/internal/" `T.isPrefixOf` T.pack f, f `elem` targets]
    it "covers every audience-facing page" $ do
      missing `shouldBe` []
    it "does not link internal notes" $ do
      internal `shouldBe` []

  describe "workflow index parity" $ do
    index <- runIO (readText "docs/workflows/README.md")
    onDisk <- runIO workflowFiles
    let rows = indexRows index
        rowFiles = sort (map snd rows)
        expected = sort ["docs/workflows" </> f | f <- onDisk]
        rowNumbers = map fst rows
        fileNumbers = [take 2 f | f <- onDisk]
    it "has exactly one row per workflow file" $ do
      rowFiles `shouldBe` expected
    it "keeps row numbering equal to file numbering" $ do
      rowNumbers `shouldBe` fileNumbers
    it "does not repeat a row" $ do
      length rowFiles `shouldBe` length (nub rowFiles)

  describe "MCP tool reference parity" $ do
    body <- runIO (readText "docs/reference/mcp-tools.md")
    it "documents exactly the tools exported by allTools" $ do
      sort (mcpDocTools body) `shouldBe` sort (map third allTools)

  describe "CLI reference coverage" $ do
    cli <- runIO (readText "docs/reference/cli.md")
    readme <- runIO (readText "README.md")
    forM_ subcommandNames $ \name -> it ("documents subcommand " ++ T.unpack name) $ do
      when (not (mentioned name cli)) $
        expectationFailure ("docs/reference/cli.md does not document `" ++ T.unpack name ++ "`")
      when (not (mentioned name cli || T.isInfixOf name readme)) $
        expectationFailure ("README plus CLI reference omit `" ++ T.unpack name ++ "`")

  describe "governance documentation" $ do
    contributing <- runIO (readText "CONTRIBUTING.md")
    it "links the branch model owner GITFLOW.md" $ do
      T.isInfixOf "GITFLOW.md" contributing `shouldBe` True
    it "links the release policy instead of inlining it" $ do
      T.isInfixOf "docs/reference/release-policy.md" contributing `shouldBe` True
    it "contains no GitFlow integration-branch prescription" $ do
      T.isInfixOf "`develop`" contributing `shouldBe` False
      T.isInfixOf "develop branch" contributing `shouldBe` False
    it "keeps the release policy document present" $ do
      exists <- doesFileExist "docs/reference/release-policy.md"
      exists `shouldBe` True
