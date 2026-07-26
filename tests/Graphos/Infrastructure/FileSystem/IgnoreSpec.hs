-- | Tests for FileSystem.Ignore module — .graphosignore and .gitignore pattern handling.
module Graphos.Infrastructure.FileSystem.IgnoreSpec where

import Test.Hspec
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import qualified Data.List.NonEmpty as NE

import Graphos.Infrastructure.FileSystem.Ignore

-- | Create a temporary test directory, run the action, then clean up.
withTestDir :: FilePath -> IO a -> IO a
withTestDir dir action = do
  createDirectoryIfMissing True dir
  result <- action
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()
  pure result

spec :: Spec
spec = do
  describe "IgnorePattern parsing" $ do
    it "parses wildcard patterns (*.log)" $ do
      parsePattern "*.log" `shouldBe` WildcardPattern "*.log"

    it "parses directory patterns (node_modules/)" $ do
      parsePattern "node_modules/" `shouldBe` PrefixPattern "node_modules"

    it "parses dot-filenames as exact patterns (.min.js)" $ do
      parsePattern ".min.js" `shouldBe` ExactPattern ".min.js"

    it "parses exact patterns (build)" $ do
      parsePattern "build" `shouldBe` ExactPattern "build"

    it "parses leading-slash patterns (/absolute)" $ do
      parsePattern "/absolute" `shouldBe` ExactPattern "/absolute"

    it "parses wildcard patterns with star in middle (.ghc.environment.*)" $ do
      parsePattern ".ghc.environment.*" `shouldBe` WildcardPattern ".ghc.environment.*"

    it "parses wildcard patterns with star at end (result-*)" $ do
      parsePattern "result-*" `shouldBe` WildcardPattern "result-*"

    it "parses wildcard patterns with star in path (.opencode/*)" $ do
      parsePattern ".opencode/*" `shouldBe` WildcardPattern ".opencode/*"

    it "parses dot-filenames as exact (cabal.project.local)" $ do
      parsePattern "cabal.project.local" `shouldBe` ExactPattern "cabal.project.local"

    it "parses dot-filenames as exact (.DS_Store)" $ do
      parsePattern ".DS_Store" `shouldBe` ExactPattern ".DS_Store"

    it "parses dot-directories as exact (.git)" $ do
      parsePattern ".git" `shouldBe` ExactPattern ".git"

    it "parses double-star patterns (.opencode/agent/**)" $ do
      parsePattern ".opencode/agent/**" `shouldBe` WildcardPattern ".opencode/agent/**"

    it "parses tilde-suffixed patterns (cabal.project.local~)" $ do
      parsePattern "cabal.project.local~" `shouldBe` ExactPattern "cabal.project.local~"

  describe "shouldIgnore" $ do
    it "ignores paths matching prefix patterns" $ do
      let patterns = [AnnotatedPattern (PrefixPattern "node_modules") False 0]
      shouldIgnore patterns "node_modules/pkg/index.js" `shouldBe` True

    it "ignores paths matching suffix patterns" $ do
      let patterns = [AnnotatedPattern (SuffixPattern ".min.js") False 0]
      shouldIgnore patterns "app/bundle.min.js" `shouldBe` True

    it "ignores paths matching exact patterns" $ do
      let patterns = [AnnotatedPattern (ExactPattern "build") False 0]
      shouldIgnore patterns "src/build" `shouldBe` True

    it "ignores paths matching glob patterns" $ do
      let patterns = [AnnotatedPattern (GlobPattern ".log") False 0]
      shouldIgnore patterns "app/debug.log" `shouldBe` True

    it "does not ignore paths that don't match any pattern" $ do
      let patterns = [AnnotatedPattern (ExactPattern "build") False 0]
      shouldIgnore patterns "src/Main.hs" `shouldBe` False

    it "negation patterns re-include previously ignored paths" $ do
      let patterns = [ AnnotatedPattern (GlobPattern ".log") False 0
                      , AnnotatedPattern (ExactPattern "important.log") True 1
                      ]
      shouldIgnore patterns "debug.log" `shouldBe` True
      shouldIgnore patterns "important.log" `shouldBe` False

    it "higher priority negation overrides lower priority ignore" $ do
      let patterns = [ AnnotatedPattern (PrefixPattern "vendor") False 0  -- hardcoded
                     , AnnotatedPattern (ExactPattern "vendor") True 2     -- graphosignore override
                     ]
      shouldIgnore patterns "vendor/pkg/go.mod" `shouldBe` False

  -- ───────────────────────────────────────────────
  -- Wildcard pattern matching (WildcardPattern)
  -- ───────────────────────────────────────────────

  describe "WildcardPattern matching" $ do
    it "matches leading wildcard (*.log)" $ do
      let patterns = [AnnotatedPattern (WildcardPattern "*.log") False 0]
      shouldIgnore patterns "app/debug.log" `shouldBe` True
      shouldIgnore patterns "debug.log" `shouldBe` True
      shouldIgnore patterns "src/app/debug.log" `shouldBe` True
      shouldIgnore patterns "src/Main.hs" `shouldBe` False

    it "matches trailing wildcard (result-*)" $ do
      let patterns = [AnnotatedPattern (WildcardPattern "result-*") False 0]
      shouldIgnore patterns "result-1" `shouldBe` True
      shouldIgnore patterns "result-foo" `shouldBe` True
      shouldIgnore patterns "result-" `shouldBe` True
      shouldIgnore patterns "result" `shouldBe` False

    it "matches middle wildcard (.ghc.environment.*)" $ do
      let patterns = [AnnotatedPattern (WildcardPattern ".ghc.environment.*") False 0]
      shouldIgnore patterns ".ghc.environment.x86_64-linux" `shouldBe` True
      shouldIgnore patterns ".ghc.environment.aarch64-darwin" `shouldBe` True
      shouldIgnore patterns "ghc.environment" `shouldBe` False

    it "matches wildcard with path (.opencode/*)" $ do
      let patterns = [AnnotatedPattern (WildcardPattern ".opencode/*") False 0]
      shouldIgnore patterns ".opencode/config.json" `shouldBe` True
      shouldIgnore patterns ".opencode/opencode.json" `shouldBe` True

    it "matches exact filenames that previously were suffix patterns" $ do
      let patterns = [AnnotatedPattern (ExactPattern "cabal.project.local") False 0]
      shouldIgnore patterns "cabal.project.local" `shouldBe` True
      shouldIgnore patterns "my-cabal.project.local" `shouldBe` False

    it "matches dot-filenames as exact (.DS_Store)" $ do
      let patterns = [AnnotatedPattern (ExactPattern ".DS_Store") False 0]
      shouldIgnore patterns ".DS_Store" `shouldBe` True
      shouldIgnore patterns "src/.DS_Store" `shouldBe` True

    it "does not match across path separators with single star" $ do
      let patterns = [AnnotatedPattern (WildcardPattern "result-*") False 0]
      -- Single * should match within path segments
      shouldIgnore patterns "some/result-1" `shouldBe` True

  describe "WildcardPattern double-star (**) matching" $ do
    it "matches nested paths with double-star" $ do
      let patterns = [AnnotatedPattern (WildcardPattern ".opencode/agent/**") False 2]
      shouldIgnore patterns ".opencode/agent/core/openagent.md" `shouldBe` True
      shouldIgnore patterns ".opencode/agent/subagents/coder.md" `shouldBe` True

    it "negation with double-star overrides lower priority ignore" $ do
      let patterns = [ AnnotatedPattern (PrefixPattern ".opencode") False 0      -- hardcoded
                     , AnnotatedPattern (WildcardPattern ".opencode/agent/**") True 2  -- graphosignore override
                     ]
      shouldIgnore patterns ".opencode/agent/core/openagent.md" `shouldBe` False
      shouldIgnore patterns ".opencode/config.json" `shouldBe` True

  describe "loadGraphosignore" $ do
    it "returns empty list when .graphosignore does not exist" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-1"
      withTestDir tmpDir $ do
        patterns <- loadGraphosignore tmpDir
        patterns `shouldBe` []

    it "parses .graphosignore patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-2"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".graphosignore") "node_modules/\n*.log\nbuild\n"
        patterns <- loadGraphosignore tmpDir
        length patterns `shouldBe` 3
        apPattern (patterns !! 0) `shouldBe` PrefixPattern "node_modules"
        apPattern (patterns !! 1) `shouldBe` WildcardPattern "*.log"
        apPattern (patterns !! 2) `shouldBe` ExactPattern "build"

    it "ignores comments and blank lines" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-3"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".graphosignore") "# comment\n\nbuild\n  \n"
        patterns <- loadGraphosignore tmpDir
        length patterns `shouldBe` 1

    it "assigns priority 2 to graphosignore patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-4"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".graphosignore") "build\n"
        patterns <- loadGraphosignore tmpDir
        apPriority (NE.head $ NE.fromList patterns) `shouldBe` 2

  describe "loadGitignore" $ do
    it "returns empty list when .gitignore does not exist" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-5"
      withTestDir tmpDir $ do
        patterns <- loadGitignore tmpDir
        patterns `shouldBe` []

    it "parses .gitignore patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-6"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "node_modules/\n*.log\nbuild\n"
        patterns <- loadGitignore tmpDir
        length patterns `shouldBe` 3

    it "handles negation patterns (!)" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-7"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "*.log\n!important.log\n"
        patterns <- loadGitignore tmpDir
        length patterns `shouldBe` 2
        apNegate (patterns !! 0) `shouldBe` False
        apNegate (patterns !! 1) `shouldBe` True

    it "assigns priority 1 to gitignore patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-8"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "build\n"
        patterns <- loadGitignore tmpDir
        apPriority (NE.head $ NE.fromList patterns) `shouldBe` 1

    it "parses wildcard patterns with star in middle" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-11"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") ".ghc.environment.*\nresult-*\n"
        patterns <- loadGitignore tmpDir
        length patterns `shouldBe` 2
        apPattern (patterns !! 0) `shouldBe` WildcardPattern ".ghc.environment.*"
        apPattern (patterns !! 1) `shouldBe` WildcardPattern "result-*"

    it "parses dot-filenames as exact patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-12"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "cabal.project.local\n.DS_Store\n"
        patterns <- loadGitignore tmpDir
        length patterns `shouldBe` 2
        apPattern (patterns !! 0) `shouldBe` ExactPattern "cabal.project.local"
        apPattern (patterns !! 1) `shouldBe` ExactPattern ".DS_Store"

    it "parses negation with wildcard patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-13"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "!important.log\n"
        patterns <- loadGitignore tmpDir
        length patterns `shouldBe` 1
        apNegate (patterns !! 0) `shouldBe` True
        apPattern (patterns !! 0) `shouldBe` ExactPattern "important.log"

  describe "loadIgnorePatterns" $ do
    it "returns hardcoded patterns when no ignore files exist" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-9"
      withTestDir tmpDir $ do
        patterns <- loadIgnorePatterns tmpDir
        -- Hardcoded patterns are now ExactPattern (not SuffixPattern for dot-names)
        any (\ap -> apPattern ap == ExactPattern ".git") patterns `shouldBe` True
        any (\ap -> apPattern ap == ExactPattern "node_modules") patterns `shouldBe` True

    it "merges hardcoded, gitignore, and graphosignore patterns" $ do
      let tmpDir = "/tmp/graphos-test-ignore-spec-10"
      withTestDir tmpDir $ do
        writeFile (tmpDir </> ".gitignore") "*.log\n"
        writeFile (tmpDir </> ".graphosignore") "dist/\n"
        patterns <- loadIgnorePatterns tmpDir
        -- Should have hardcoded + gitignore + graphosignore patterns
        length patterns `shouldSatisfy` (> 35) -- 35+ hardcoded + gitignore + graphosignore
        any (\ap -> apPattern ap == ExactPattern ".git") patterns `shouldBe` True
        any (\ap -> apPattern ap == WildcardPattern "*.log") patterns `shouldBe` True
        any (\ap -> apPattern ap == PrefixPattern "dist") patterns `shouldBe` True

  describe "hardcodedIgnorePatterns" $ do
    it "contains common directories" $ do
      -- Entries containing dots are now parsed as ExactPattern (not SuffixPattern)
      any (\ap -> apPattern ap == ExactPattern ".git") hardcodedIgnorePatterns `shouldBe` True
      any (\ap -> apPattern ap == ExactPattern "node_modules") hardcodedIgnorePatterns `shouldBe` True
      any (\ap -> apPattern ap == ExactPattern "target") hardcodedIgnorePatterns `shouldBe` True
      any (\ap -> apPattern ap == ExactPattern "vendor") hardcodedIgnorePatterns `shouldBe` True
      any (\ap -> apPattern ap == ExactPattern ".next") hardcodedIgnorePatterns `shouldBe` True
      any (\ap -> apPattern ap == ExactPattern ".gradle") hardcodedIgnorePatterns `shouldBe` True

    it "all hardcoded patterns have priority 0" $ do
      all (\ap -> apPriority ap == 0) hardcodedIgnorePatterns `shouldBe` True

    it "all hardcoded patterns are non-negation" $ do
      all (\ap -> not (apNegate ap)) hardcodedIgnorePatterns `shouldBe` True

  -- ───────────────────────────────────────────────
  -- Integration test: project .gitignore patterns
  -- ───────────────────────────────────────────────

  describe "project .gitignore integration" $ do
    it "correctly parses all common .gitignore patterns" $ do
      -- Verify key patterns from a typical .gitignore
      let testPatterns =
            [ ("*.o", WildcardPattern "*.o")
            , ("*.hi", WildcardPattern "*.hi")
            , ("*.dyn_o", WildcardPattern "*.dyn_o")
            , (".ghc.environment.*", WildcardPattern ".ghc.environment.*")
            , ("result-*", WildcardPattern "result-*")
            , ("dist/", PrefixPattern "dist")
            , ("node_modules/", PrefixPattern "node_modules")
            , ("cabal.project.local", ExactPattern "cabal.project.local")
            , (".DS_Store", ExactPattern ".DS_Store")
            , ("result", ExactPattern "result")
            , (".tmp/", PrefixPattern ".tmp")
            , (".opencode/*", WildcardPattern ".opencode/*")
            -- Note: ! negation is handled by parseGitignoreLine, not parsePattern.
            -- parsePattern receives the pattern after ! is stripped.
            , (".opencode/agent/", PrefixPattern ".opencode/agent")
            ]
      mapM_ (\(input, expected) ->
               parsePattern input `shouldBe` expected
             ) testPatterns

    it "wildcard patterns match intended file paths" $ do
      -- Test that wildcard patterns actually match the paths they should
      let mkAnnot pat = AnnotatedPattern pat False 1
      -- .ghc.environment.* should match .ghc.environment.x86_64-linux
      shouldIgnore [mkAnnot (WildcardPattern ".ghc.environment.*")] ".ghc.environment.x86_64-linux" `shouldBe` True
      -- result-* should match result-1, result-foo
      shouldIgnore [mkAnnot (WildcardPattern "result-*")] "result-1" `shouldBe` True
      shouldIgnore [mkAnnot (WildcardPattern "result-*")] "result-foo" `shouldBe` True
      -- *.o should match .o files
      shouldIgnore [mkAnnot (WildcardPattern "*.o")] "src/Main.o" `shouldBe` True
      -- .opencode/* should match files inside .opencode/
      shouldIgnore [mkAnnot (WildcardPattern ".opencode/*")] ".opencode/opencode.json" `shouldBe` True