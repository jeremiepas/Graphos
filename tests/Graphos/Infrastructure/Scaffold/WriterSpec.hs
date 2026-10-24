module Graphos.Infrastructure.Scaffold.WriterSpec where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (doesFileExist, getCurrentDirectory, setCurrentDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO

import Graphos.Domain.Scaffold
import Graphos.UseCase.Scaffold
import Graphos.Infrastructure.Scaffold.Writer

spec :: Spec
spec = do
  describe "writeScaffold" $ do
    it "writes all files in a fresh directory" $ do
      withSystemTempDirectory "graphos-scaffold-test" $ \tmpDir -> do
        let files =
              [ ScaffoldFile (tmpDir </> ".claude/skills/graphos/SKILL.md") "skill content"
              , ScaffoldFile (tmpDir </> ".claude/agents/graphos-navigator.md") "nav content"
              ]
        result <- writeScaffold files
        length (srCreated result) `shouldBe` 2
        length (srSkipped result) `shouldBe` 0
        skillExists <- doesFileExist (tmpDir </> ".claude/skills/graphos/SKILL.md")
        navExists <- doesFileExist (tmpDir </> ".claude/agents/graphos-navigator.md")
        skillExists `shouldBe` True
        navExists `shouldBe` True

    it "skips existing files and writes remaining" $ do
      withSystemTempDirectory "graphos-scaffold-test" $ \tmpDir -> do
        let skillPath = tmpDir </> ".claude/skills/graphos/SKILL.md"
            navPath = tmpDir </> ".claude/agents/graphos-navigator.md"
        createDirectoryIfMissing True (tmpDir </> ".claude/skills/graphos")
        TIO.writeFile skillPath "existing"
        let files =
              [ ScaffoldFile skillPath "new skill"
              , ScaffoldFile navPath "nav content"
              ]
        result <- writeScaffold files
        length (srCreated result) `shouldBe` 1
        length (srSkipped result) `shouldBe` 1
        navExists <- doesFileExist navPath
        navExists `shouldBe` True

    it "exits successfully even when files are skipped" $ do
      withSystemTempDirectory "graphos-scaffold-test" $ \tmpDir -> do
        let path = tmpDir </> "existing.md"
        TIO.writeFile path "old"
        let files = [ScaffoldFile path "new"]
        result <- writeScaffold files
        length (srSkipped result) `shouldBe` 1
        length (srCreated result) `shouldBe` 0

  describe "gatherDetectionFacts" $ do
    it "detects .claude directory" $ do
      withSystemTempDirectory "graphos-detect-test" $ \tmpDir -> do
        createDirectoryIfMissing True (tmpDir </> ".claude")
        oldDir <- getCurrentDirectory
        setCurrentDirectory tmpDir
        facts <- gatherDetectionFacts
        setCurrentDirectory oldDir
        dfClaudeDirExists facts `shouldBe` True

    it "reports absence when no dirs detected" $ do
      withSystemTempDirectory "graphos-detect-test" $ \tmpDir -> do
        oldDir <- getCurrentDirectory
        setCurrentDirectory tmpDir
        facts <- gatherDetectionFacts
        setCurrentDirectory oldDir
        dfClaudeDirExists facts `shouldBe` False
