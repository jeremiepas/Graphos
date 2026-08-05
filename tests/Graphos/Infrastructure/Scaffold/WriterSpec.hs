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
              [ ScaffoldFile (tmpDir </> ".opencode/skills/graphos/SKILL.md") "skill content"
              , ScaffoldFile (tmpDir </> ".opencode/agent/graphos-navigator.md") "nav content"
              ]
        result <- writeScaffold files
        length (srCreated result) `shouldBe` 2
        length (srSkipped result) `shouldBe` 0
        skillExists <- doesFileExist (tmpDir </> ".opencode/skills/graphos/SKILL.md")
        navExists <- doesFileExist (tmpDir </> ".opencode/agent/graphos-navigator.md")
        skillExists `shouldBe` True
        navExists `shouldBe` True

    it "skips existing files and writes remaining" $ do
      withSystemTempDirectory "graphos-scaffold-test" $ \tmpDir -> do
        let skillPath = tmpDir </> ".opencode/skills/graphos/SKILL.md"
            navPath = tmpDir </> ".opencode/agent/graphos-navigator.md"
        createDirectoryIfMissing True (tmpDir </> ".opencode/skills/graphos")
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
    it "detects .opencode directory" $ do
      withSystemTempDirectory "graphos-detect-test" $ \tmpDir -> do
        createDirectoryIfMissing True (tmpDir </> ".opencode")
        oldDir <- getCurrentDirectory
        setCurrentDirectory tmpDir
        facts <- gatherDetectionFacts
        setCurrentDirectory oldDir
        dfOpencodeDirExists facts `shouldBe` True