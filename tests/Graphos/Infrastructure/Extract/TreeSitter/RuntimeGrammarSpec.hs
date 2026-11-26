-- | Runtime tree-sitter grammar loader checks (custom-extractors 4.5).
--
-- The ABI guard and entry-symbol derivation are pure, so they are covered
-- directly. The optional-query fixture uses a real compiled-in grammar (the
-- Python binding) to compile a definitions query, run it on source, and prove
-- that a missing @calls.scm@ disables only the calls kind.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.RuntimeGrammarSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
  ( ExtractorConfig(..)
  , ExtractorMode(..)
  , GraphosConfig(..)
  , PipelineConfig(..)
  , defaultConfig
  , defaultGraphosConfig
  )
import Graphos.Infrastructure.Extract.TreeSitter.RuntimeGrammar
import Graphos.Infrastructure.Wiring (getGrammarPtr)
import Graphos.UseCase.Extract.TreeSitter
  ( GrammarSelection(..)
  , grammarForFile
  , grammarSelectionForFile
  , hasRuntimeGrammar
  )

spec :: Spec
spec = describe "runtime tree-sitter grammar loader (avi-1766 4.1-4.5)" $ do
  describe "ABI guard (4.1)" $ do
    it "accepts every ABI the bundled runtime supports (9-11)" $
      mapM_ (\abi -> abiSupported abi `shouldBe` True) [9, 10, 11]

    it "rejects an ABI below the supported range" $
      abiSupported 8 `shouldBe` False

    it "rejects the ABI 13 case from the spec" $
      abiSupported 13 `shouldBe` False

    it "names the path and ABI when refusing an incompatible grammar" $
      case checkGrammarAbi "/tmp/tree-sitter-future.so" 13 of
        Left msg -> do
          msg `shouldSatisfy` T.isInfixOf "/tmp/tree-sitter-future.so"
          msg `shouldSatisfy` T.isInfixOf "13"
          msg `shouldSatisfy` T.isInfixOf "9-11"
        Right () -> expectationFailure "expected ABI 13 to be rejected"

    it "accepts a supported ABI" $
      checkGrammarAbi "/tmp/tree-sitter-ok.so" 11 `shouldBe` Right ()

  describe "entry symbol (4.2)" $ do
    it "derives the symbol from libtree-sitter-<lang>.so" $
      entrySymbolForPath "/usr/lib/libtree-sitter-php.so" `shouldBe` "tree_sitter_php"

    it "derives the symbol from tree-sitter-<lang>.so" $
      entrySymbolForPath "tree-sitter-c-sharp.so" `shouldBe` "tree_sitter_c_sharp"

    it "derives the symbol from a bare <lang>.so" $
      entrySymbolForPath "~/.local/lib/kotlin.so" `shouldBe` "tree_sitter_kotlin"

    it "handles versioned shared-object names" $
      entrySymbolForPath "libtree-sitter-perl.so.1" `shouldBe` "tree_sitter_perl"

  describe "grammar loading failure (4.2)" $ do
    it "returns a warning instead of throwing when the library is missing" $
      withSystemTempDirectory "graphos-runtime-grammar" $ \dir -> do
        result <- loadRuntimeGrammar (dir </> "missing.so")
        result `shouldSatisfy` isLeft
        case result of
          Left err -> do
            T.pack (rgePath err) `shouldSatisfy` T.isInfixOf "missing.so"
            rgeMessage err `shouldSatisfy` T.isInfixOf "not found"
          Right _ -> expectationFailure "expected a missing library to fail"

  describe "optional runtime queries (4.4)" $ do
    it "loads definitions/imports and warns only about the missing calls kind" $
      withSystemTempDirectory "graphos-queries" $ \dir -> do
        writeFile (dir </> "definitions.scm")
          "(function_definition name: (identifier) @definition.function)\n"
        writeFile (dir </> "imports.scm")
          "(import_statement) @import\n"
        (sources, warnings) <- discoverRuntimeQueries dir
        map rqsKind sources `shouldBe` [QueryDefinitions, QueryImports]
        warnings `shouldSatisfy` any (T.isInfixOf "calls.scm")
        warnings `shouldSatisfy` not . any (T.isInfixOf "definitions.scm")
        warnings `shouldSatisfy` not . any (T.isInfixOf "imports.scm")

    it "compiles and applies a definitions query against real source (fixture)" $
      case getGrammarPtr "python" of
        Nothing -> pendingWith "no compiled-in python grammar binding"
        Just language -> withSystemTempDirectory "graphos-queries" $ \dir -> do
          writeFile (dir </> "definitions.scm")
            "(function_definition name: (identifier) @definition.function)\n"
          (queries, warnings) <- loadRuntimeQueries language dir
          warnings `shouldSatisfy` any (T.isInfixOf "calls.scm")
          case rqDefinitions queries of
            Nothing -> expectationFailure "definitions query was not compiled"
            Just query -> do
              captures <- runRuntimeQuery language
                (TE.encodeUtf8 "def total():\n    pass\n") query
              case captures of
                [capture] -> qcStartRow capture `shouldBe` 0
                _ -> expectationFailure $ "expected exactly one capture, got "
                       ++ show (length captures)
          deleteRuntimeQueries queries

    it "disables an invalid query instead of failing the load" $
      case getGrammarPtr "python" of
        Nothing -> pendingWith "no compiled-in python grammar binding"
        Just language -> withSystemTempDirectory "graphos-queries" $ \dir -> do
          writeFile (dir </> "definitions.scm") "(this-is-not-a-query\n"
          (queries, warnings) <- loadRuntimeQueries language dir
          rqDefinitions queries `shouldSatisfy` isNothing
          warnings `shouldSatisfy` any (T.isInfixOf "invalid definitions query")
          deleteRuntimeQueries queries

  describe "grammar selection plumbing (4.3)" $ do
    let treeSitterEntry grammarPath queries = ExtractorConfig
          { ecMode           = ExtractTreeSitter
          , ecGrammar        = Just "php"
          , ecLanguageId     = Just "php"
          , ecGranularity    = Nothing
          , ecCommand        = Nothing
          , ecBatch          = Nothing
          , ecTimeoutSeconds = Nothing
          , ecGrammarPath    = grammarPath
          , ecQueries        = queries
          }
        withEntry ext ec = defaultConfig
          { cfgGraphosConfig = defaultGraphosConfig
              { gcExtractors = Map.insert ext ec
                  (gcExtractors defaultGraphosConfig) } }

    it "carries grammar_path and queries from the per-extension config" $ do
      let cfg = withEntry ".php"
            (treeSitterEntry (Just "/lib/tree-sitter-php.so")
                             (Just "/conf/queries/php"))
          selection = grammarSelectionForFile cfg "src/App.php"
      gsGrammarName selection `shouldBe` "php"
      gsGrammarPath selection `shouldBe` Just "/lib/tree-sitter-php.so"
      gsQueriesPath selection `shouldBe` Just "/conf/queries/php"
      grammarForFile cfg "src/App.php" `shouldBe` "php"
      hasRuntimeGrammar cfg "src/App.php" `shouldBe` True

    it "keeps a compiled-in grammar when no grammar_path is configured" $ do
      let cfg = withEntry ".php" (treeSitterEntry Nothing Nothing)
      gsGrammarPath (grammarSelectionForFile cfg "App.php") `shouldBe` Nothing
      gsQueriesPath (grammarSelectionForFile cfg "App.php") `shouldBe` Nothing
      hasRuntimeGrammar cfg "App.php" `shouldBe` False

    it "falls back to the extension name for unconfigured files" $ do
      let cfg = withEntry ".php" (treeSitterEntry Nothing Nothing)
          selection = grammarSelectionForFile cfg "script.zzz"
      gsGrammarName selection `shouldBe` "zzz"
      gsGrammarPath selection `shouldBe` Nothing
      grammarForFile cfg "script.zzz" `shouldBe` "zzz"
      hasRuntimeGrammar cfg "script.zzz" `shouldBe` False

    it "ignores a runtime grammar configured for a different extension" $ do
      let cfg = withEntry ".php" (treeSitterEntry (Just "/lib/php.so") Nothing)
      hasRuntimeGrammar cfg "src/App.py" `shouldBe` False
