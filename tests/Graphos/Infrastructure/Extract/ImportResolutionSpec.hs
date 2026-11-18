{-# LANGUAGE OverloadedStrings #-}
-- | Corpus-aware import resolution (bunch-real-projects D4): one case per
-- scenario of the @import-resolution@ delta, run on real tree-sitter
-- extractions of small on-disk projects, plus the pure config reader.
module Graphos.Infrastructure.Extract.ImportResolutionSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)

import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types (Node(..), Edge(..), Relation(..), Extraction(..))
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar, mkSourceFile, defaultTruncationBudget)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import Graphos.Infrastructure.Extract.ImportResolution
import Graphos.Infrastructure.Wiring (getGrammarPtr)

writeTree :: FilePath -> [(FilePath, Text)] -> IO ()
writeTree root = mapM_ $ \(rel, body) -> do
  let p = root </> rel
  createDirectoryIfMissing True (takeDirectory p)
  BS.writeFile p (TE.encodeUtf8 body)

-- | Extract every TS/JS file of the tree and merge, as the pipeline does.
extractTree :: FilePath -> [FilePath] -> IO Extraction
extractTree root rels = do
  exs <- mapM one rels
  pure Extraction
    { extractionNodes = Map.unions (map extractionNodes exs)
    , extractionEdges = Map.unions (map extractionEdges exs) }
  where
    one rel = do
      let path = root </> rel
      src <- BS.readFile path
      lang <- maybe (fail "no typescript grammar") pure (getGrammarPtr "typescript")
      parsed <- parseWithGrammar lang src
      ts <- maybe (fail ("parse failed: " ++ path)) pure parsed
      pure (tsNodesToExtraction (mkSourceFile path "") GranularityFunction defaultTruncationBudget ts)

-- | Build the tree, extract it, run the pass (configs loaded from disk).
resolveTree :: [(FilePath, Text)] -> (FilePath -> Extraction -> ResolutionStats -> IO a) -> IO a
resolveTree files k = withSystemTempDirectory "graphos-d4" $ \root -> do
  writeTree root files
  ex <- extractTree root [ f | (f, _) <- files, not (".json" `T.isSuffixOf` T.pack f) ]
  cfgs <- loadPathsConfigs ex
  let (ex', stats) = resolveCorpusImports cfgs ex
  k root ex' stats

-- | Target source file (relative to @root@ when inside it) of every
-- @imports@ edge leaving @from@.
importsFrom :: FilePath -> FilePath -> Extraction -> [Text]
importsFrom root from ex =
  sort [ rel (toText (nodeSourceFile n))
       | e <- Map.elems (extractionEdges ex), edgeRelation e == Imports
       , Just s <- [Map.lookup (edgeSource e) (extractionNodes ex)]
       , toText (nodeSourceFile s) == T.pack (root </> from)
       , Just n <- [Map.lookup (edgeTarget e) (extractionNodes ex)] ]
  where
    rel t = maybe t id (T.stripPrefix (T.pack root <> "/") t)

spec :: Spec
spec = describe "Corpus-aware import resolution (bunch-real-projects D4)" $ do

  it "extension-less relative import targets the module node of the file" $
    resolveTree [ ("src/app/a.ts", "import { b } from './b';\nexport const a = b;\n")
                , ("src/app/b.ts", "export const b = 1;\n") ] $ \root ex stats -> do
      importsFrom root "src/app/a.ts" ex `shouldBe` ["src/app/b.ts"]
      rsExtension stats `shouldBe` 1
      -- the placeholder `src/app/b` module node is gone
      [ () | n <- Map.elems (extractionNodes ex), nodeLineStart n == Nothing
           , fmap toText (nodeKind n) == Just "Module" ] `shouldBe` []

  it "directory import resolves to its index file" $
    resolveTree [ ("src/a.ts", "import { x } from './lib';\nexport const a = x;\n")
                , ("src/lib/index.ts", "export const x = 1;\n") ] $ \root ex _ ->
      importsFrom root "src/a.ts" ex `shouldBe` ["src/lib/index.ts"]

  it "a .js specifier of a plain JavaScript file resolves to that file" $
    resolveTree [ ("src/a.js", "import { x } from './x.js';\nexport const a = x;\n")
                , ("src/x.js", "export const x = 1;\n") ] $ \root ex _ ->
      importsFrom root "src/a.js" ex `shouldBe` ["src/x.js"]

  it "Next.js root alias resolves; a scoped package stays external" $
    resolveTree [ ("app/page.tsx", "import { cn } from '@/lib/utils';\nimport { json } from '@remix-run/react';\nexport const p = cn;\n")
                , ("lib/utils.ts", "export const cn = 1;\n") ] $ \root ex stats -> do
      importsFrom root "app/page.tsx" ex `shouldBe` ["external:@remix-run/react", "lib/utils.ts"]
      rsAlias stats `shouldBe` 1

  it "SvelteKit $lib with an extension" $
    resolveTree [ ("src/routes/+page.js", "import { get } from '$lib/api.js';\nexport const load = get;\n")
                , ("src/lib/api.js", "export const get = 1;\n") ] $ \root ex _ ->
      importsFrom root "src/routes/+page.js" ex `shouldBe` ["src/lib/api.js"]

  it "Angular tsconfig paths (JSON with comments and trailing commas)" $
    resolveTree [ ("tsconfig.json", "{\n  // generated\n  \"compilerOptions\": {\n    \"baseUrl\": \"./\",\n    \"paths\": { \"app/*\": [\"src/main/webapp/app/*\"], },\n  },\n}\n")
                , ("src/main/webapp/app/home/home.ts", "import { E } from 'app/config/error.constants';\nexport const h = E;\n")
                , ("src/main/webapp/app/config/error.constants.ts", "export const E = 1;\n") ] $ \root ex stats -> do
      importsFrom root "src/main/webapp/app/home/home.ts" ex
        `shouldBe` ["src/main/webapp/app/config/error.constants.ts"]
      rsPaths stats `shouldBe` 1

  it "an ambiguous suffix stays external" $
    resolveTree [ ("src/c.ts", "import { u } from '@/utils';\nexport const c = u;\n")
                , ("src/a/utils.ts", "export const u = 1;\n")
                , ("src/b/utils.ts", "export const u = 2;\n") ] $ \root ex stats -> do
      importsFrom root "src/c.ts" ex `shouldBe` ["external:@/utils"]
      rsAlias stats `shouldBe` 0

  it "bare packages and node: builtins stay external" $
    resolveTree [ ("src/a.ts", "import fs from 'node:fs';\nimport React from 'react';\nexport const a = 1;\n")
                , ("src/react.ts", "export const r = 1;\n") ] $ \root ex _ ->
      importsFrom root "src/a.ts" ex `shouldBe` ["external:node:fs", "external:react"]

  it "a resolved edge keeps its EdgeId" $
    withSystemTempDirectory "graphos-d4" $ \root -> do
      let files = [ ("src/a.ts", "import { b } from './b';\nexport const a = b;\n")
                  , ("src/b.ts", "export const b = 1;\n") ]
      writeTree root files
      ex <- extractTree root (map fst files)
      let (ex', _) = resolveCorpusImports [] ex
          importIds x = sort [ k | (k, e) <- Map.toList (extractionEdges x), edgeRelation e == Imports ]
      importIds ex' `shouldBe` importIds ex

  it "is the identity on a corpus without placeholders" $
    resolveTree [ ("src/a.ts", "import { b } from './b.ts';\nexport const a = b;\n")
                , ("src/b.ts", "export const b = 1;\n") ] $ \root ex stats -> do
      importsFrom root "src/a.ts" ex `shouldBe` ["src/b.ts"]
      stats `shouldBe` ResolutionStats 0 0 0 0

  describe "parsePathsConfig" $ do
    it "reads baseUrl and paths, ignoring comments and trailing commas" $
      parsePathsConfig "web/tsconfig.json"
        "{ /* c */ \"compilerOptions\": { \"baseUrl\": \"src\", \"paths\": { \"~/*\": [\"./*\",], }, }, }"
        `shouldBe` Just (PathsConfig "web" "src" [("~/*", ["./*"])])
    it "keeps // inside strings" $
      stripJsonComments "{\"a\": \"http://x\"} // tail" `shouldBe` "{\"a\": \"http://x\"} "
    it "has no config without compilerOptions.paths" $
      parsePathsConfig "tsconfig.json" "{ \"compilerOptions\": { \"strict\": true } }" `shouldBe` Nothing
