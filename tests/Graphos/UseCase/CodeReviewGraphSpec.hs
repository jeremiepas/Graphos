{-# LANGUAGE OverloadedStrings #-}
-- | usable-graph-for-code-review, end to end:
--
--   * 4.3 — the real pipeline on @example/ts-lsp-test@, then @path@ through the
--     UseCase: @index.ts@ reaches @db.ts@ by @imports@ hops;
--   * 5.3 — @symbols@ on the new labels;
--   * 6.3 — checker parity: the Haskell mirrors on the Lean toy fixtures of
--     @lean/ReviewGraph.lean@ (§8) give the Lean verdicts.
module Graphos.UseCase.CodeReviewGraphSpec (spec) where

import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText, fromText)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Temp (withSystemTempDirectory)

import Graphos.Domain.Config (Granularity(..), defaultOtelConfig)
import Graphos.Domain.Logging (LogLevel(..))
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, gEdges)
import Graphos.Domain.Graph.Index (buildIndex)
import Graphos.Domain.Types
  ( Node(..), Edge(..), Relation(..), Extraction(..), NodeId, FileType(..), extractionFromLists )
import Graphos.Domain.Types.Pipeline (PipelineConfig(..), defaultConfig)
import Graphos.Domain.Query.Cypher.Parser (parseQuery)
import Graphos.Domain.Query.Cypher.Eval (evaluate, CypherResult(..))
import Graphos.Domain.Graph.Index (GraphIndex(..))
import Graphos.Infrastructure.Logging (defaultLogEnv)
import Graphos.Infrastructure.Observability.SDK (initObservability)
import Graphos.Infrastructure.Wiring (productionAppEnv, getGrammarPtr)
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar, mkSourceFile, defaultTruncationBudget)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import Graphos.Infrastructure.Extract.ImportResolution (resolveCorpusImports)
import Graphos.UseCase.Load (LoadResult(..), loadGraphFromFile)
import Graphos.UseCase.Pipeline (runPipeline)
import Graphos.UseCase.Query
  ( pathQueryWithIndex, resolveNodeArg, NodeResolution(..), symbolLookup, SymbolResult(..), ScoredNode(..) )

-- | Every consecutive pair of the path is joined by an @imports@ edge (either
-- direction: 'shortestPath' walks the undirected projection).
importsHops :: Graph -> [NodeId] -> Bool
importsHops g path =
  let imports = Map.elems (Map.filter ((== Imports) . edgeRelation) (gEdges g))
      hop a b = any (\e -> (edgeSource e, edgeTarget e) `elem` [(a, b), (b, a)]) imports
  in length path >= 2 && and (zipWith hop path (drop 1 path))

fileOf :: Graph -> NodeId -> Maybe Text
fileOf g nid = fmap (toText . nodeSourceFile) (Map.lookup nid (gNodes g))

kindOf :: Graph -> NodeId -> Maybe Text
kindOf g nid = fmap toText . nodeKind =<< Map.lookup nid (gNodes g)

-- | Run the production pipeline on the example corpus into a temp directory
-- and load the exported graph back.
withExampleGraph :: (LoadResult -> IO a) -> IO a
withExampleGraph k = withSystemTempDirectory "graphos-review" $ \tmp -> do
  input <- makeAbsolute "example/ts-lsp-test"
  logEnv <- defaultLogEnv LogError
  obsEnv <- initObservability LogError defaultOtelConfig Nothing (tmp </> "debug")
  let out = tmp </> "out"
      cfg = defaultConfig
        { cfgInputPath = input, cfgOutputDir = out, cfgNoViz = True
        , cfgNoCluster = True, cfgFresh = True, cfgMetricsPort = Nothing
        , cfgNoObservability = True }
  r <- runPipeline (productionAppEnv logEnv obsEnv) cfg
  case r of
    Left err -> expectationFailure ("pipeline failed: " ++ T.unpack err) >> error "unreachable"
    Right _ -> do
      lr <- loadGraphFromFile (out </> "graph.json")
      either (\e -> expectationFailure (T.unpack e) >> error "unreachable") k lr

-- ───────────────────────────────────────────────
-- Lean toy fixtures (ReviewGraph.lean §8)
-- ───────────────────────────────────────────────

toyCorpus :: [(FilePath, Text)]
toyCorpus =
  [ ("src/index.ts", "import { dbGetById } from './db';\nimport { User } from './types';\nexport const u: User | undefined = dbGetById(1);\n")
  , ("src/db.ts", "import { User, UserId } from './types';\nlet nextId: UserId = 1;\nexport function generateId(): UserId { return nextId++; }\nexport function dbGetById(id: UserId): User | undefined { return undefined; }\n")
  , ("src/types.ts", "export type UserId = number;\nexport interface User { id: UserId }\n") ]

-- | Extract the toy corpus with the production tree-sitter path and resolve
-- imports against it, as the pipeline does between extraction and build.
toyGraph :: IO Graph
toyGraph = withSystemTempDirectory "graphos-toy" $ \root -> do
  mapM_ (\(rel, body) -> do
          createDirectoryIfMissing True (takeDirectory (root </> rel))
          BS.writeFile (root </> rel) (TE.encodeUtf8 body)) toyCorpus
  lang <- maybe (fail "no typescript grammar") pure (getGrammarPtr "typescript")
  exs <- mapM (\(rel, _) -> do
                 src <- BS.readFile (root </> rel)
                 ts <- maybe (fail "parse") pure =<< parseWithGrammar lang src
                 pure (tsNodesToExtraction (mkSourceFile rel "") GranularityFunction defaultTruncationBudget ts))
              toyCorpus
  let merged = Extraction (Map.unions (map extractionNodes exs)) (Map.unions (map extractionEdges exs))
  pure (buildGraph False (fst (resolveCorpusImports [] merged)))

spec :: Spec
spec = do
  describe "4.3 / 5.3 — the pipeline on example/ts-lsp-test" $ do
    it "path index.ts db.ts is a path of imports hops between the two module nodes" $
      withExampleGraph $ \lr -> do
        let g = lrGraph lr
            idx = lrIndex lr
        case pathQueryWithIndex g idx "index.ts" "db.ts" of
          Nothing -> expectationFailure "no path from index.ts to db.ts"
          Just path -> do
            let ends = take 1 path ++ take 1 (reverse path)
            map (kindOf g) ends `shouldBe` [Just "Module", Just "Module"]
            map (fmap (T.unpack . T.takeWhileEnd (/= '/')) . fileOf g) ends
              `shouldBe` [Just "index.ts", Just "db.ts"]
            path `shouldSatisfy` importsHops g

    it "symbols finds a function by its identifier and a module by its file name" $
      withExampleGraph $ \lr -> do
        let g = lrGraph lr
            idx = lrIndex lr
            hit name = [ (snKind sn, takeFileName (T.unpack (snSourceFile sn))) | sn <- srFound (symbolLookup name g idx) ]
        hit "dbGetById" `shouldBe` [(Just "Function", "db.ts")]
        hit "service.ts" `shouldBe` [(Just "Module", "service.ts")]

    it "a near miss is not found and suggests the nearest identifiers" $
      withExampleGraph $ \lr -> do
        let r = symbolLookup "dbGetByIdd" (lrGraph lr) (lrIndex lr)
        srNotFound r `shouldBe` True
        take 3 (srSuggestions r) `shouldSatisfy` any (T.isPrefixOf "dbgetbyid" . T.toLower)

  describe "6.3 — checker parity with lean/ReviewGraph.lean §8" $ do
    it "the index.ts → db.ts → types.ts chain lifts to paths of imports hops" $ do
      g <- toyGraph
      let idx = buildIndex g Map.empty
      mapM_ (\(a, b) -> case pathQueryWithIndex g idx a b of
                Nothing -> expectationFailure ("no path " ++ T.unpack a ++ " → " ++ T.unpack b)
                Just p  -> p `shouldSatisfy` importsHops g)
        [("index.ts", "db.ts"), ("db.ts", "types.ts"), ("index.ts", "types.ts")]

    it "resolve \"dbGetById\" is single, \"dbGetByIdentifier\" is not found" $ do
      g <- toyGraph
      let idx = buildIndex g Map.empty
      case resolveNodeArg "dbGetById" g idx of
        ResolvedSingle nid -> (kindOf g nid, fileOf g nid) `shouldBe` (Just "Function", Just "src/db.ts")
        other -> expectationFailure ("expected a single match, got " ++ show other)
      resolveNodeArg "dbGetByIdentifier" g idx `shouldBe` NotFound

    it "the cypher evaluator filters before the budget: [2] where the old order gave []" $ do
      -- evalOld 1 (· == 2) [1, 2] = []  /  evalNew 1 (· == 2) [1, 2] = [2]
      let node nid = Node
            { nodeId = nid, nodeLabel = fromText nid, nodeFileType = CodeFile
            , nodeSourceFile = fromText "toy.ts", nodeSource = Nothing
            , nodeLineStart = Just 1, nodeLineEnd = Just 1, nodeSignature = Nothing
            , nodeCommunityId = Nothing, nodeKind = Just (fromText "Function")
            , nodeDegree = Nothing, nodeIsBridge = Nothing, nodeExtra = Nothing
            , nodePresentBits = 0 }
          g = buildGraph True (extractionFromLists [node "1", node "2"] [])
          emptyIdx = GraphIndex Map.empty Map.empty Map.empty Map.empty
      case parseQuery "MATCH (n) WHERE n.label = '2' RETURN n.label" of
        Left err -> expectationFailure (show err)
        Right ast -> do
          let r = evaluate 1 ast g emptyIdx
          length (crRows r) `shouldBe` 1
          T.pack (show (crRows r)) `shouldSatisfy` T.isInfixOf "\"2\""
