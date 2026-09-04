module Graphos.CLI.ParserSpec where

import Test.Hspec
import Data.List (isInfixOf)
import Options.Applicative
import Options.Applicative.Help (renderHelp, parserHelp)

import Graphos.CLI.Parser
import Graphos.Domain.Types (PipelineConfig(..))

parseServe :: [String] -> Either String Command
parseServe args =
  case execParserPure defaultPrefs (info serveOpts idm) args of
    Success cmd -> Right cmd
    Failure f   -> Left $ fst $ renderFailure f "serve"
    CompletionInvoked _ -> Left "completion"

parseWith :: Parser Command -> [String] -> Either String Command
parseWith p args =
  case execParserPure defaultPrefs (info p idm) args of
    Success cmd -> Right cmd
    Failure f   -> Left $ fst $ renderFailure f "cmd"
    CompletionInvoked _ -> Left "completion"

parsePipeline :: [String] -> Either String PipelineConfig
parsePipeline args =
  case execParserPure defaultPrefs (info pipelineOpts idm) args of
    Success cfg -> Right cfg
    Failure f   -> Left $ fst $ renderFailure f "run"
    CompletionInvoked _ -> Left "completion"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _       = False

spec :: Spec
spec = do
  describe "renderCommandReference" $ do
    it "produces non-empty output" $ do
      length (lines renderCommandReference) `shouldSatisfy` (> 0)

    it "starts and ends with fenced code block markers" $ do
      case lines renderCommandReference of
        (first:rest) -> do
          first `shouldBe` "```"
          case reverse rest of
            []     -> expectationFailure "no closing fence"
            (l:_)  -> l `shouldBe` "```"
        [] -> expectationFailure "empty reference"

    it "is within the 60-line budget" $ do
      length (lines renderCommandReference) `shouldSatisfy` (<= 60)

    it "contains key subcommands" $ do
      renderCommandReference `shouldSatisfy` isInfixOf "graphos query"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos cypher"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos path"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos explain"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos symbols"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos neighbors"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos ingest"
      renderCommandReference `shouldSatisfy` isInfixOf "graphos init"

    it "contains key flags" $ do
      renderCommandReference `shouldSatisfy` isInfixOf "--update"
      renderCommandReference `shouldSatisfy` isInfixOf "--no-viz"
      renderCommandReference `shouldSatisfy` isInfixOf "--graph"
      renderCommandReference `shouldSatisfy` isInfixOf "--budget"
      renderCommandReference `shouldSatisfy` isInfixOf "--dfs"
      renderCommandReference `shouldSatisfy` isInfixOf "--edges"
      renderCommandReference `shouldSatisfy` isInfixOf "--api-only"
      renderCommandReference `shouldSatisfy` isInfixOf "--no-api"

    it "lists --json for the query family" $ do
      renderCommandReference `shouldSatisfy` isInfixOf "--json"
      renderCommandReference `shouldSatisfy` isInfixOf "--label-width"

  describe "query family uniform flag surface" $ do
    it "query accepts --json / --label-width / --edges all / --budget" $ do
      parseWith queryOpts ["q", "--json", "--label-width", "80", "--edges", "all", "--budget", "1000"] `shouldSatisfy` isRight
    it "query accepts --edges semantic" $ do
      parseWith queryOpts ["q", "--edges", "semantic"] `shouldSatisfy` isRight
    it "query rejects an unknown --edges mode" $ do
      parseWith queryOpts ["q", "--edges", "bogus"] `shouldSatisfy` isLeft
    it "query still accepts --dfs and --budget" $ do
      parseWith queryOpts ["q", "--dfs", "--budget", "1000"] `shouldSatisfy` isRight
    it "path accepts --json" $ do
      parseWith pathOpts ["a", "b", "--json"] `shouldSatisfy` isRight
    it "explain accepts --json" $ do
      parseWith explainOpts ["node", "--json"] `shouldSatisfy` isRight
    it "neighbors accepts a display-name argument (metavar widened)" $ do
      parseWith neighborsOpts ["Some.Display.Name", "--depth", "1"] `shouldSatisfy` isRight
    it "cypher accepts a positional query plus --json / --budget" $ do
      parseWith cypherOpts ["MATCH (n) RETURN n", "--json", "--budget", "100"] `shouldSatisfy` isRight

  describe "serveOpts" $ do
    it "parses default serve command" $ do
      parseServe [] `shouldBe` Right (Serve "graphos-out" "graphos-out/graph.json" 8080 False False)

    it "parses --graph" $ do
      parseServe ["--graph", "other/graph.json"] `shouldBe`
        Right (Serve "graphos-out" "other/graph.json" 8080 False False)

    it "parses --api-only" $ do
      parseServe ["--api-only"] `shouldBe`
        Right (Serve "graphos-out" "graphos-out/graph.json" 8080 True False)

    it "parses --no-api" $ do
      parseServe ["--no-api"] `shouldBe`
        Right (Serve "graphos-out" "graphos-out/graph.json" 8080 False True)

    it "parses combined flags" $ do
      parseServe ["--dir", "static", "--graph", "g.json", "--port", "9090", "--api-only"] `shouldBe`
        Right (Serve "static" "g.json" 9090 True False)

  describe "pipelineOpts semantic-edges flags" $ do
    it "defaults both flags to False" $ do
      parsePipeline [] `shouldSatisfy` \case
        Right cfg -> cfgNoSemanticEdges cfg == False && cfgForceSemanticEdges cfg == False
        Left _    -> False

    it "--no-semantic-edges sets cfgNoSemanticEdges" $ do
      parsePipeline ["--no-semantic-edges"] `shouldSatisfy` \case
        Right cfg -> cfgNoSemanticEdges cfg == True
        Left _    -> False

    it "--force-semantic-edges sets cfgForceSemanticEdges" $ do
      parsePipeline ["--force-semantic-edges"] `shouldSatisfy` \case
        Right cfg -> cfgForceSemanticEdges cfg == True
        Left _    -> False

    it "accepts both flags together" $ do
      parsePipeline ["--no-semantic-edges", "--force-semantic-edges"] `shouldSatisfy` \case
        Right cfg -> cfgNoSemanticEdges cfg == True && cfgForceSemanticEdges cfg == True
        Left _    -> False

    it "--help lists both semantic-edges flags" $ do
      let helpText = renderHelp 80 (parserHelp defaultPrefs (infoParser (info pipelineOpts idm)))
      helpText `shouldSatisfy` isInfixOf "--no-semantic-edges"
      helpText `shouldSatisfy` isInfixOf "--force-semantic-edges"

  describe "RTS profiling flags" $ do
    it "defaults both cfgRtsProfile and cfgMaxHeap to False/Nothing" $ do
      parsePipeline [] `shouldSatisfy` \case
        Right cfg -> cfgRtsProfile cfg == False && cfgMaxHeap cfg == Nothing
        Left _    -> False

    it "--rts-profile sets cfgRtsProfile to True" $ do
      parsePipeline ["--rts-profile"] `shouldSatisfy` \case
        Right cfg -> cfgRtsProfile cfg == True
        Left _    -> False

    it "--max-heap 1G sets cfgMaxHeap to Just 1024" $ do
      parsePipeline ["--max-heap", "1G"] `shouldSatisfy` \case
        Right cfg -> cfgMaxHeap cfg == Just 1024
        Left _    -> False

    it "--max-heap 512M sets cfgMaxHeap to Just 512" $ do
      parsePipeline ["--max-heap", "512M"] `shouldSatisfy` \case
        Right cfg -> cfgMaxHeap cfg == Just 512
        Left _    -> False

    it "--max-heap 2048 sets cfgMaxHeap to Just 2048 (plain number)" $ do
      parsePipeline ["--max-heap", "2048"] `shouldSatisfy` \case
        Right cfg -> cfgMaxHeap cfg == Just 2048
        Left _    -> False

    it "accepts both --rts-profile and --max-heap together" $ do
      parsePipeline ["--rts-profile", "--max-heap", "4G"] `shouldSatisfy` \case
        Right cfg -> cfgRtsProfile cfg == True && cfgMaxHeap cfg == Just 4096
        Left _    -> False

    it "--help lists --rts-profile and --max-heap" $ do
      let helpText = renderHelp 80 (parserHelp defaultPrefs (infoParser (info pipelineOpts idm)))
      helpText `shouldSatisfy` isInfixOf "--rts-profile"
      helpText `shouldSatisfy` isInfixOf "--max-heap"