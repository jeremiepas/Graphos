module Graphos.CLI.ParserSpec where

import Test.Hspec
import Data.List (isInfixOf)
import Options.Applicative

import Graphos.CLI.Parser

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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

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
    it "query accepts --json / --label-width / --edges" $ do
      parseWith queryOpts ["q", "--json", "--label-width", "80", "--edges"] `shouldSatisfy` isRight
    it "query still accepts --dfs and --budget" $ do
      parseWith queryOpts ["q", "--dfs", "--budget", "1000"] `shouldSatisfy` isRight
    it "path accepts --json" $ do
      parseWith pathOpts ["a", "b", "--json"] `shouldSatisfy` isRight
    it "explain accepts --json" $ do
      parseWith explainOpts ["node", "--json"] `shouldSatisfy` isRight
    it "neighbors accepts a display-name argument (metavar widened)" $ do
      parseWith neighborsOpts ["Some.Display.Name", "--depth", "1"] `shouldSatisfy` isRight

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