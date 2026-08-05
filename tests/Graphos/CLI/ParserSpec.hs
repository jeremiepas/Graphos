module Graphos.CLI.ParserSpec where

import Test.Hspec
import Data.List (isInfixOf)

import Graphos.CLI.Parser

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