-- | Landscape orchestration (bunch-search-based-modularization 5.2, 6.2):
-- the Leiden point is present exactly when the graph carries communities, a
-- control MDG bypasses the graph, and the JSON output carries the
-- documented keys.
module Graphos.UseCase.ClusterEvalSpec (spec) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Numeric (showFFloat)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Community.Bunch (mdgFileCount)
import Graphos.Domain.Community.Landscape (LandscapeResult(..), RandomGraphSpec(..), Basin(..), RunRecord(..))
import Graphos.UseCase.Cluster (joinCommunitiesToNodes)
import Graphos.UseCase.ClusterEval

spec :: Spec
spec = describe "UseCase.ClusterEval landscape" $ do
  it "emits N runs with seeds s..s+N-1 sorted by final MQ" $ do
    let rep = landscapeReport input (Just plain)
        runs = lrRuns (lrResult rep)
    length runs `shouldBe` 6
    map rrSeed runs `shouldMatchList` [3 .. 8]
    map rrFinalMQ runs `shouldBe` scanl1 max (map rrFinalMQ runs)

  it "omits the Leiden point when the graph carries no communities" $
    lrLeiden (landscapeReport input (Just plain)) `shouldBe` Nothing

  it "folds the graph's communities to a Leiden point when present" $ do
    let point = lrLeiden (landscapeReport input (Just clustered))
    fmap lpClusters point `shouldBe` Just 2
    -- Communities {a,b} / {c,d}: two intra edges of three, MQ = 2·(2/(2·1+1)).
    fmap lpIntraFraction point `shouldSatisfy` maybe False (\x -> abs (x - 2 / 3) < 1e-12)
    fmap lpMQ point `shouldSatisfy` maybe False (\x -> abs (x - 2 * (2 / 3)) < 1e-12)

  it "runs a control MDG without any graph" $ do
    let rep = landscapeReport input { liControl = Just (RndSpec 12 0.3) } Nothing
    mdgFileCount (lrmdg rep) `shouldBe` 12
    lrLeiden rep `shouldBe` Nothing
    length (lrRuns (lrResult rep)) `shouldBe` 6

  it "encodes config, mdg, runs, similarity, basins, random_baseline and leiden" $ do
    let keysOf rep = case landscapeJSON input rep of
          Object o -> map Key.toText (KeyMap.keys o)
          _        -> []
        withLeiden = keysOf (landscapeReport input (Just clustered))
        without    = keysOf (landscapeReport input (Just plain))
    withLeiden `shouldMatchList`
      ["config", "mdg", "runs", "similarity", "basins", "random_baseline", "leiden"]
    without `shouldMatchList`
      ["config", "mdg", "runs", "similarity", "basins", "random_baseline"]

  it "names every basin in the markdown summary" $ do
    let rep = landscapeReport input (Just clustered)
        summary = landscapeSummary rep
        basins = lrBasins (lrResult rep)
    basins `shouldNotBe` []
    mapM_ (\b -> summary `shouldSatisfy` T.isInfixOf (T.pack ("| " ++ showFFloat (Just 4) (basinKey b) " |")))
          basins
    summary `shouldSatisfy` T.isInfixOf "Leiden point"
  where
    input = defaultLandscapeInput { liSeed = 3, liRuns = 6, liRandomPartitions = 20 }

-- | Four files: a–b and c–d strongly coupled, one b→c edge between.
plain :: Graph
plain = buildGraph False $ extractionFromLists
  [ nodeIn "a.ts" "a1", nodeIn "b.ts" "b1", nodeIn "c.ts" "c1", nodeIn "d.ts" "d1" ]
  [ edgeOf "a1" "b1", edgeOf "c1" "d1", edgeOf "b1" "c1" ]

clustered :: Graph
clustered = joinCommunitiesToNodes plain (Map.fromList [(0, ["a1", "b1"]), (1, ["c1", "d1"])])

nodeIn :: Text -> Text -> Node
nodeIn file nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText file
  , nodeSource = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

edgeOf :: Text -> Text -> Edge
edgeOf u v = Edge
  { edgeId = EdgeId (u <> "->" <> v)
  , edgeSource = u
  , edgeTarget = v
  , edgeRelation = Calls
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }
