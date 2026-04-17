-- | Report generation - produces GRAPH_REPORT.md
module Graphos.UseCase.Report
  ( generateReport
  ) where

import Data.List (intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, godNodes, gNodes, gEdges, neighbors, articulationPoints, biconnectedComponents)
import Graphos.Domain.Community (cohesionScore)

-- | Generate a markdown report
generateReport :: Graph -> Analysis -> PipelineConfig -> Detection -> Text
generateReport g analysis config detection =
  T.unlines
    [ "# Graph Report"
    , ""
    , "## Summary"
    , ""
    , T.pack $ "Nodes: " ++ show (Map.size (gNodes g))
    , T.pack $ "Edges: " ++ show (Map.size (gEdges g))
    , T.pack $ "Communities: " ++ show (Map.size (analysisCommunities analysis))
    , T.pack $ "Articulation points: " ++ show (length artPoints)
    , T.pack $ "Biconnected components: " ++ show (length bccs)
    , ""
    , "## Communities"
    , ""
    , communitiesSection (analysisCommunities analysis) g
    , ""
    , "## God Nodes (Top Hubs)"
    , ""
    , godNodesSection (analysisGodNodes analysis)
    , ""
    , "## Bridge Nodes (Articulation Points)"
    , ""
    , bridgeNodesSection artPoints g
    , ""
    , "## Surprising Connections"
    , ""
    , surprisesSection (analysisSurprises analysis)
    , ""
    , "## Suggested Questions"
    , ""
    , questionsSection (analysisQuestions analysis)
    ]
  where
    artPoints = articulationPoints g
    bccs = biconnectedComponents g

-- | Format cohesion score to 2 decimal places
fmtCohesion :: Double -> String
fmtCohesion d = take 5 (show d)

communitiesSection :: CommunityMap -> Graph -> Text
communitiesSection commMap g =
  let header = "| Community | Members | Cohesion | Top Nodes |"
      sep    = "|-----------|---------|----------|-----------|"
      rows   = [T.pack $ "| " ++ show cid ++ " | " ++ show (length members)
                       ++ " | " ++ fmtCohesion (cohesionScore g members)
                       ++ " | " ++ intercalate ", " (take 3 [T.unpack (nodeLabel n) | nid <- members, Just n <- [Map.lookup nid (gNodes g)]])
                       ++ " |"
               | (cid, members) <- Map.toList commMap]
  in T.unlines (header : sep : rows)

godNodesSection :: [GodNode] -> Text
godNodesSection nodes =
  let header = "| Node | Edges |"
      sep    = "|------|-------|"
      rows   = map (\g -> "| " <> gnLabel g <> " | " <> T.pack (show (gnEdges g)) <> " |") nodes
  in T.unlines (header : sep : rows)

bridgeNodesSection :: [NodeId] -> Graph -> Text
bridgeNodesSection artPoints g =
  if null artPoints
    then "_No articulation points found — graph is well-connected._"
    else let header = "| Bridge Node | Degree |"
             sep    = "|-------------|--------|"
             rows   = [ "| " <> nid <> " | " <> T.pack (show (Set.size (neighbors g nid))) <> " |"
                       | nid <- artPoints]
         in T.unlines (header : sep : rows)

surprisesSection :: [SurprisingConnection] -> Text
surprisesSection surprises =
  T.unlines $ map (\s -> "- **" <> scSource s <> " → " <> scTarget s <> "** (" <> scRelation s <> ") " <> scWhy s) surprises

questionsSection :: [SuggestedQuestion] -> Text
questionsSection questions =
  T.unlines $ map (\q -> "- " <> maybe "No question" id (sqQuestion q) <> " — " <> sqWhy q) questions