-- | Report generation - produces GRAPH_REPORT.md
module Graphos.UseCase.Report
  ( generateReport
  ) where

import Data.List (intercalate, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors)

-- | Generate a markdown report.
--
-- Pure rendering (bounded-report-export): connectivity figures and cohesion
-- come from the 'Analysis' record — the values computed once during
-- clustering/analysis — never recomputed here, so rendering does no
-- whole-graph work.
generateReport :: Graph -> Analysis -> PipelineConfig -> Detection -> Maybe (Map.Map CommunityId Text) -> Text
generateReport g analysis _config _detection mLabels =
  T.unlines
    [ "# Graph Report"
    , ""
    , "## Summary"
    , ""
    , T.pack $ "Nodes: " ++ show (Map.size (gNodes g))
    , T.pack $ "Edges: " ++ show (Map.size (gEdges g))
    , T.pack $ "Communities: " ++ show (Map.size (analysisCommunities analysis))
    , T.pack $ "Articulation points: " ++ show (length artPoints)
    , T.pack $ "Biconnected components: " ++ show (analysisBccCount analysis)
    , ""
    , "## Communities"
    , ""
    , communitiesSection (analysisCommunities analysis) (analysisCohesion analysis) g mLabels
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
    artPoints = analysisArticulation analysis

-- | Format cohesion score to 2 decimal places
fmtCohesion :: Double -> String
fmtCohesion d = take 5 (show d)

-- | Communities table. Cohesion comes from the clustering-time map (same
-- keys as the community map by construction: producers derive it with
-- @scoreAllCohesion@ / @fmap@ over the same communities); a missing key
-- renders 0.0 rather than triggering a whole-graph recomputation.
communitiesSection :: CommunityMap -> CohesionMap -> Graph -> Maybe (Map.Map CommunityId Text) -> Text
communitiesSection commMap cohesionMap g mLabels =
  let header = "| Community | Members | Cohesion | Top Nodes |"
      sep    = "|-----------|---------|----------|-----------|"
      rows   = [T.pack $ "| " ++ show cid ++ " | " ++ show (length members)
                       ++ " | " ++ fmtCohesion (Map.findWithDefault 0.0 cid cohesionMap)
                       ++ " | " ++ intercalate ", " (take 3 [T.unpack (toText (nodeLabel n)) | nid <- members, Just n <- [Map.lookup nid (gNodes g)]])
                       ++ " |"
                | (cid, members) <- Map.toList commMap]
      labelRows = case mLabels of
        Just labels | not (Map.null labels) ->
            let header' = "\n### Community Labels (LLM)\n"
                sep'    = "| Community | Label |"
                sep2    = "|-----------|-------|"
                rows'   = [T.pack $ "| " ++ show cid ++ " | " ++ T.unpack lbl ++ " |"
                          | (cid, lbl) <- Map.toList labels]
            in T.unlines (header' : sep' : sep2 : rows')
        _ -> ""
  in T.unlines (header : sep : rows) <> labelRows

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
  T.unlines $ map renderSurprise $ dedupSurprises surprises
  where
    renderSurprise s = "- **" <> scSource s <> " -> " <> scTarget s <> "** (" <> scRelation s <> ") " <> scWhy s
    dedupSurprises = map NE.head . NE.groupBy sameSurprise . sortOn key
      where
        key s = (scSource s, scTarget s, scRelation s, scWhy s)
        sameSurprise a b = key a == key b


questionsSection :: [SuggestedQuestion] -> Text
questionsSection questions =
  T.unlines $ map (\q -> "- " <> maybe "No question" id (sqQuestion q) <> " — " <> sqWhy q) questions