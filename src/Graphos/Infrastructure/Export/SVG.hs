-- | SVG export - proper graph visualization with force-directed layout
module Graphos.Infrastructure.Export.SVG
  ( exportSVG
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors, degree)

-- | Export graph as SVG
exportSVG :: Graph -> Analysis -> FilePath -> IO ()
exportSVG g analysis path = do
  let nodeCount = Map.size (gNodes g)
  if nodeCount > 5000
    then writeFile path "<!-- Graph too large for SVG visualization. Use Obsidian vault instead. -->"
    else writeFile path (T.unpack (generateSVG g analysis))

-- | Generate SVG with circular layout (simple, deterministic)
generateSVG :: Graph -> Analysis -> Text
generateSVG g analysis =
  let nodes = Map.toList (gNodes g)
      nodeCount = length nodes
      width = max 800 (nodeCount * 8 + 200)
      height = max 600 (nodeCount * 6 + 200)
      cx = width `div` 2
      cy = height `div` 2
      radius = min cx cy - 50
      -- Assign positions in a circle
      positions :: Map NodeId (Int, Int)
      positions = Map.fromList
        [ (nid, (cx + round (fromIntegral radius * cos (2 * pi * fromIntegral i / fromIntegral nodeCount))
                , cy + round (fromIntegral radius * sin (2 * pi * fromIntegral i / fromIntegral nodeCount))))
        | (i, (nid, _)) <- zip [0..] nodes ]
      commMap = analysisCommunities analysis
      -- Build SVG elements
      edgeSvg = T.concat [renderEdge positions src tgt | ((src, tgt), _) <- Map.toList (gEdges g)]
      nodeSvg = T.concat [renderNode positions commMap nid n | (nid, n) <- nodes]
  in T.unlines
       [ "<?xml version='1.0' encoding='UTF-8'?>"
       , "<svg xmlns='http://www.w3.org/2000/svg' width='" <> T.pack (show width) <> "' height='" <> T.pack (show height) <> "'>"
       , "  <style>"
       , "    .node { fill: #4a90d9; stroke: #2c5f8a; stroke-width: 1; }"
       , "    .node:hover { fill: #e8a838; }"
       , "    .edge { stroke: #999; stroke-width: 0.5; opacity: 0.6; }"
       , "    .label { font-family: monospace; font-size: 8px; fill: #333; }"
       , "  </style>"
       , edgeSvg
       , nodeSvg
       , "</svg>"
       ]

-- | Render a single node as SVG circle + label
renderNode :: Map NodeId (Int, Int) -> CommunityMap -> NodeId -> Node -> Text
renderNode positions commMap nid n =
  case Map.lookup nid positions of
    Nothing -> ""
    Just (x, y) ->
      let color = communityColor commMap nid
          labelT = T.take 15 (nodeLabel n)
      in T.concat
        [ "  <circle class='node' cx='", T.pack (show x), "' cy='", T.pack (show y)
        , "' r='5' fill='", color, "' title='", nodeLabel n, "'/>"
        , "\n  <text class='label' x='", T.pack (show (x + 7)), "' y='", T.pack (show (y + 3))
        , "'>", escapeSvg labelT, "</text>\n"
        ]

-- | Render a single edge as SVG line
renderEdge :: Map NodeId (Int, Int) -> NodeId -> NodeId -> Text
renderEdge positions src tgt =
  case (Map.lookup src positions, Map.lookup tgt positions) of
    (Just (x1, y1), Just (x2, y2)) ->
      T.concat [ "  <line class='edge' x1='", T.pack (show x1), "' y1='", T.pack (show y1)
               , "' x2='", T.pack (show x2), "' y2='", T.pack (show y2), "'/>\n" ]
    _ -> ""

-- | Get community color for a node
communityColor :: CommunityMap -> NodeId -> Text
communityColor commMap nid =
  let mCid = findCommunity nid commMap
  in case mCid of
       Nothing -> "#4a90d9"  -- default blue
       Just cid -> communityPalette (cid `mod` 12)

-- | Find which community a node belongs to
findCommunity :: NodeId -> CommunityMap -> Maybe CommunityId
findCommunity nid commMap =
  case [cid | (cid, members) <- Map.toList commMap, nid `elem` members] of
    (cid:_) -> Just cid
    []      -> Nothing

-- | 12-color palette for communities
communityPalette :: Int -> Text
communityPalette 0  = "#e41a1c"  -- red
communityPalette 1  = "#377eb8"  -- blue
communityPalette 2  = "#4daf4a"  -- green
communityPalette 3  = "#984ea3"  -- purple
communityPalette 4  = "#ff7f00"  -- orange
communityPalette 5  = "#ffff33"  -- yellow
communityPalette 6  = "#a65628"  -- brown
communityPalette 7  = "#f781bf"  -- pink
communityPalette 8  = "#999999"  -- grey
communityPalette 9  = "#66c2a5"  -- teal
communityPalette 10 = "#fc8d62"  -- salmon
communityPalette 11 = "#8da0cb"  -- light blue
communityPalette _  = "#4a90d9"  -- default

-- | Escape special SVG characters
escapeSvg :: Text -> Text
escapeSvg = T.replace "&" "&amp;"
          . T.replace "<" "&lt;"
          . T.replace ">" "&gt;"
          . T.replace "\"" "&quot;"