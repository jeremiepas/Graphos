-- | HTML export - interactive graph visualization with community coloring
-- Uses external JSON data files to avoid 1MB+ inline JSON that breaks browsers.
module Graphos.Infrastructure.Export.HTML
  ( exportHTML
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=), encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors, articulationPoints)
import Graphos.Domain.Community (cohesionScore)
import qualified Data.Set as Set

-- | Community color palette (distinct, accessible colors)
communityColors :: [Text]
communityColors =
  [ "#7dd3fc", "#f472b6", "#34d399", "#fbbf24", "#a78bfa"
  , "#fb923c", "#2dd4bf", "#f87171", "#818cf8", "#4ade80"
  , "#e879f9", "#38bdf8", "#facc15", "#fb7185", "#22d3ee"
  , "#c084fc"
  ]

colorForCommunity :: Int -> Text
colorForCommunity cid = communityColors !! (cid `mod` length communityColors)

-- | Escape strings for safe JSON embedding
escapeLabel :: Text -> Text
escapeLabel t = T.replace "\\" "\\\\" (T.replace "\"" "\\\"" (T.replace "\n" " " t))

-- | Export graph as interactive HTML with vis.js
-- Writes three files: graph.html, graph_nodes.json, graph_edges.json
exportHTML :: Graph -> Analysis -> FilePath -> IO ()
exportHTML g analysis htmlPath = do
  let baseDir = takeDirectory htmlPath
      nodesPath = baseDir ++ "/graph_nodes.json"
      edgesPath = baseDir ++ "/graph_edges.json"
      -- Use relative filenames for fetch() (same directory as HTML)
      nodesFetch = "graph_nodes.json"
      edgesFetch = "graph_edges.json"
  
  -- Write node and edge data as separate JSON files
  BSL.writeFile nodesPath (encode (nodesToJSON g (analysisCommunities analysis) (articulationPoints g)))
  BSL.writeFile edgesPath (encode (edgesToJSON g))
  
  -- Write the HTML that loads them
  let html = buildHTML g analysis nodesFetch edgesFetch
  writeFile htmlPath (T.unpack html)

  where
    takeDirectory path = case reverse (dropWhile (/= '/') (reverse path)) of
      ""  -> "."
      dir -> dir

-- | Build the HTML page - loads data via fetch() instead of inlining
buildHTML :: Graph -> Analysis -> FilePath -> FilePath -> Text
buildHTML g analysis nodesPath edgesPath =
  T.unlines
    [ "<!DOCTYPE html>"
    , "<html><head>"
    , "<meta charset='utf-8'>"
    , "<title>Graphos Knowledge Graph</title>"
    , "<script src='https://unpkg.com/vis-network/standalone/umd/vis-network.min.js'></script>"
    , "<style>"
    , "  * { box-sizing: border-box; margin: 0; padding: 0; }"
    , "  body { background: #0f0f1a; color: #e0e0e0; font-family: 'Inter', -apple-system, sans-serif; height: 100vh; display: flex; flex-direction: column; }"
    , "  header { background: #1a1a2e; padding: 12px 20px; border-bottom: 1px solid #2a2a4e; display: flex; justify-content: space-between; align-items: center; }"
    , "  header h1 { font-size: 18px; color: #7dd3fc; }"
    , "  header p { font-size: 12px; color: #888; margin-top: 4px; }"
    , "  .layout { display: flex; height: calc(100vh - 52px); }"
    , "  #graph { flex: 1; }"
    , "  #sidebar { width: 300px; background: #1a1a2e; border-left: 1px solid #2a2a4e; padding: 16px; overflow-y: auto; }"
    , "  #sidebar h3 { color: #7dd3fc; font-size: 14px; margin-bottom: 8px; }"
    , "  #sidebar p { font-size: 12px; color: #888; margin-bottom: 12px; }"
    , "  .comm-item { background: #252540; border-radius: 6px; padding: 8px 10px; margin-bottom: 6px; font-size: 12px; cursor: pointer; }"
    , "  .comm-item:hover { background: #2a2a50; }"
    , "  .comm-item strong { color: #e0e0e0; }"
    , "  .comm-item .members { color: #888; }"
    , "  .comm-item .cohesion { color: #4ade80; font-size: 11px; }"
    , "  .legend { margin-top: 12px; border-top: 1px solid #2a2a4e; padding-top: 12px; }"
    , "  .legend-item { display: flex; align-items: center; gap: 6px; font-size: 11px; margin-bottom: 4px; }"
    , "  .legend-dot { width: 10px; height: 10px; border-radius: 50%; }"
    , "  #loading { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); font-size: 18px; color: #7dd3fc; }"
    , "  .selected-info { background: #252540; border-radius: 6px; padding: 8px 10px; margin-bottom: 8px; font-size: 11px; display: none; }"
    , "  .selected-info .label { color: #e0e0e0; font-weight: bold; font-size: 13px; }"
    , "  .selected-info .detail { color: #888; margin-top: 4px; }"
    , "</style>"
    , "</head><body>"
    , "<header>"
    , "  <div>"
    , "    <h1>Graphos Knowledge Graph</h1>"
    , "    <p>" <> statsText <> "</p>"
    , "  </div>"
    , "</header>"
    , "<div class='layout'>"
    , "  <div id='graph'><div id='loading'>Loading graph...</div></div>"
    , "  <div id='sidebar'>"
    , "    <div class='selected-info' id='selectedInfo'>"
    , "      <div class='label' id='selectedLabel'></div>"
    , "      <div class='detail' id='selectedDetail'></div>"
    , "    </div>"
    , "    <h3>Communities</h3>"
    , "    <p>Click a community to focus on it. Click a node for details.</p>"
    , commInfo
    , "    <div class='legend'>"
    , "      <h3>Legend</h3>"
    , "      <div class='legend-item'><div class='legend-dot' style='background:#7dd3fc'></div> Community member</div>"
    , "      <div class='legend-item'><div class='legend-dot' style='background:#f87171; border: 2px solid #f87171'></div> Bridge node</div>"
    , "      <div class='legend-item'><div style='width:20px; border-top: 2px dashed #6a6a8a'></div> Inferred edge</div>"
    , "    </div>"
    , "  </div>"
    , "</div>"
    , "<script>"
    , "  async function loadGraph() {"
    , "    const container = document.getElementById('graph');"
    , "    const nodesResp = await fetch('" <> T.pack nodesPath <> "');"
    , "    const edgesResp = await fetch('" <> T.pack edgesPath <> "');"
    , "    const nodes = await nodesResp.json();"
    , "    const edges = await edgesResp.json();"
    , "    document.getElementById('loading').style.display = 'none';"
    , "    const options = {"
    , "      nodes: { shape: 'dot', size: 14, font: { color: '#e0e0e0', size: 11, face: 'Inter, sans-serif', strokeWidth: 0 }, borderWidth: 2, borderWidthSelected: 4 },"
    , "      edges: {"
    , "        arrows: { to: { enabled: true, scaleFactor: 0.5, type: 'arrow' } },"
    , "        color: { color: '#8b8baa', highlight: '#7dd3fc', hover: '#a0a0cc' },"
    , "        shadow: { enabled: false },"
    , "        smooth: { type: 'continuous', roundness: 0.3 },"
    , "        font: { color: '#777', size: 9, strokeWidth: 0, align: 'middle' }"
    , "      },"
    , "      physics: { stabilization: { iterations: 150 }, barnesHut: { gravitationalConstant: -5000, centralGravity: 0.1, springLength: 80, springConstant: 0.04 } },"
    , "      interaction: { hover: true, tooltipDelay: 300, navigationButtons: true, keyboard: true }"
    , "    };"
    , "    const network = new vis.Network(container, { nodes, edges }, options);"
    , "  "
    , "    // Click node to show details"
    , "    network.on('click', function(params) {"
    , "      if (params.nodes.length > 0) {"
    , "        const nodeId = params.nodes[0];"
    , "        const node = nodes.find(n => n.id === nodeId);"
    , "        if (node) {"
    , "          document.getElementById('selectedInfo').style.display = 'block';"
    , "          document.getElementById('selectedLabel').textContent = node.label;"
    , "          document.getElementById('selectedDetail').textContent = node.title || '';"
    , "        }"
    , "      } else {"
    , "        document.getElementById('selectedInfo').style.display = 'none';"
    , "      }"
    , "    });"
    , "  }"
    , "  loadGraph().catch(err => {"
    , "    document.getElementById('loading').textContent = 'Error loading graph: ' + err.message;"
    , "  });"
    , "</script>"
    , "</body></html>"
    ]
  where
    statsText = T.pack $ show (Map.size $ gNodes g) ++ " nodes, "
             ++ show (Map.size $ gEdges g) ++ " edges, "
             ++ show (length $ analysisCommunities analysis) ++ " communities, "
             ++ show (length $ articulationPoints g) ++ " bridge nodes"
    commInfo = communitiesToHTML (analysisCommunities analysis) g

-- | Convert communities to sidebar HTML with cohesion scores
communitiesToHTML :: CommunityMap -> Graph -> Text
communitiesToHTML commMap g =
  let items = [T.unlines
        [ "    <div class='comm-item' onclick=\"this.style.opacity='0.7'\">"
        , "      <strong style='color:" <> colorForCommunity cid <> "'>Community " <> T.pack (show cid) <> "</strong>"
        , "      <span class='members'> — " <> T.pack (show (length members)) <> " members</span>"
        , "      <br><span class='cohesion'>cohesion: " <> T.pack (take 5 (show (cohesionScore g members))) <> "</span>"
        , "      <br><span class='members'>" <> T.intercalate ", " (take 3 [nodeLabel n | nid <- members, Just n <- [Map.lookup nid (gNodes g)]]) <> "</span>"
        , "    </div>"
        ]
        | (cid, members) <- Map.toList commMap]
  in T.concat items

-- ───────────────────────────────────────────────
-- JSON data generation (using Aeson, NOT manual string building)
-- ───────────────────────────────────────────────

-- | Node data for JSON export
data VisNode = VisNode
  { vnId      :: Text
  , vnLabel   :: Text
  , vnTitle   :: Text
  , vnBgColor :: Text
  , vnBorder  :: Text
  , vnBw      :: Int
  , vnGroup   :: Int
  } deriving (Show)

instance ToJSON VisNode where
  toJSON n = object
    [ "id"         .= vnId n
    , "label"      .= vnLabel n
    , "title"      .= vnTitle n
    , "color"      .= object ["background" .= vnBgColor n, "border" .= vnBorder n]
    , "borderWidth" .= vnBw n
    , "group"      .= vnGroup n
    ]

-- | Edge data for JSON export
data VisEdge = VisEdge
  { veFrom   :: Text
  , veTo     :: Text
  , veTitle  :: Text
  , veLabel  :: Text
  , veDashes :: Bool
  , veWidth  :: Int
  , veColor  :: Text
  } deriving (Show)

instance ToJSON VisEdge where
  toJSON e = object
    [ "from"   .= veFrom e
    , "to"     .= veTo e
    , "title"  .= veTitle e
    , "label"  .= veLabel e
    , "dashes" .= veDashes e
    , "width"  .= veWidth e
    , "color"  .= object ["color" .= veColor e, "highlight" .= ("#7dd3fc" :: Text), "hover" .= ("#a0a0cc" :: Text)]
    , "arrows" .= object ["to" .= object ["enabled" .= True, "scaleFactor" .= (0.5 :: Double)]]
    ]

-- | Convert graph nodes to JSON (via Aeson, no manual string building)
nodesToJSON :: Graph -> CommunityMap -> [NodeId] -> [VisNode]
nodesToJSON g commMap artPoints =
  let nodeCommMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]
      artSet = Set.fromList artPoints
  in [ VisNode
       { vnId      = nodeId n
       , vnLabel   = nodeLabel n
       , vnTitle   = nodeSourceFile n <> " [" <> T.pack (show cid) <> "]"
       , vnBgColor = if nid `Set.member` artSet then "#f87171" else colorForCommunity cid
       , vnBorder  = if nid `Set.member` artSet then "#f87171" else "#333"
       , vnBw      = if nid `Set.member` artSet then 3 else 1
       , vnGroup   = cid
       }
      | n <- Map.elems (gNodes g)
      , let nid = nodeId n
      , let cid = Map.findWithDefault (-1) nid nodeCommMap
      ]

-- | Convert graph edges to JSON (via Aeson, no manual string building)
edgesToJSON :: Graph -> [VisEdge]
edgesToJSON g =
  [ VisEdge
    { veFrom   = edgeSource e
    , veTo     = edgeTarget e
    , veTitle  = relLabel
    , veLabel  = relLabel
    , veDashes = isInferred
    , veWidth  = if isInferred then 1 else 2
    , veColor  = if isInferred then "#6a6a8a" else "#8b8baa"
    }
  | ((_, _), e) <- Map.toList (gEdges g)
  , let isInferred = edgeConfidence e == Inferred
        relLabel = relationToText (edgeRelation e)
  ]