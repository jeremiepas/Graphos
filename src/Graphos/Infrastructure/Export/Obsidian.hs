-- | Obsidian vault export - full implementation
-- Generates one markdown note per node, community overview notes, and graph.canvas
module Graphos.Infrastructure.Export.Obsidian
  ( exportObsidian
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors, degree)

-- | Export as Obsidian vault
exportObsidian :: Graph -> Analysis -> FilePath -> IO ()
exportObsidian g analysis dir = do
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True (dir ++ "/nodes")
  createDirectoryIfMissing True (dir ++ "/communities")

  -- Create one note per node
  mapM_ (writeNodeNote g dir) (Map.toList (gNodes g))

  -- Create community overview notes
  let commMap = analysisCommunities analysis
      cohesion = analysisCohesion analysis
  mapM_ (writeCommunityNote g dir cohesion) (Map.toList commMap)

  -- Create graph.canvas
  writeCanvasFile g dir commMap

-- | Write a single node's markdown note
writeNodeNote :: Graph -> FilePath -> (NodeId, Node) -> IO ()
writeNodeNote g dir (nid, n) = do
  let filename = sanitizeFilename (nodeLabel n) ++ ".md"
      filepath = dir ++ "/nodes/" ++ filename
      nbs = Set.toList (neighbors g nid)
      neighborLinks = [formatNeighbor g nb | nb <- nbs]
      frontmatter = T.unlines
        [ "---"
        , "id: " <> quoteWrap nid
        , "label: " <> quoteWrap (nodeLabel n)
        , "source_file: " <> quoteWrap (nodeSourceFile n)
        , "file_type: " <> T.pack (show (nodeFileType n))
        , "degree: " <> T.pack (show (degree g nid))
        , "---"
        ]
      content = T.unlines
        [ "# " <> nodeLabel n
        , ""
        , "## Properties"
        , "- **Source**: " <> nodeSourceFile n
        , "- **Type**: " <> T.pack (show (nodeFileType n))
        , "- **Degree**: " <> T.pack (show (degree g nid))
        , ""
        , "## Connections"
        , T.intercalate "\n" neighborLinks
        ]
  writeFile filepath (T.unpack (frontmatter <> content))

-- | Write a community overview note
writeCommunityNote :: Graph -> FilePath -> CohesionMap -> (CommunityId, [NodeId]) -> IO ()
writeCommunityNote g dir cohesion (cid, members) = do
  let filename = "_COMMUNITY_" ++ show cid ++ ".md"
      filepath = dir ++ "/communities/" ++ filename
      score = Map.findWithDefault 0.0 cid cohesion
      memberList = T.intercalate ", " [formatNodeLink nb | nid <- members
                                         , let nb = Map.findWithDefault (Node nid "unknown" CodeFile "" Nothing Nothing Nothing Nothing Nothing) nid (gNodes g)]
      content = T.unlines
        [ "# Community " <> T.pack (show cid)
        , ""
        , "## Overview"
        , "- **Members**: " <> T.pack (show (length members))
        , "- **Cohesion**: " <> T.pack (show score)
        , ""
        , "## Members"
        , memberList
        ]
  writeFile filepath (T.unpack content)

-- | Write graph.canvas file for Obsidian
writeCanvasFile :: Graph -> FilePath -> CommunityMap -> IO ()
writeCanvasFile g dir commMap = do
  let filepath = dir ++ "/graph.canvas"
      nodesSection = concatMap (uncurry (formatCanvasNodesInCommunity g)) (Map.toList commMap)
      edgesSection = [formatCanvasEdge src tgt | ((src, tgt), _) <- Map.toList (gEdges g)]
      canvas = unlines (["---", "nodes:"] ++ nodesSection ++ ["edges:"] ++ edgesSection)
  writeFile filepath canvas

formatCanvasNodesInCommunity :: Graph -> CommunityId -> [NodeId] -> [String]
formatCanvasNodesInCommunity _g cid members =
  [ "  - id: " ++ show (cid * 1000 + idx) ++ "\n    node: " ++ T.unpack nid ++ "\n    community: " ++ show cid
  | (idx, nid) <- zip [0..] members
  ]

formatCanvasEdge :: NodeId -> NodeId -> String
formatCanvasEdge src tgt =
  "  - from: " ++ T.unpack src ++ "\n    to: " ++ T.unpack tgt

-- ───────────────────────────────────────────────
-- Formatting helpers
-- ───────────────────────────────────────────────

formatNeighbor :: Graph -> NodeId -> Text
formatNeighbor g nbId =
  case Map.lookup nbId (gNodes g) of
    Just nb -> "- [[" <> nodeLabel nb <> "]]"
    Nothing -> "- " <> nbId

formatNodeLink :: Node -> Text
formatNodeLink n = "[[" <> nodeLabel n <> "]]"

quoteWrap :: Text -> Text
quoteWrap t = "\"" <> t <> "\""

sanitizeFilename :: Text -> String
sanitizeFilename = T.unpack . T.map safeChar
  where
    safeChars :: String
    safeChars = "/\\:*?\"<>|"
    safeChar c
      | c `elem` safeChars = '_'
      | otherwise = c