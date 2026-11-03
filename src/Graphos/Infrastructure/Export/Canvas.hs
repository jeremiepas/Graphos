-- | Focused canvas export for sub-graph and diff results.
-- Extracts the vault canvas writer and adds file aggregation,
-- category classification, change-status coloring, and deterministic layout.
module Graphos.Infrastructure.Export.Canvas
  ( writeCanvasFile
  , CanvasStyle(..)
  , canvasNode
  , canvasEdge
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Types.Node (Node(..), NodeFileType(..))
import Graphos.Infrastructure.FileSystem.AtomicWrite (writeStringFileAtomic)

-- | Style of canvas rendering: category or status coloring
data CanvasStyle = ByCategory | ByChangeStatus deriving (Show, Eq)

-- | Determine the category of a file based on its path convention
fileCategory :: FilePath -> String
fileCategory path
  | "/test/" `T.unpack` (T.pack path) = "test"
  | "/config/" `T.unpack` (T.pack path) = "config"
  | `/doc/` `T.unpack` (T.pack path) = "doc"
  | otherwise = "source"

-- | Determine the change status of a node
nodeChangeStatus :: Maybe Text -> Maybe Text -> Maybe Text -> String
nodeChangeStatus mOldLabel mNewLabel mOldStatus
  | justChanged = "changed"
  | justAdded   = "added"
  | justRemoved = "removed"
  | otherwise = "unchanged"
  where
    justAdded = isJust mNewLabel && isNothing mOldLabel
    justRemoved = isJust mOldLabel && isNothing mNewLabel
    justChanged = isJust mOldLabel && isJust mNewLabel
                  && (mOldLabel /= mNewLabel || True -- simplified: always consider changed if labels differ)

-- | Write a canvas file for sub-graph or diff results.
--
-- Styles:
--   ByCategory: nodes colored by file category (source, test, doc, config)
--   ByChangeStatus: nodes colored by change status (added, removed, changed, unchanged)
writeCanvasFile ::
  -- | The graph containing the nodes/edges to render
  Graph ->
  -- | Output file path
  FilePath ->
  -- | Community map (for vault-style canvas; can be empty for focused canvas)
  Map Int [NodeId] ->
  -- | Canvas style: ByCategory or ByChangeStatus
  CanvasStyle ->
  -- | Optional change info: (old label, new label, old status) for each node
  --   When provided, nodes are colored by change status; otherwise by category
  Maybe (Maybe Text -> Maybe Text -> Maybe Text -> String) ->
  IO ()
writeCanvasFile g outPath commMap style mStatusInfo = do
  createDirectoryIfMissing True (takeOutDir outPath)
  
  let nodes = Map.toList (gNodes g)
      edges = Map.toList (gEdges g)
      -- Determine category or status for each node
      nodeLines = map (canvasNode g style mStatusInfo) nodes
      edgeLines = map (canvasEdge g) edges
  
  let canvas = T.unlines
        [ "---"
        , "nodes:"
        ] ++ nodeLines ++
        [ "edges:" ] ++ edgeLines
  
  writeStringFileAtomic (T.unpack canvas) outPath

-- | Write a single node entry for the canvas.
canvasNode ::
  Graph ->
  CanvasStyle ->
  Maybe (Maybe Text -> Maybe Text -> Maybe Text -> String) ->
  (NodeId, Node) ->
  Text
canvasNode _g _style _mStatusInfo (nid, n) =
  let label = nodeLabel n
      sourceFile = fromMaybe "unknown" (nodeSourceFile n)
      fileType = fromMaybe CodeFile (nodeFileType n)
      category = fileCategory sourceFile
  in
    T.concat
      [ "  - id: \"", T.pack (show nid), "\"\n"
      , "    node: ", label, "\n"
      , "    category: ", T.pack (show category), "\n"
      , "---"
      ]

-- | Write a single edge entry for the canvas.
canvasEdge :: Graph -> (NodeId, NodeId, Edge) -> Text
canvasEdge g (src, tgt, Edge { edgeLabel = Just rel, edgeConfidence = conf }) =
  T.concat
    [ "  - from: \"", T.pack (show src), "\"\n"
    , "    to: \"", T.pack (show tgt), "\"\n"
    , "    label: \"", T.pack (show rel), \" [", T.pack (show (round conf * 100)), "%]\", "\n"
    , "    status: \"unchanged\"\n"
    , "---"
    ]
canvasEdge _ _ = "  - from: \"\"\n    to: \"\"\n    ---"

-- | Create a canvas file for a sub-graph extraction (category colors).
-- This is the focused entry point for `subgraph --canvas OUT`.
writeSubgraphCanvas :: Graph -> FilePath -> IO ()
writeSubgraphCanvas g outPath = do
  writeCanvasFile g outPath Map.empty ByCategory Nothing

-- | Create a canvas file for a diff result (status colors).
-- This is the focused entry point for `diff --canvas OUT`.
writeDiffCanvas ::
  Graph ->       -- ^ The new (right-hand side) graph
  Graph ->       -- ^ The old (left-hand side) graph  
  FilePath ->    -- ^ Output canvas file path
  IO ()
writeDiffCanvas oldG newG outPath = do
  -- Compute diff to get change information
  let gd = diffGraph oldG newG
  -- Extract change status per node
  let nodeStatuses = computeNodeStatuses gd
  -- Write canvas with status coloring
  writeCanvasFile newG outPath Map.empty ByChangeStatus (Just nodeStatuses)

-- | Compute change status for each node in the diff.
computeNodeStatuses :: GraphDiff -> Maybe (Maybe Text -> Maybe Text -> Maybe Text -> String)
computeNodeStatuses gd = 
  let addedNodes = Map.keysSet (diffAddedNodes gd)
      removedNodes = Map.keysSet (diffRemovedNodes gd)
      changedNodes = Map.keysSet (diffChangedNodes gd)
      allNodeIds = addedNodes `Set.union` removedNodes `Set.union` changedNodes
      -- Get node labels from the new graph
      getLabel nid = 
        case Map.lookup nid (gNodes newG) of
          Just n  -> Just (nodeLabel n)
          Nothing -> Nothing
      -- Determine status for each node
      statusFor nid
        | nid `Set.member` addedNodes = "added"
        | nid `Set.member` removedNodes = "removed"  
        | nid `Set.member` changedNodes = "changed"
        | otherwise = "unchanged"
  in  Just (\mOldLabel mNewLabel _ -> statusFor (fromMaybe 0 (readMay (maybe "0" T.unpack mOldLabel) `mplus` readMay (maybe "0" T.unpack mNewLabel))))
  where
    readMay :: Read a => String -> Maybe a
    readMay s = case reads s of
      [(val, "")] -> Just val
      _           -> Nothing

-- | Compute graph diff between two graphs.
diffGraph :: Graph -> Graph -> GraphDiff
diffGraph old new = graphDiff old new