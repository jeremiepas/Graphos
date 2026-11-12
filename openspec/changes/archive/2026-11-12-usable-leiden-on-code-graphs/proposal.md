## Why

Two defects block using graphos's own Leiden clustering on code repositories
(GitHub #74, #75), measured on a 1,679-file TypeScript repository (src/ only,
tree-sitter mode):

- `--cluster-only` computes communities from the checkpoint and exits without
  writing `graph.json` or `community_graph.json` (#74).
- The clustered pass adds 214,853 `documents` edges from one doc file under
  `src/` (co-location links every doc node to every code node of the subtree),
  `--edge-density Sparse` keeps them, and on a `--directed` graph Leiden puts
  1,609 of 1,679 files in one community (#75). The collapse comes from
  forward-only neighbours: every sink node has no neighbour and the
  small-community merge falls back to the largest community.

## What Changes

- `--cluster-only` persists its result: `graph.json` (atomic, communities
  filled), report exports, and `community_graph.json` under
  `--community-graph`. The checkpoint is kept.
- Pipeline co-location links one node per doc file to one node per code file,
  and skips a doc whose subtree holds more than 25 code files.
- `--edge-density Sparse` drops the co-location heuristic; symbol-mention and
  path-reference links stay.
- Community detection and cohesion run on the undirected projection of the
  graph, so `--directed` changes the exported edges, not the partition.
- `addEdges` merges per-node adjacency sets instead of keeping the old set
  (left-biased `Map.union` dropped added edges from the adjacency).

## Capabilities

### Modified Capabilities
- `checkpoint-controls`: cluster-only writes its outputs.
- `community-detection`: partition on the undirected projection; bounded
  `documents` co-location; Sparse excludes co-location.

## Impact

- `UseCase/Pipeline/Core.hs`, `UseCase/Infer.hs`, `UseCase/Infer/Document.hs`,
  `UseCase/Cluster.hs`, `Domain/Community.hs`, `Domain/Graph/Core.hs`.
- Partitions of `--directed` builds change (no longer collapsed). Graphs with
  doc files under large source trees get far fewer `documents` edges.
