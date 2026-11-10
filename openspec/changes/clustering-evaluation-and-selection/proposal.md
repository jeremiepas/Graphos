## Why

Graphos clusters every graph with Leiden and has no way to say whether the
partition is any good for the codebase in front of it. The only quality signal
is modularity/cohesion, which Leiden itself optimises, so it cannot judge
Leiden. On code graphs this has already failed silently (one community holding
1,609 of 1,679 files, see `usable-leiden-on-code-graphs`), and the partition is
what any architecture view (C4 component level, `community_graph.json`) is
built from.

E-SC4R (Tan, Chong, Aleti — *Explaining Software Clustering for
Remodularisation*, arXiv:2107.01766, 2021) shows three things this change
borrows:

1. A usable **ground truth** can be taken from the **directory structure**,
   made robust by keeping only the grouping that is stable across several
   prior releases.
2. **MoJoFM** (Wen & Tzerpos, 2004) is the standard external metric between a
   recovered partition and that ground truth:
   `MoJoFM(A,B) = (1 − mno(A,B) / max(mno(∀A,B))) × 100%`, where `mno` is the
   minimum number of Move and Join operations turning A into B.
3. **No single algorithm wins everywhere.** Over 300 project versions,
   agglomerative hierarchical clustering (distance × linkage × cut) beat Bunch
   291 times, and the best configuration depended on the system. The paper
   picks a configuration per system (SVM over code metrics); a per-repository
   **configuration sweep scored by MoJoFM** gives the same benefit without a
   training corpus.

No PRD.md exists in this repository; section references below point to the
`community-detection` spec, which cites PRD §5.1–§5.4.

## What Changes

- **Directory ground truth**: derive a reference partition of file nodes from
  their directory at a chosen depth, optionally intersected across several
  graphs built from earlier refs (E-SC4R stability rule).
- **Partition comparison**: pure `MoJo`/`MoJoFM` on two partitions of the same
  node set, plus partition **stability** (MoJoFM between partitions of two
  builds on their common nodes).
- **Agglomerative clustering** as a second algorithm, at file granularity:
  feature vector = a file's row in the file-dependency matrix (all edge
  relations folded to file→file, both directions), distance ∈ {euclidean,
  manhattan, cosine}, linkage ∈ {single, average, complete}, dendrogram cut
  at K clusters.
- **Configuration sweep**: run Leiden (the current auto-tuned resolution) and
  the agglomerative grid, score each against the ground truth, report the
  ranking and select the best one.
- **CLI**: `graphos clustereval` (score / compare / sweep, `--json`) and
  `--cluster-algorithm leiden|agglomerative|auto` on the build; `auto` runs
  the sweep. Leiden stays the default; nothing changes unless asked.

## Non-goals

- The SVM / PCA "instance space" algorithm-selection model of the paper (needs
  a labelled corpus of hundreds of systems).
- Bunch / search-based MQ clustering (lost 291/300 in the paper).
- C4 generation itself. The selected file partition is the input a later
  `c4-component-view` change will collapse into a component graph.

## Capabilities

### New Capabilities
- `clustering-evaluation`: directory ground truth, MoJo/MoJoFM, stability,
  `graphos clustereval`.
- `agglomerative-clustering`: file-level hierarchical clustering with the
  distance × linkage × K grid.
- `clustering-selection`: the sweep, its ranking report and `auto` selection.

### Modified Capabilities
- `community-detection`: the clustering algorithm becomes selectable
  (`--cluster-algorithm`), Leiden remaining the default (PRD §5.1).

## Impact

- **Domain** (pure): new `Graphos.Domain.Community.Partition` (ground truth,
  MoJo, MoJoFM, stability), `Graphos.Domain.Community.Agglomerative`.
- **UseCase**: `UseCase/ClusterEval.hs` (score, compare, sweep);
  `UseCase/Cluster.hs` dispatches on the algorithm.
- **Infrastructure / CLI**: `clustereval` subcommand, `--cluster-algorithm`,
  config key `cluster.algorithm`, JSON/markdown report writers.
- **Cost**: agglomerative is O(F²) memory in file count F; it is capped
  (default F ≤ 10,000, beyond that the sweep skips it and says so). Function
  or fine granularity graphs are folded to files first.
- Workflow docs: 01-full-pipeline (cluster stage), 04-query (new subcommand).
