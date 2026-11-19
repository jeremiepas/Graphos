## Context

Source method: E-SC4R (arXiv:2107.01766v2). Its pipeline is
*dependency matrix → clustering configurations → MoJoFM against a
directory-derived ground truth → pick the configuration per system*. Graphos
already has the dependency graph and one algorithm (Leiden). This change adds
the missing measuring stick, a second algorithm family, and the selection loop.

## Pipeline

```
graph.json (any granularity)
   │  Bunch file-level MDG (code files, clustering.bunch.relations, edge weights)
   ▼
file dependency matrix W (F × F, symmetric: the weighted MDG pairs)
   │
   ├──► Leiden (auto-tuned)  ─────────────┐
   ├──► Bunch (clustering.bunch.*)  ───────┤
   ├──► agglomerative(dist, link, K) ×grid ┤──► candidate partitions P₁…Pₙ
   │                                       │
directory ground truth T (depth d,         │
  ∩ over prior refs) ──────────────────────┴──► MoJoFM(Pᵢ, T), stability
                                                   │
                                                   ▼
                                     ranking report → selected partition
```

## Decisions

### D1 — Where the code lives (clean architecture)

| Piece | Layer | Module |
|---|---|---|
| Partition type, ground truth, MoJo, MoJoFM, stability | Domain | `Graphos.Domain.Community.Partition` |
| Distance matrix over the MDG's `W`, linkage, dendrogram cut (the fold is `Bunch.foldToFileMDGWith`) | Domain | `Graphos.Domain.Community.Agglomerative` |
| Sweep, ranking, selection | UseCase | `Graphos.UseCase.ClusterEval` |
| Algorithm dispatch in the cluster stage | UseCase | `Graphos.UseCase.Cluster` |
| Load several graphs, write JSON / markdown report, CLI | Infrastructure | CLI parser, `app/Main.hs`, Export |

Alternatives: putting MoJo in `UseCase` (rejected — it is a pure function of
two partitions); a separate executable (rejected — reuses graph loading).

### D2 — Ground truth from directories

| Rule | Value |
|---|---|
| Group of a file | its directory path truncated to depth `d` below the corpus root |
| Default `d` | 2; `--truth-depth N` overrides |
| Multi-ref stability (E-SC4R) | given graphs from refs r₁…rₘ, keep files present in all; two files share a group iff they share it in every ref (the meet of the partitions) |
| Explicit truth | `--truth FILE`, JSON `{ "<file>": "<group>" }`, for a hand-made reference architecture |

Alternatives: package/module declarations (language-specific, rejected for
now); a single release's directories (kept as the `m = 1` case — the paper's
point is that multi-ref intersection removes transient moves).

### D3 — MoJo and MoJoFM

| Item | Definition |
|---|---|
| Domain | both partitions restricted to their common node set (reported count of dropped nodes) |
| `mno(A,B)` | MoJo distance of Wen & Tzerpos 2004: tag each cluster of A to a group of B, maximum bipartite matching, moves + joins |
| `MoJoFM(A,B)` | `(1 − mno(A,B) / max(mno(∀A,B))) × 100`, B is the reference; asymmetric |
| Stability | `MoJoFM(P_old, P_new)` on common nodes, reported both directions |
| Edge case | `max(mno(∀A,B)) = 0` (single node) → 100 |

Alternatives: a2a / c2c (more recent architecture-recovery metrics; can be
added later behind the same interface), NMI / ARI (not move/join-based, kept
out to stay comparable with the paper).

Closed form used for `max(mno(∀A,B))`: with B's group sizes sorted
descending `g₁ ≥ g₂ ≥ …`, `max(mno(∀A,B)) = n − h`, where `h` is the largest
`k` with `gᵢ ≥ k − i + 1` for every `i ≤ k` (the longest "staircase" of group
sizes; `h = 1` for one group or all singletons). It was derived by exhaustive
search and is checked by brute force: for n ≤ 6 the test enumerates every
partition A (Bell(6) = 203) for every reference B and asserts the maximum of
`mno` equals the closed form (it was also checked for n = 7 when derived).
`mno` itself is the optimal one-way MoJo: tag each cluster of A with a
maximal-overlap group of B, `M` = elements outside their cluster's tag,
`g` = maximum bipartite matching over the maximal-overlap pairs,
`mno = M + l − g` with `l` the number of clusters of A.

### D4 — Agglomerative clustering

| Parameter | Grid (defaults) |
|---|---|
| Feature vector of file i | row i of W (summed edge weights, both directions) with `W[i][i]` = the row's largest weight — without it, two files that depend only on each other have orthogonal rows |
| Distance | euclidean, manhattan, cosine (cosine of a zero row = 1) |
| Linkage | single, average, complete |
| Cut | K = ⌈F / q⌉ for q ∈ {5, 7, 10, 15, 20, 25} (the paper's configuration suffixes such as `cosine_single_25` are read as this divisor; configurable) |
| Determinism | ties broken by smallest (cluster id, cluster id) pair |
| Cap | F ≤ 10,000 (`--agglomerative-max-files`); above it the run is skipped with a reported reason |

Algorithm (all three linkages): the generic agglomerative algorithm with a
cached nearest neighbour per cluster and Lance–Williams updates (min,
size-weighted mean, max). Each step merges the globally closest pair by
`(distance, cluster id, cluster id)` — exactly the naive O(F³) rule, so the
result equals the naive reference, ties included; typical cost O(F²), O(F³)
only in degenerate tie patterns. A minimum spanning tree cut (single) or the
nearest-neighbour chain (average, complete) were the first plan; both resolve
a tie at the cut differently from the cluster-id rule (found by the
naive-reference property), so they were dropped. Distance matrix O(F²)
memory — at F = 10,000 that is ~400 MB of `Double`s in an unboxed upper
triangle, which is why the cap exists.

Alternatives: naive O(F³) merging (rejected, too slow above a few thousand
files); approximate/sparse linkage (deferred); clustering at function
granularity (rejected — the paper works at class level and ground truth is
per file).

### D5 — Selection

| Mode | Behaviour |
|---|---|
| `clustereval score` | MoJoFM of the graph's stored partition (folded to files by majority community) vs truth |
| `clustereval compare A B` | MoJoFM and stability between two graphs' partitions |
| `clustereval sweep` | Leiden + agglomerative grid, ranked by MoJoFM desc, then by fewer singleton clusters, then by config name |
| `--cluster-algorithm auto` | build runs the sweep and writes the winner's file partition back to node communities (symbols inherit their file's cluster) |

Alternatives: the paper's SVM over 160 code metrics (rejected — needs a
training corpus; noted as future work, the sweep report already records the
per-config scores such a model would learn from); optimising modularity
instead of MoJoFM (rejected — that is what Leiden already does).

## Risks

- Directory truth rewards "looks like the folders"; a codebase whose folders
  are wrong will steer selection wrong. Mitigation: `--truth FILE` and the
  report always shows modularity/cohesion next to MoJoFM.
- Folding symbols to files can hide cross-cutting files. The report lists
  files whose symbols were split across Leiden communities.

## Verification

- `cabal build --flag dev` (-Werror) and `cabal test`.
- Hspec + QuickCheck: MoJo properties (identity = 0, MoJoFM(A,A) = 100,
  brute-force max for n ≤ 6), linkage outputs on hand-sized fixtures,
  determinism (same input → same partition), cap behaviour.
- Smoke: `graphos clustereval sweep` on the Graphos repo itself and on one
  solario-core graph; record the ranking in the change's tasks log.
