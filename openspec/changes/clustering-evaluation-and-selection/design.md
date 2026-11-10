## Context

Source method: E-SC4R (arXiv:2107.01766v2). Its pipeline is
*dependency matrix → clustering configurations → MoJoFM against a
directory-derived ground truth → pick the configuration per system*. Graphos
already has the dependency graph and one algorithm (Leiden). This change adds
the missing measuring stick, a second algorithm family, and the selection loop.

## Pipeline

```
graph.json (any granularity)
   │  fold to files (contains/defines collapse, all relations → file→file, counts)
   ▼
file dependency matrix W (F × F, symmetric: W = A + Aᵀ)
   │
   ├──► Leiden (auto-tuned)  ─────────────┐
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
| File folding, distance matrix, linkage, dendrogram cut | Domain | `Graphos.Domain.Community.Agglomerative` |
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

The closed form for `max(mno(∀A,B))` is taken from the paper and checked by
brute force: for n ≤ 6 the test enumerates every partition A (Bell(6) = 203)
and asserts the maximum of `mno` equals the closed form.

### D4 — Agglomerative clustering

| Parameter | Grid (defaults) |
|---|---|
| Feature vector of file i | row i of W (edge counts, both directions) |
| Distance | euclidean, manhattan, cosine (cosine of a zero row = 1) |
| Linkage | single, average, complete |
| Cut | K = ⌈F / q⌉ for q ∈ {5, 7, 10, 15, 20, 25} (the paper's configuration suffixes such as `cosine_single_25` are read as this divisor; configurable) |
| Determinism | ties broken by smallest (cluster id, cluster id) pair |
| Cap | F ≤ 10,000 (`--agglomerative-max-files`); above it the run is skipped with a reported reason |

Algorithms: single linkage via a minimum spanning tree (Prim, O(F²) time,
O(F) extra space over the distance matrix); average and complete via the
nearest-neighbour-chain algorithm (O(F²) time, valid because both linkages
are reducible). Distance matrix O(F²) memory — at F = 10,000 that is ~400 MB
of `Double`s in an unboxed upper triangle, which is why the cap exists.

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
