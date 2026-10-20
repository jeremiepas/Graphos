# Design: community-level incremental update adoption (AVI-1291, element G6)

Adopts `docs/math/incremental-edge-updates.md` (AVI-519) into an OpenSpec capability. Notation: `docs/math-requirements/consolidated-requirements.md` §0, extended with the churn objects of AVI-519 §1.

## 1. Math approach

**Churn.** Between runs `t` and `t+1`, the churn is `Δ = (R, A)` with `R ⊆ V_t` removed nodes, `A ⊆ V_{t+1}` added nodes (a replacement with coinciding ids changes payload in place, `R = A = ∅` for that id); equivalently `Δ ≡ graphDiff G_t G_{t+1}` (`GraphDiff`). Churn scope:

```
k    = |R| + |A|
N(Δ) = ⋃_{n ∈ R ∪ A} adj(n)                -- neighbours of churned nodes
C(Δ) = { communities touching R ∪ A } ∪ { communities adjacent to R ∪ A }
```

`|N(Δ)| ≤ 2E_Δ` (`E_Δ` = edges incident to churn), `|C(Δ)| ≤ |C_t| + k` — computable from `gAdjFwd/gAdjBack` alone.

**Update operator.** `Update(G, Δ) = G'` with `V' = (V \ R) ∪ A`, `E'` = surviving edges (all edges incident to `R` removed: `E'_survive = E \ { e : endpoints(e) ∩ R ≠ ∅ }`) plus edges of `A` plus inferred edges whose source-of-truth lies in `N(Δ)`, and partition `P'` over `V'`.

**Theorems carried into the spec.**
- **Theorem 1 (incremental edge equivalence / PROP-EQV).** `keys(gEdges G') = keys(gEdges(full rebuild on G_{t+1}))` and likewise for nodes: `Map` ops keyed by `(source,target)` are associative/commutative on disjoint keys, deletions and insertions commute ⇒ incremental = full rebuild. Central validity claim.
- **Lemma 1 (partition validity).** Restricting `P_t` to surviving communities and assigning each added node to exactly one community keeps a valid partition of `V'` (cover + disjoint; INV-PARTITION).
- **Lemma 2 (modularity delta correctness).** `ΔQ` is computable from local terms at churned nodes + affected communities without rescanning all edges (`m`/`W` re-derivable from `E_Δ`); this is the `leiden-scalability` one-pass modularity-gain scheme, and it forbids per-candidate rescans (INV-MODULARITY; directed formula `Q_δ` per `gDirected`).
- **Theorem 2 (termination).** Bounded by `resMaxIterations` + strictly-increasing `Q_γ` on improving moves ⇒ termination.
- **Theorem 3 (complexity).** `O(k log k + |N(Δ)|·polylog + E_Δ·polylog)` vs `O(N + E)` full rebuild — sub-linear when `k ≪ N`, `E_Δ ≪ E`.

**Feasibility honesty (AVI-519 §7).** The current `runIncrementalPipeline` builds the graph from the changed files' `[extraction]` only (no merge into retained `graph.json`, no `deleteEdgesTouching` for `R`) — PROP-EQV is currently **unsatisfied**. The spec records the requirement; the gap is closed in the implementation change (head-of-dev), not silently.

## 2. Requirements register → spec mapping

| Req (AVI-519 §8) | Statement | Classification | Acceptance |
|---|---|---|---|
| REQ-EQV / PROP-EQV | `Update(G,Δ) = G_{t+1}` (node+edge key sets) | new-feature | incremental vs fresh-run `gHash` equality on fixtures; Theorem 1 property |
| REQ-PARTITION | `P'` valid partition; every added node assigned; untouched communities preserved; re-cluster scope `C(Δ)` only | new-feature | INV-PARTITION property + preservation scenario (Lemma 1) |
| REQ-INFER-BOUNDED | inferred-edge update touches only `C(Δ)` scopes; fan-out caps respected | new-feature | INV-FANOUT counts, INV-DEDUP `nubBy` reference equality |
| REQ-REMOVAL-EFFICIENT | removal via reverse adjacency in `O(deg)` (`deleteNodeIncident`), never `O(E)` partition scan | new-feature | complexity/micro-benchmark vs `O(E)` |
| REQ-MODULARITY | `Q(G')` recomputed over `E'` in the correct class (directed/weighted per `gDirected`), one-pass delta | new-feature | INV-MODULARITY two-way recomputation equality |
| REQ-DETERMINISTIC | same churn ⇒ same `gHash` regardless of event batching order | new-feature | INV-HASH-DET property (shuffled mutation order) |
| REQ-CHECKPOINT | resume from any stage reproduces the uninterrupted `G'` | new-feature | INV-CHECKPOINT (checkpointed vs non-checkpointed share `gHash`) |

## 3. Surface grounding

- **Capabilities**: new `community-incremental-update`; consumed unchanged: `02-incremental-pipeline` (Workflow 02/03 + checkpoint resume — the workflow being constrained; its cache-key requirements are NOT this capability), `community-detection` (Leiden phases, `cohesionScore`), `leiden-scalability` (CSR + one-pass gain + `resMaxIterations`), `bounded-edge-inference` (fan-out caps `maxCommunityBridges = 10000`, `maxLabelFanOut = 20`, `maxSemanticFanOut = 50`; log-linear dedup).
- **Modules constrained**: `src/Graphos/Domain/Graph/Core.hs` (`buildGraph:96` dangling drop, `mergeGraphs:140`), `src/Graphos/Domain/Graph/Mutation.hs` (`deleteEdgesTouching`, `rebuildAdjacency`; required helper `deleteNodeIncident`), `src/Graphos/Domain/Graph/Diff.hs` / `Types/Graph.hs` (`graphDiff:127`, `GraphDiff`), `src/Graphos/Domain/Community.hs` (`detectCommunitiesWithResolution:73`, `cohesionScore:385`), `src/Graphos/UseCase/Pipeline/Incremental.hs` (`runIncrementalPipeline:65-99` — the §7 gap).

## 4. Consistency with the consolidated layer

- **Non-conflation with the cache capability:** `incremental-cache-eviction` bounds cache growth; G6 bounds *graph/partition* update scope. The spec states the distinction in its Purpose so the two capabilities cannot be merged by accident.
- **G4 reduces to G2:** the update operator is defined through the merge primitives already pinned by G2/AVI-511 (`mergeGraphs` node-new-wins per `Map.union` on the retained graph, dangling drop of `buildGraph`) and the `GraphDiff` pushout of CT-R3; no new merge semantics introduced; `merge-confluence` untouched.
- **CT-R7 conflict policy:** node-new-wins on the incremental merge path must be pinned by config (AVI-519 §6 point 1), consistent with M4's "winner pinned by fixed order" divergence-set framing.
- **CT-R10 preserved:** re-clustering `C(Δ)` after churn is fresh detection on the current graph — never a merge of community ids across runs, mirroring UQI-4/J1.

## 5. Risks / boundaries

- PROP-EQV is a behavioral *goal* spec'd for the implementation change; until it lands, `--update` output is a changed-files subgraph. The spec's scenarios are written as post-implementation acceptance; the gap is explicitly recorded (AVI-519 §7) so archival does not claim current compliance.
- Modularity class (undirected/weighted/directed) must match `gDirected` — INV-MODULARITY pins the sum over the *actual* edge multiset of `G'`.

## 6. Feasibility gate

Archival gated on `head-of-dev` confirmation of Haskell impact (expected: `Pipeline.Incremental` reconciliation + `Domain.Graph.Mutation.deleteNodeIncident` + property tests; no new dependency) via a comment on [AVI-1291](/AVI/issues/AVI-1291).