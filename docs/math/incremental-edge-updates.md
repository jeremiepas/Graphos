# Graphos Math Requirements — Incremental Edge Updates Under Node Churn

**Agent:** graph-theory-expert · **Issue:** [AVI-519](/AVI/issues/AVI-519) · **Parent:** [AVI-512](/AVI/issues/AVI-512)
**Status:** requirements complete · **Surface map:** see §0

> Deliverable type: math requirements (definitions → invariants → theorems with proof sketches → complexity → acceptance criteria). No Haskell is written here; implementation is `[Graphos Developer](/AVI/agents/graphos-dev)` / Head of R&D work (see §8).

---

## 0. Scope & Surface Map

This doc constrains the graph-theoretic behaviour invoked by the incremental pipeline
(`Graphos.UseCase.Pipeline.Incremental.runIncrementalPipeline` / `runSingleFilePipeline`) and
by watch mode / single-file ingestion. Every requirement below names the spec header and the
Domain module/type it constrains; nothing here is ungrounded.

| Surface | Spec header | Domain type / function |
|---|---|---|
| Graph model | *(this doc)* | `Graph(..)` in `Graphos.Domain.Graph.Core`; `gNodes :: Map NodeId Node`, `gEdges :: Map (NodeId,NodeId) Edge`, `gAdjFwd/gAdjBack :: Map NodeId (Set NodeId)`, `gDirected :: Bool` |
| Extraction merge | `02-incremental-pipeline` Req "Workflow 02 — incremental pipeline" | `mergeExtractions`, `buildGraph :: Bool -> Extraction -> Graph` (`Core.hs`) |
| Graph merge | `02-incremental-pipeline` Req "Workflow 02 — incremental pipeline" | `mergeGraphs :: Graph -> Graph -> Graph` (`Core.hs`) |
| Node/edge mutation | *(this doc)* | `deleteNode`, `deleteEdgesTouching`, `putEdgeUpsert`, `rebuildAdjacency` in `Graphos.Domain.Graph.Mutation` |
| Diff | *(this doc)* | `graphDiff :: Graph -> Graph -> GraphDiff`, `GraphDiff(..)` in `Graphos.Domain.Graph.Diff` / `Types.Graph` |
| Communities (partition) | `community-detection` Req "Leiden algorithm three phases", "cohesionScore" | `detectCommunities`, `cohesionScore`, `CommunityMap = Map CommunityId [NodeId]` |
| Leiden complexity vocab | `leiden-scalability` Req "Constant-time node moves", "CSR adjacency representation", "One-pass modularity-gain scoring" | `LeidenState`, `clusterGraphWithResolution` |
| Bounded inference | `bounded-edge-inference` Req "Log-linear edge deduplication", "Community bridges derive from real adjacency" | `inferCommunityBridges`, `nubBy`-equivalent O(k log k) dedup |

---

## 1. Definitions

### 1.1 Graph
Graphos produces a **weighted, possibly-directed** property graph
`G = (V, E)`:

- `V ⊆ Nodes`, represented as `gNodes :: Map NodeId Node`; each node carries `nodeKind`,
  `nodeCommunityId :: Maybe Int`, `nodeDegree`, and structural attributes.
- `E ⊆ V × V`, represented as `gEdges :: Map (NodeId,NodeId) Edge`; each edge carries
  `edgeWeight :: Double` and `edgeConfidence :: Confidence`.
- Directedness `gDirected :: Bool` is per-graph and follows `Relation` semantics
  (directed relations ⇒ `gDirected = True`). Adjacency is stored as
  `gAdjFwd` (source → target) and `gAdjBack` (target → source; for undirected graphs this is
  the symmetric closure of `gAdjFwd`).
- `Λ` denotes edge count `|E|` (or total edge weight `W = ½ Σ_{e∈E} edgeWeight e` when weights
  are used in a modularity sum). This is the same complexity vocabulary as
  `leiden-scalability` (N = `|V|`, E = `|E|`).

Dangling edges (endpoint not in `V`) are dropped at build time
(`buildGraph`, `Core.hs:100`) — an invariant reused below (§3, INV-DANGLE).

### 1.2 Churn
A **churn** `Δ` is the change applied between two pipeline runs `t` and `t+1`. It is
formalised two equivalent ways:

- **Set form:** `Δ = (R, A)` where `R ⊆ V_t` are removed nodes and `A ⊆ V_{t+1}` are added
  nodes. A *replacement* (a file re-extracted under the same logical identity) is the pair
  `( {old}, {new} )`; if ids coincide the payload changes in place and `R = A = ∅` for that id.
- **Diff form:** `Δ ≡ graphDiff G_t G_{t+1} :: GraphDiff`
  (`diffAddedNodes / diffRemovedNodes / diffAddedEdges / diffRemovedEdges`, `Diff.hs`).

Define the churn size and its neighbourhood:

```
k    = |R| + |A|                         -- churn cardinality
N(Δ) = ⋃_{n ∈ R ∪ A} adj(n)              -- neighbours of churned nodes (adj = fwd∪back)
C(Δ) = { c : c touches R ∪ A }          -- communities containing a churned node
      ∪ { c' : ∃ edge between R∪A and c' } -- communities adjacent to churned nodes
```

`|N(Δ)| ≤ 2E_Δ` and `|C(Δ)| ≤ |C_t| + k` where `E_Δ` is the number of edges incident to churned
nodes. All quantities are computable from `gAdjFwd/gAdjBack` alone.

### 1.3 Incremental update operator
`Update(G, Δ) = G'` must satisfy, by definition, the **incremental-equivalence property**
(`PROP-EQV`, §4): the result equals the graph a full rebuild on `G_{t+1}` would produce.

`G'` is composed of three parts:

1. `V' = (V \ R) ∪ A` (node set after churn).
2. `E'` = edges of `G` surviving removal of all edges incident to `R`, plus edges induced by
   `A` (from the fresh extraction) plus inferred edges whose *source of truth*
   (§2.2) lies in `N(Δ)`.
3. `P'` = community partition over `V'` (§3, INV-PARTITION).

### 1.4 Communities as a partition
A community assignment is a surjection `P : V → C`. It is **valid** iff:

- **Cover:** `⋃_{c∈C} P^{-1}(c) = V`.
- **Disjoint:** `∀ i≠j : P^{-1}(i) ∩ P^{-1}(j) = ∅`.

Equivalently `CommunityMap = Map CommunityId [NodeId]` has pairwise-disjoint member lists whose
union is `V` and whose total length is `|V|`. This is the **partition-refinement lens**: no node
lost, none duplicated.

### 1.5 Modularity (well-definedness class)
For the graph class Graphos actually emits (weighted, directed where `Relation` says so):

- Undirected unweighted (Newman 2004; Traag et al. 2019, Leiden):
  `Q = (1/2m) Σ_{i,j} [A_ij − (k_i k_j)/(2m)] δ(c_i,c_j)`, `m = |E|`.
- Undirected weighted: substitute `W = ½ Σ w_ij`, `w_ij` for `A_ij`, weighted degree
  `k_i^w = Σ_j w_ij` for `k_i`.
- Directed (Newman 2004, directed modularity):
  `Q_δ = (1/m) Σ_{i,j} [A_ij − (k^{out}_i k^{in}_j)/m] δ(c_i,c_j)`.

`Q` is **well-defined** only when degrees/weights are summed over the *actual* edge multiset of
`G'` (§3, INV-MODULARITY).

---

## 2. Objects whose incremental content must be exact

### 2.1 Surviving edges (removal)
For each `n ∈ R`, every edge `e` with `edgeSource e = n` or `edgeTarget e = n` is removed.
Formally `E'_survive = E \ { e : endpoints(e) ∩ R ≠ ∅ }`.

### 2.2 Inferred edges — source of truth
Inferred edges (community bridges, doc/code references, semantic edges) are **not** global
recomputed objects; they are functions of local adjacency. An inferred edge `f` is *affected by
Δ* iff its generating data (`N(Δ)`, or the community adjacency it depends on) touches churn.
Only affected inferred edges may change; unaffected ones are reused verbatim from `G_t`. This is
what makes incremental inference sub-linear (§5). It is grounded in
`bounded-edge-inference` Req "Community bridges derive from real adjacency" (bridges emitted only
for adjacent community pairs) and the fan-out caps (`maxCommunityBridges = 10000`,
`maxLabelFanOut = 20`, `maxSemanticFanOut = 50`).

### 2.3 Adjacency & hash
After edges change, `gAdjFwd/gAdjBack` and `gHash` (`computeGraphHash`, FNV-1a over sorted node
ids + sorted edge tuples, `Core.hs:259`) must reflect `E'`. Determinism of `gHash` requires
stable ordering of the tuple sorts (`Data.List.sort` on `NodeId`/edge pairs).

---

## 3. Invariants (checkable; each is a QuickCheck-ready property)

Let `Δ = (R, A)`, `G' = Update(G, Δ)`.

- **INV-NODECOUNT** `|V'| = |V| − |R| + |A|`. *(Property: for random `G`, random disjoint
  `R,A`, the resulting node count equals the arithmetic.)* — constrains `mergeGraphs`/`deleteNode`.
- **INV-PARTITION** `P'` is a valid partition of `V'` (cover + disjoint; total member length
  `= |V'|`). *(Property: `S.length . concat . Map.elems $ commMap' == V'` and no id appears twice.)*
  — constrains re-clustering after churn.
- **INV-DANGLE** `E'` has no edge whose endpoint ∉ `V'`. *(Property: every `(s,t) ∈ keys gEdges'`
  has `s,t ∈ keys gNodes'`.)* — constrains `addEdges`/`putEdgeUpsert` post-churn.
- **INV-REMOVAL** `E' ∩ (edges incident to R) = ∅`. *(Property: no surviving edge touches `R`.)*
  — constrains `deleteEdgesTouching`.
- **INV-EQV-EDGE** `E'(G,Δ) = E(G_{t+1})` — the incremental edge set equals a full rebuild on the
  churned graph. *(Property: diff-based update and one-shot `buildGraph`/`mergeGraphs` yield equal
  `gEdges`; test against fixtures.)* — **central correctness claim (§4, THEOREM 1).**
- **INV-MODULARITY** `Q(G')` is computed from `E'` and `P'` using the correct class
  (undirected/weighted/directed per `gDirected`); degree/weight sums use `E'` exactly.
  *(Property: `Q` recomputed two ways — direct sum vs. delta accumulation — agree.)*
- **INV-FANOUT** every inferred edge batch respects its named cap
  (`≤ maxCommunityBridges / maxLabelFanOut / maxSemanticFanOut`). *(Property: counts per cap.)*
- **INV-DEDUP** edge/candidate dedup preserves input order with first-occurrence-wins and equals
  the reference `nubBy`. *(Property: QuickCheck equality vs `nubBy`; mirrors
  `bounded-edge-inference` Req "Log-linear edge deduplication".)*
- **INV-HASH-DET** `gHash(G')` is independent of update *order* (incremental vs. batch); it is a
  pure function of `(V', E')`. *(Property: shuffling the mutation sequence yields identical hash.)*
  — constrains determinism (§6).
- **INV-CHECKPOINT** a resume from any stage (`Detect→Extract→Build→Cluster→Infer+Analyze+Export`,
  `02-incremental-pipeline` Req "checkpoint resume from interruption") reproduces the same `G'`
  as an uninterrupted run. *(Property: checkpointed and non-checkpointed runs share `gHash`.)*

---

## 4. Theorems with proof sketches

### THEOREM 1 (Incremental edge equivalence)
*`Update(G, Δ)` produces `E'` equal to the edge set of a full rebuild on `G_{t+1}`:*
`keys(g'(Δ)) = Map.keys (buildGraph directed (extraction of G_{t+1}))`.

**Proof sketch.** Edge identity is the pair `(source,target)`, stored in a `Map` keyed by that
pair (`gEdges`). A `Map` union/insert/delete is associative, commutative (on disjoint keys) and
idempotent-on-reinsert. Deletion of edges incident to `R` removes exactly the set in §2.1
(monotone: removing one node's incident edges never re-adds another's). Insertion of edges from
`A` and from affected inference adds exactly the pairs whose source-of-truth is in `N(Δ)` (§2.2);
pairs whose source-of-truth is untouched are identical in both computations. Since the final
object is a `Map` keyed by endpoint pairs, the *order* in which removals and insertions are
applied does not change the key set. Hence incremental and full rebuild agree on `keys(gEdges)`. ∎

> **Corollary (why order doesn't matter for correctness):** the equivalence holds even though
> `mergeExtractions`/`mergeGraphs` use `Map.union` (new wins) — see INV-DEDUP and §6 for the
> determinism caveat on *properties*, which must be pinned by config, not left to map iteration.

### LEMMA 1 (Partition validity under churn)
*Restricting `P_t` to surviving communities and assigning each added node to one community yields
a valid partition `P'` of `V'`.*

**Proof sketch.** Removing a node deletes it from exactly one member list (disjointness preserved;
cover holds over `V \ {node}`). Adding a node inserts it into exactly one member list (by
construction of `P'`). Neither operation duplicates a node across lists nor removes a node from
all lists. Hence cover + disjoint hold for `V'`. ∎ (Grounds INV-PARTITION; also why
re-clustering must reassign *every* added node — none may be left unassigned.)

### LEMMA 2 (Modularity delta correctness)
*The change `ΔQ` induced by `Δ` can be computed from the local quantities at churned nodes and
their affected communities, without rescanning all edges.*

**Proof sketch.** `Q` is a sum over edge pairs of local terms `A_ij − (k_ik_j)/2m`
(undirected) or `(k^{out}_i k^{in}_j)/m` (directed). Only terms touching `R ∪ A` change their
`A_ij`, `k`, or `m`. Re-derive `m` (or `W`) from `Δ`-affected edges only; re-derive affected
`k_i` from `N(Δ)`. Sum the changed local terms. This is exactly the one-pass modularity-gain
scheme of `leiden-scalability` ("One-pass modularity-gain scoring"): a single fold over the
neighbour vector yields the `Map CommunityId Int` used for both the decision and the
`sigmaTot` delta. ∎ (Grounds INV-MODULARITY; forbids per-candidate rescans.)

### THEOREM 2 (Termination of incremental update)
*`Update(G, Δ)` terminates in finite time for any finite `G` and finite `Δ`.*

**Proof sketch.** Node/edge map operations terminate (finite `Map` insert/delete). The only
iterative component is re-clustering of `C(Δ)`: (a) Leiden local-moving is bounded by
`resMaxIterations` (`community-detection` Req "Leiden algorithm three phases";
`leiden-scalability` "Constant-time node moves"), and (b) quality is a bounded-below measure that
strictly increases per improving move, so the number of improving moves is finite. Bounded
iterations × O(1)/move ⇒ termination. ∎

### THEOREM 3 (Complexity of incremental vs full rebuild)
*Incremental edge update costs `O(k log k + |N(Δ)|·polylog + E_Δ·polylog)`, versus `O(N + E)` for
a full rebuild.*

**Proof sketch.**
- Removal via reverse adjacency: `O(deg(n)·polylog)` per removed node ⇒ `O(E_Δ · polylog)` total
  (see §5.1 on why scanning all edges is *not* allowed).
- Addition: `O(deg(a)·polylog)` per added node plus affected inference `O(|N(Δ)|·polylog)`
  (bounded fan-out ⇒ inference work is linear in neighbourhood, not quadratic).
- Dedup: `O(k log k)` per `bounded-edge-inference` Req "Log-linear edge deduplication".
- Re-clustering of `C(Δ)`: bounded by Theorem 2; local moving is O(1)/move over the affected
  CSR slices (`leiden-scalability` "CSR adjacency representation").
All `polylog` factors are the `Data.Map.Strict` / `Data.Set` keyed-by-`NodeId` cost. When `k ≪ N`
and `E_Δ ≪ E`, incremental is asymptotically cheaper than the `O(N+E)` rebuild. ∎

---

## 5. Complexity bounds (data-structure grounded)

Model: `Data.Map.Strict` keyed by `NodeId` → `O(log n)` ops; `Data.Set` → `O(log n)` set ops;
CSR adjacency (`leiden-scalability` "CSR adjacency representation") ⇒ neighbour scan is
`O(deg)`. N = `|V|`, E = `|E|`, k = churn cardinality, `E_Δ` = edges incident to churn.

| Operation | Bound | Grounding |
|---|---|---|
| Node removal (incident edges) | `O(deg(n)·log N)` per node | must use reverse adjacency (`gAdjBack`); **not** a full edge scan |
| Node addition | `O(deg(a)·log N)` | forward adjacency insert |
| Adjacency recompute (affected) | `O(|N(Δ)|·log N)` | rebuild only `N(Δ)` entries |
| Inferred-edge update | `O(|N(Δ)| · fanout)` | bounded by `maxCommunityBridges/LabelFanOut/SemanticFanOut` |
| Edge/candidate dedup | `O(k log k)` | `bounded-edge-inference` "Log-linear edge deduplication"; `nubBy` forbidden on graph-scaled lists |
| Re-cluster affected communities | `≤ resMaxIterations × O(1)/move` over CSR | `leiden-scalability` constant-time moves |
| Modularity delta | `O(|N(Δ)|)` one pass | `leiden-scalability` one-pass scoring; no per-candidate rescan |
| Full rebuild (reference) | `O(N + E)` | `buildGraph`/`mergeGraphs` |

### 5.1 Data-structure invariants that make the bounds hold
- **R-REMOVAL-EFFICIENCY:** incremental node deletion MUST locate incident edges via reverse
  adjacency (`gAdjBack`), i.e. `O(deg)`. The current `deleteEdgesTouching` in
  `Graph.Mutation` partitions the *entire* edge map (`Map.partitionWithKey … gEdges`), which is
  `O(E)` — acceptable only for a full rebuild, never for incremental churn. **Requirement:** add a
  `deleteNodeIncident :: NodeId -> Graph -> (Graph, Int)` that reads `gAdjBack`/`gAdjFwd` and
  deletes only incident edges in `O(deg)`.
- **R-JIT-ADJACENCY:** adjacency + `gHash` MUST be rebuilt incrementally for affected nodes
  (`rebuildAdjacency`, `Graph.Mutation`) after each edge mutation rather than after a global
  rebuild, so the `O(|N(Δ)|)` bound holds.

---

## 6. Determinism constraints

The pipeline promises deterministic output (stable `gHash`, stable report, stable query ranking).
Incremental update introduces four decision points that can break determinism; each MUST be
constrained:

1. **Merge precedence on overlapping nodes.** `mergeExtractions`/`mergeGraphs` use `Map.union`
   (new wins). The *winning payload* must be pinned by config/PRD, not left to map iteration.
2. **Dedup order.** First-occurrence-wins requires a *stable, documented* input order for the
   merged edge/candidate stream (`INV-DEDUP`, `bounded-edge-inference` "Log-linear edge
   deduplication").
3. **Inferred-edge tie-breaking.** Community-bridge selection and fan-out truncation
   (`max*FanOut`) MUST break ties deterministically (e.g. by sorted endpoint pair), so the same
   churn yields the same edges regardless of when watch-mode events arrive.
4. **Hash inputs.** `computeGraphHash` sorts node ids and edge pairs; incremental updates must
   feed the same canonical key set so `gHash(G')` is order-independent (`INV-HASH-DET`).

---

## 7. Feasibility finding (must be read before implementation)

**The current incremental pipeline does NOT satisfy `PROP-EQV`.** In
`Pipeline.Incremental.runIncrementalPipeline`:

- Line 65 builds `graph` from `[extraction]` — **only the changed files' extraction** — with no
  merge into the retained global graph (`graph.json`).
- Lines 81–99 cluster/infer/export that **isolated subgraph**; it is never reconciled with the
  previous full graph via `mergeGraphs`, and removed nodes (`R`) are never detached from the
  retained graph via `deleteEdgesTouching`.

Consequence: `Update(G, Δ)` currently returns a *subgraph of changed files*, not
`Update(G_t, Δ) = G_{t+1}`. The requirement "incremental output equals a full rebuild on
`G_{t+1}`" (§3 INV-EQV-EDGE / Theorem 1) is therefore **currently unsatisfied**. This is a
feasibility gap, not a notation problem.

**Required fix (implementation is graphos-dev work):** reconcile old + new via
`mergeGraphs oldGraph newExtractionGraph` (node-new-wins per `Map.union`), then detach edges
incident to `R` using the §5.1 `deleteNodeIncident` helper, then recompute affected adjacency +
affected inferred edges + re-`cluster` only `C(Δ)`, preserving all other communities from
`G_t` (§5.2 re-use rule). The math above constrains *what* must hold; the algebra of the merge
is graphos-dev's to implement.

---

## 8. Acceptance criteria (requirements, mapped to surfaces)

- **REQ-EQV (core).** `runIncrementalPipeline`/`runSingleFilePipeline` output graph `G'` satisfies
  `keys(gEdges G') = keys(gEdges (full rebuild on G_{t+1}))` and `keys(gNodes G') =
  keys(gNodes G_{t+1})`. *(Maps to `02-incremental-pipeline` "Workflow 02" + Theorem 1; test via
  `cabal test` comparing incremental vs full-run `gHash`.)*
- **REQ-PARTITION.** After any churn, the community map is a valid partition of the node set
  (`INV-PARTITION`). *(Maps to `community-detection` "Leiden … three phases"; QuickCheck.)*
- **REQ-REMOVAL-EFFICIENT.** Node removal deletes only incident edges in `O(deg)`
  (`R-REMOVAL-EFFICIENCY`). *(Maps to §5.1; heap/CPU microbenchmark vs `O(E)` scan.)*
- **REQ-INFER-BOUNDED.** Inferred-edge update touches only communities in `C(Δ)` and respects
  named fan-out caps. *(Maps to `bounded-edge-inference` "Community bridges derive from real
  adjacency" + fan-out caps.)*
- **REQ-MODULARITY.** Cohesion/modularity after churn is recomputed over `E'` with the correct
  directed/weighted formula. *(Maps to `community-detection` "cohesionScore" + Lemma 2.)*
- **REQ-DETERMINISTIC.** Same churn ⇒ same `gHash` regardless of event batching order
  (`INV-HASH-DET`, §6). *(Maps to checkpoint-resume Req "checkpoint resume".)*
- **REQ-CHECKPOINT.** Resume from any stage reproduces the uninterrupted `G'`. *(Maps to
  `02-incremental-pipeline` "checkpoint resume from interruption".)*

---

## 9. Scale-guard coherence

All bounds sit inside Graphos's existing scale guards:

- **10K code-node cap** and **log-linear dedup caps** (`bounded-edge-inference` "Log-linear edge
  deduplication") bound per-churn work; incremental churn is sub-linear in the *global* graph
  because only `N(Δ)` / `C(Δ)` are touched.
- **Fan-out caps** (`maxCommunityBridges = 10000`, `maxLabelFanOut = 20`,
  `maxSemanticFanOut = 50`) keep inference `O(|N(Δ)|)` rather than quadratic.
- **Leiden CSR + constant-time moves** (`leiden-scalability`) keep re-clustering of `C(Δ)`
  proportional to affected edges, not the whole graph.

---

## 10. References (prior art)

- M. E. J. Newman, *Modularity detection and community structure in algorithms*, Am J Phys
  (2004); directed modularity `Q_δ`.
- V. A. Traag, L. Waltman, N. van Eck, *From the Louwitz method to the Leiden algorithm*
  (2019) — strictly-improved modularity, O(1)-ish local moves.
- FGL / Haskell `Data.Map.Strict` · `Data.Set` semantics for the `O(log n)` grounding used in §5.
- Graphos specs: `02-incremental-pipeline`, `bounded-edge-inference`, `leiden-scalability`,
  `community-detection`; Domain `Graph.Core`, `Graph.Mutation`, `Graph.Diff`, `Types.Graph`.
