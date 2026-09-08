# Graphos Math Requirements — Hot-Reload Cost Model & Cache Invalidation

**Agent:** graph-theory-expert · **Issue:** [AVI-521](/AVI/issues/AVI-521) · **Parent:** [AVI-512](/AVI/issues/AVI-512)
**Status:** requirements complete · **Surface map:** see §0

> Deliverable type: math requirements (definitions → invariants → theorems with proof sketches → complexity → acceptance criteria). No Haskell is written here; implementation is `[Graphos Developer](/AVI/agents/graphos-dev)` / Head of R&D work (see §7). This doc is **distinct from** [AVI-519](/AVI/issues/AVI-519) (incremental edge updates under node churn): AVI-519 constrains a *single churn's* equivalence/partition/modularity-delta; this doc constrains the **content-addressed cache invalidation** (§1.2, §3 INV-CACHE-*), the **multi-cycle watch-loop cost model** (§4–5), and the **confluence/convergence** of the reload sequence (§6). It builds on AVI-519 rather than repeating it.

---

## 0. Scope & Surface Map

This doc constrains the graph-theoretic behaviour invoked by watch mode
(`Graphos.Infrastructure.FileSystem.Watcher.watchDirectory`, `03-watch-mode`) and the
incremental pipeline (`Graphos.UseCase.Pipeline.Incremental.runIncrementalPipeline`,
`02-incremental-pipeline`). Every requirement names the spec header and the Domain
module/type it constrains; nothing here is ungrounded.

| Surface | Spec header | Domain type / function |
|---|---|---|
| Content-addressed file cache | `02-incremental-pipeline` Req "Workflow 02 — incremental pipeline" (load SHA256 hashes, compare current SHA256 vs cached) | `loadCached`, `saveCached`, `fileHash`, `checkSemanticCache` in `Infrastructure.FileSystem.Cache` |
| Extraction merge | `02-incremental-pipeline` Req "Workflow 02" | `mergeExtractions`, `buildGraph` (`Domain.Graph.Core`) |
| Graph merge (fold batch into prior) | `09-merge` Req "Workflow 09"; `02-incremental-pipeline" | `mergeGraphs :: Graph -> Graph -> Graph` (`Core.hs`) |
| Node/edge churn | [AVI-519](/AVI/issues/AVI-519) §1.2 (churn `Δ = (R,A)` / `GraphDiff`) | `graphDiff`, `GraphDiff(..)` (`Domain.Graph.Diff`) |
| Communities (partition) | `community-detection` | `detectCommunities`, `cohesionScore`, `CommunityMap = Map CommunityId [NodeId]` |
| Leiden complexity vocab | `leiden-scalability` "Constant-time node moves", "CSR adjacency representation", "One-pass modularity-gain scoring" | `LeidenState`, `clusterGraphWithResolution` |
| Bounded inference | `bounded-edge-inference` "Community bridges derive from real adjacency", "Log-linear edge deduplication" | `inferCommunityBridges`, fan-out caps `maxCommunityBridges / maxLabelFanOut / maxSemanticFanOut` |
| Scale guards | `leiden-scalability` "O(N) community member grouping"; 10K code-node cap | `leidenStateToCommunityMap` |

---

## 1. Definitions

### 1.1 Files and content
Let `F` be the set of candidate source files. A **content map** is a partial function
`σ : F ⇢ Bytes` assigning each present file its byte contents. Extraction is a deterministic
function `ext : F ⇢ Extraction` defined on `Source`-classified files (`Types.Pipeline.isSourceClass`).
One file extracts a bounded number of nodes; let `β = max_f |nodes(ext(f))|`.

### 1.2 Content-addressed cache
The extraction cache is a keyed store `Cache : Slot → Extraction`. The **cache key** is a pure
function `K : F → Slot`. The cache is **content-addressed** iff `K` factors through content:
there is `h : Bytes → Slot` with `K(f) = h(σ(f))`.

Define the **hit-correctness property** `H`:
```
∀ f . loadCached f = Just e  ⇒  extract(f) = e            (H)
```
i.e. a cache hit returns an extraction that reproduces the current extraction of `f`.

### 1.3 Hot-reload cycle
A **reload batch** `B ⊆ F` is the set of files whose content differs from their state at the
start of a debounce window. A **watch cycle** transforms `B` into a new global graph:
```
Cycle(B) = Export ∘ Analyze ∘ Infer ∘ Cluster ∘ Merge · ( Ext(B), G_prev )
```
where `Ext(B)` re-extracts only changed files (skipping unchanged via `H`), `G_prev` is the
retained global graph, and `Merge` folds the batch's fresh graph into `G_prev`
(`mergeGraphs`). The *intended* `02-incremental-pipeline` behaviour loads `graph.json`, splits
changed vs. unchanged, and **merges old + new extractions** before Cluster→Infer→Analyze→Export.

### 1.4 Affected region (the cost object)
For a churn `Δ = (R, A)` induced by batch `B` (`|Δ_nodes| ≤ β·|B|`), define the **affected
region** `Aff(Δ) ⊆ V` as the set of nodes whose downstream quantity may change:
```
Aff(Δ) = { v ∈ V : c'(v) ≠ c(v) }   ∪   { v : cohesion'(v) ≠ cohesion(v) }
         ∪   { v : analysis'(v) ≠ analysis(v) }  ∪   { v : v is endpoint of a changed/inferred edge }
```
`c, c'` are the pre-/post-reload community assignments. Trivially `Δ_nodes ⊆ Aff(Δ)`. Let
`E_Aff = { e ∈ E : endpoints(e) ∈ Aff(Δ) }`.

### 1.5 Ripple bound
Let `φ` bound one hop of influence propagation through the graph: `φ = Δ_max · d`, where
`Δ_max` is the maximum node degree and `d` is the Leiden iteration depth (`≤ resMaxIterations`).
The **ripple** is `|Aff(Δ)| ≤ |Δ_nodes| · (1 + φ)` — the churn set expanded by ≤ `φ` per hop.
(This composes with [AVI-519](/AVI/issues/AVI-519) §2.3 `C(Δ)` / bounded-delta: AVI-519 bounds the
number of changed *community assignments*; this doc bounds the *set of affected nodes* whose
recompute the cost model charges.)

---

## 2. The cache is an object whose correctness is itself the requirement

### 2.1 Hit-correctness
`H` (§1.2) is the property the cache exists to guarantee. Equivalently, contrapositive: if
`σ(f)` changed since the entry was written, `loadCached f` MUST return `Nothing` (a miss that
triggers re-extraction).

### 2.2 Necessary conditions on `K`
For `H` to hold with collision probability `ε`:
- **Determinism:** `σ(f₁) = σ(f₂) ⇒ K(f₁) = K(f₂)`.
- **Collision-soundness:** `K(f₁) = K(f₂) ⇒ σ(f₁) = σ(f₂)` with probability `≥ 1 − ε`
  (for SHA-256, `ε ≤ 2^-256` by the birthday bound).

**Theorem 3.1** below proves `H ⟺ collision-soundness`. The spec (`02-incremental-pipeline`,
"load SHA256 hashes … compare current SHA256 vs cached") fixes `h = SHA-256`, which satisfies
both conditions.

### 2.3 The current key is content-blind (critical finding — see §7)
`Cache.fileHash` (`Cache.hs:120`) returns
```
show (length rel) ++ "_" ++ map safeChar rel        -- rel = makeRelative root path
```
This is a function of the file's **relative path** only — never of `σ(f)`. It therefore
violates collision-soundness *catastrophically*: (a) any two files with equal path-length and
equal sanitized path collide to one slot (a certain, not merely probable, collision); and
(b) editing a file's content while its path length is unchanged yields the **same key**, so
`loadCached` returns the stale entry — the "skip unchanged files" optimization re-extracts
nothing on content edits. The cache is sound only when the *path itself* changes.

---

## 3. Invariants (checkable; each is a QuickCheck-ready property)

Let `Δ = (R, A)`, `G' = Cycle(B)` applied to `G_prev`.

- **INV-CACHE-SOUND** `H` holds: `forAll f . loadCached f == Just e ⇒ extract f == e`.
  *(Property: with a content-addressed cache, mutating `σ(f)` to different bytes forces a miss;
  identical content hits the same slot. Negative test: two files of equal content share a slot,
  two files of differing content (differing beyond ε) never do.)* — **constrains `fileHash` /
  `loadCached` (`Cache.hs`).** *This is the central correctness claim of this doc.*
- **INV-CACHE-DET** `K` is deterministic and collision-sound: `σ(f₁)=σ(f₂) ⇒ K(f₁)=K(f₂)` and
  collision probability `≤ ε`. *(Property: QuickCheck over random byte strings that equal
  content ⇒ equal key; SHA-256 length ≥ 256 bits.)* — constrains `fileHash`.
- **INV-DEBOUNCE-NO-LOSS** after coalescing a debounce window, the file contents equal the
  last-edit contents for every file (no lossy merge), and the re-extracted set equals exactly
  `{ f : σ_now(f) ≠ σ_preWindow(f) }`. *(Property: replay a window of edits; assert final
  content per file equals its final edit and the re-extracted set matches the changed set.)* —
  constrains `watchDirectory` event queue.
- **INV-AFFECTED-CONTAIN** `Δ_nodes ⊆ Aff(Δ)` for the diff. *(Property: for random `G`, random
  `Δ`, every churned node appears in the recomputed set.)* — constrains the reload operator.
- **INV-RIPPLE-BOUND** changing a batch of `k` files changes ≤ `f(k)` community assignments,
  where `f` is the AVI-519 bounded-delta, and `|Aff(Δ)| ≤ |Δ_nodes|·(1+φ)`. *(Property: on
  synthetic graphs, perturb `k` files; assert changed-assignment count and affected-region size
  stay within the bound.)* — composes with AVI-519; constrains `Cluster`/`Infer` scope.
- **INV-CONFLUENCE** two edit sequences ending in identical content map `σ*` yield isomorphic
  `(graph, communityMap)` after reload as after a single full build. *(Property: build `G*`
  incrementally over a shuffled edit order and over a single full run; assert
  `gHash`/isomorphism of `(V,E,c)` agree.)* — constrains `Merge` + stage determinism.
- **INV-COST-BOUND** an incremental reload on post-state `G*` costs no more wall-clock than a
  full rebuild producing the same `G*`, and no less than the work on `Aff(Δ)`. *(Property:
  non-regression microbenchmark; incremental timing ⊆ [full·(Aff-fraction), full].)* —
  constrains the reload operator vs. full pipeline.
- **INV-TERMINATION** the Leiden loop inside `Cycle` exits when `moved == 0` and refinement
  fixes nothing (`leiden-scalability`), hard-capped at `resMaxIterations`; the watch event
  queue makes forward progress (no infinite inner loop, no starvation). *(Property:
  `countMoves` stabilises; iteration count ≤ resMaxIterations.)* — constrains `leidenLoop`.

---

## 4. Theorems with proof sketches

### THEOREM 1 (Cache soundness ⟺ collision-soundness of the key)
*`H` (§1.2) holds with collision probability `ε` iff `K` is collision-sound up to `ε`.*

**Proof sketch.**
(⇐) If `K` is collision-sound, then for file `f` the slot `K(σ(f))` holds an entry written by
the last writer whose key equals `K(σ(f))`. With probability `≥ 1−ε` that writer had identical
content, and since `ext` is a deterministic function of content + path, the stored extraction
equals `extract(f)`. Hence a hit returns `e = extract(f)`, i.e. `H`.
(⇒) Suppose `K` is *not* collision-sound: there exist `f₁ ≠ f₂` with `σ(f₁) ≠ σ(f₂)` and
`K(σ(f₁)) = K(σ(f₂))`. Whichever was written last occupies the shared slot; loading the other
returns the wrong extraction, violating `H`. Thus `H ⇒ collision-soundness`. ∎

> **Corollary (why the current code fails):** `fileHash` depends only on the path, so `K` is
> constant w.r.t. content — every content edit preserves the key, and collisions are certain
> rather than bounded. By Theorem 1, `H` is violated for every content edit. This is not a
> performance issue; it is a soundness bug.

### THEOREM 2 (Bounded ripple / affected-region bound)
*`Δ_nodes ⊆ Aff(Δ)` and `|Aff(Δ)| ≤ |Δ_nodes|·(1 + φ)` with `φ = Δ_max·d`.*

**Proof sketch.** A change to `Δ` alters the Leiden modularity-gain deltas `ΔQ` only for nodes
whose decision reads changed data. By `leiden-scalability`, a node's move reads only its CSR
neighbour slice `[offset[i], offset[i]+deg(i))` and neighbour communities (one-pass scoring,
O(1)/move); hence only endpoints of changed edges and their immediate neighbours have altered
`ΔQ`. Refinement splits a community only when a member's intra-community cohesion `< 0.5`
(`Domain.Community.refineCommunitiesOpt`), affecting only that community's members; merging
small communities propagates to the merge-neighbours (`bestNeighborCommunity` reads the reverse
index of the small community's members' neighbours). Over `d ≤ resMaxIterations` iterations the
influence expands by at most `Δ_max` neighbours per hop, giving `|Aff| ≤ |Δ_nodes|·(1+φ)`.
Combined with AVI-519's bounded-delta (`|c' ≠ c| ≤ f(|Δ_edges ∩ Aff|)`), the number of changed
assignments is likewise bounded. ∎

### THEOREM 3 (Confluence / convergence under reload)
*The reload sequence converges — i.e. for any edit sequence ending in content map `σ*`, the
incremental result equals a single full build on `σ*` up to isomorphism preserving `(V,E,c)` —
iff (a) each stage is a deterministic function of its input (tie-breaks fixed), and (b) the
fold `Merge` into the running graph is a **commutative idempotent semi-lattice** on the
node/edge layer:*
```
Merge(a, a) ≅ a                        (idempotent — re-processing is safe)
Merge(a, b) ≅ Merge(b, a)              (commutative — for non-conflicting views)
Merge(Merge(a,b), c) ≅ Merge(a, Merge(b,c))   (associative — batching order independent)
```

**Proof sketch.** Model reloads as a rewrite system whose rules are the per-batch folds.
Confluence of a terminating rewrite system follows from local confluence + Newman's lemma.
Deterministic stages give local confluence provided each fold is a commutative idempotent
operator (a semi-lattice): then any interleaving/reduction of folds collapses to a canonical
order, so the final `(V,E)` is independent of batching/order. Idempotency absorbs re-processing
of unchanged files; commutativity absorbs reorderings; associativity absorbs different debounce
batchings. On conflicting `NodeId` labels the merge is *not* commutative (last-write-wins breaks
symmetry) — this matches the CTO architecture note that `merge ∘ cluster` is non-commutative;
confluence is asserted only for the consistent-view layer, exactly as [AVI-512](/AVI/issues/AVI-512) rev 2
requires. If `Merge` is **not** applied (the batch is built in isolation and never folded into
`G_prev`), the rewrite system does not accumulate; the running graph drifts toward the batch, and
`Full(σ*)` is unreachable except after a fresh full run — convergence fails. ∎

### THEOREM 4 (Cost monotonicity)
*For a batch `B` with affected region `Aff(Δ)`, if the reload merges the batch into `G_prev` and
recomputes only `Aff(Δ)`, then `Cost_inc(B) ≤ Cost_full(G*)`, with the saved work exactly the
stages applied to `V \ Aff` and `E \ Aff`; the worst case (`|Aff| = Θ(N)`, e.g. editing a
god-node file) degrades to the full-rebuild bound `O(resMaxIterations · (N + E))`.*

**Proof sketch.** The incremental cycle does strictly less work than a full rebuild: it skips
extraction of unchanged files (cached, O(1) slot lookup), skips Build/Cluster/Infer/Analyze of
`V \ Aff` and `E \ Aff`. The saved work is a non-negative measure equal to those stages on the
complement; the remaining work is monotone in `|Aff|`. Hence `Cost_inc ≤ Cost_full`, with
equality gap `Θ(|V \ Aff| + |E \ Aff|)`. When `|Aff| = O(|B|·φ) ≪ N`, incremental is
asymptotically cheaper; when a batch touches a high-degree file, `|Aff| → Θ(N)` and the cycle
correctly falls back to full-rebuild cost. ∎

### THEOREM 5 (Termination of a watch cycle)
*Each `Cycle(B)` terminates in finite time for finite `G_prev` and finite `B`.*

**Proof sketch.** `Ext(B)` processes a finite file set; `Merge`/`Build` are finite `Map`
operations; `Cluster` terminates by AVI-519 Theorem 2 (bounded iterations × O(1)/move over CSR,
quality a bounded-below measure); `Infer`/`Analyze`/`Export` are single passes over `Aff(Δ)`
(bounded fan-out ⇒ finite). The watch loop itself is an interactive unbounded event loop and is
not required to terminate, but each iteration terminates and the queue makes forward progress
(INV-TERMINATION). ∎

---

## 5. Complexity bounds (data-structure grounded)

Model: `Data.Map.Strict` keyed by `NodeId` → `O(log N)`; `Data.Set` → `O(log N)`; CSR adjacency
(`leiden-scalability`) ⇒ neighbour scan is `O(deg)`. `N = |V|`, `E = |E|`, `B` = batch size,
`Aff = Aff(Δ)`, `E_Aff = edges incident to Aff`.

| Stage | Full rebuild | Hot-reload (batch `B`, region `Aff`) | Grounding |
|---|---|---|---|
| Extract | `O(Σ_f size_f)` all files | `O(Σ_{f∈B} size_f)` — unchanged files: O(1) slot lookup + entry decode (`loadCached`) | `Cache.hs`; `02-incremental-pipeline` |
| Cache write | — | `O(size_f)` per changed file | `saveCached` |
| Build / Merge | `O(N + E)` | `O(|Δ_nodes| + |E ∩ Aff|)` build + `O(|Aff|)` merge (`mergeGraphs`) | `Core.hs` |
| Cluster | `O(resMaxIterations · (N + E))` | `O(resMaxIterations · (|Aff| + |E_Aff|))` — recompute only `Aff` | `leiden-scalability` O(1)/move, CSR |
| Infer | `O(E_inter + caps)` | `O(|E_Aff ∩ inter-community| + caps)` — bounded by `bounded-edge-inference` caps | `maxCommunityBridges / FanOut` |
| Analyze | `O(N + E)` | `O(|Aff| + |E_Aff|)` | `Domain.Graph.Analysis` |
| Export | `O(N + E)` | `O(|Aff| + exported-subgraph)` | export port |

**Worst case:** editing a god-node file gives `|Aff| = Θ(N)`, degrading to the full-rebuild
bound `O(resMaxIterations · (N + E))` — the correct, safe behaviour (Theorem 4).

### 5.1 Data-structure invariants that make the bounds hold
- **C-CONTENT-KEY:** the cache key MUST be a cryptographic hash of file *contents* (SHA-256),
  so `loadCached` is O(1) slot lookup + O(entry-size) decode and invalidates on any content
  change. A path-only or length-only key is forbidden (`INV-CACHE-SOUND`).
- **C-MERGE-INFLATION:** `Merge` must not rebuild the whole graph; it must union node/edge maps
  (`mergeExtractions`/`mergeGraphs` use `Map.union`, `O(n₁ + n₂)`) — never a list-based
  `O(n₁·n₂)` fold (`Core.hs:129` note).
- **C-RECOMPUTE-SCOPE:** Cluster/Infer/Analyze MUST recompute only `Aff(Δ)`, scanning
  `E_Aff`/CSR slices, never the global adjacency (`leiden-scalability` one-pass scoring).

---

## 6. Determinism / confluence constraints

The pipeline promises deterministic output (stable `gHash`, stable report, stable query
ranking). The watch loop adds decision points that can break confluence (§4 THEOREM 3); each
MUST be constrained:

1. **Merge precedence on overlapping nodes.** `mergeGraphs` uses new-wins (`Map.union`). The
   winning payload must be pinned by config/PRD, not left to map iteration — else two batches
   in different orders yield different retained nodes.
2. **Debounce coalescing must be lossless.** Events within a window are coalesced into one
   batch (`03-watch-mode` "debounce prevents rapid re-triggering"), but the final per-file
   content must equal the last edit (`INV-DEBOUNCE-NO-LOSS`).
3. **Inferred-edge tie-breaking.** Bridge selection and `max*FanOut` truncation MUST break ties
   deterministically (e.g. by sorted endpoint pair) so the same content yields the same edges
   regardless of when watch events arrive (`INV-CONFLUENCE`).
4. **Hash inputs.** `computeGraphHash` sorts node ids and edge pairs (`Core.hs:259`); the
   incremental fold must feed the same canonical key set so `gHash(G*)` is order-independent
   (AVI-519 `INV-HASH-DET`).

---

## 7. Feasibility findings (must be read before implementation)

**F-1 (CRITICAL) — cache invalidation is content-blind.** `Cache.fileHash` (`Cache.hs:120`)
keys on path-length + sanitized path only, never on file content. Per Theorem 1 the
hit-correctness property `H` is violated for every content edit: hot-reload re-extracts nothing
when a file's content changes (unless the edit also changes its path length). **Fix:** replace
`fileHash` with SHA-256 over contents (the spec already mandates SHA256; add `cryptohash-sha256`
per the existing TODO at `Cache.hs:122`). *Grounding: `Cache.hs`, `02-incremental-pipeline`.*

**F-2 — reload does not converge to the full graph.** `runIncrementalPipeline`
(`Incremental.hs:65`) builds the graph from `[extraction]` — **only the changed files** — with
no `mergeGraphs` fold into the retained `graph.json`, and never detaches removed nodes (`R`)
from the retained graph. Per Theorem 3 the reload sequence does **not** converge to
`Full(σ*)`; repeated edits fragment the graph into the last batch's subgraph. This is a
convergence gap (not a notation problem) that must be resolved before the confluence
requirement (`INV-CONFLUENCE`) can hold. *Grounding: `Incremental.hs`, `Core.mergeGraphs`,
[AVI-519](/AVI/issues/AVI-519) §7.* **Decision needed from Graphos-Dev:** is watch-mode intended
to maintain the global graph (fold batch into prior) or to re-analyse only the changed subgraph?

**F-3 — auditable modularity `Q` missing during reload.** `computeCommunityStats`
(`Domain.Community`) computes the exact `σ_in/σ_tot/m` quantities `Q` needs but is dead code
([AVI-533](/AVI/issues/AVI-533) / G2). For hot-reload "did the partition improve?" auditing, wire
`modularity :: G → CommunityMap → Double` off those existing quantities (reuse, do not
recompute). *Separate concern from F-1/F-2; track alongside G2.*

**F-4 — determinism under incremental must match full-build.** The tie-breaks required by §6
must equal AVI-519's determinism pins (`INV-HASH-DET`, `INV-DEDUP`); verify no divergence
between the reload path and a single full run on identical content.

---

## 8. Acceptance criteria (requirements, mapped to surfaces)

- **REQ-CACHE-SOUND (core).** `loadCached`/`saveCached` implement a content-addressed cache so
  `H` (`INV-CACHE-SOUND`) holds; a content change invalidates the entry. *(Maps to
  `02-incremental-pipeline` "compare current SHA256 vs cached" + Theorem 1; test via
  `cabal test`: mutate file content → assert miss → re-extract returns updated extraction.)*
- **REQ-DEBOUNCE-LOSSLESS.** Coalescing a debounce window never alters final per-file content
  and re-extracts exactly the changed set. *(Maps to `03-watch-mode` "debounce prevents rapid
  re-triggering" + `INV-DEBOUNCE-NO-LOSS`.)*
- **REQ-AFFECTED-SCOPE.** Reload recomputes only `Aff(Δ)`; changed-assignment count respects the
  AVI-519 bounded-delta; `|Aff(Δ)| ≤ |Δ_nodes|·(1+φ)`. *(Maps to `leiden-scalability` +
  AVI-519 bounded-delta + Theorem 2; bounded-delta test on synthetic graphs.)*
- **REQ-CONFLUENCE.** Two edit sequences ending in identical `σ*` yield isomorphic
  `(graph, communityMap)` as a single full build. *(Maps to `02-incremental-pipeline` +
  Theorem 3; compare shuffled-order incremental build vs. full-build `gHash`.)*
- **REQ-COST-BOUND.** Incremental reload wall-time ≤ full rebuild on the same post-state, and ≥
  the work on `Aff(Δ)`. *(Maps to §5 table + Theorem 4; non-regression microbenchmark.)*
- **REQ-TERMINATION.** Each watch cycle terminates; Leiden iterations ≤ `resMaxIterations`.
  *(Maps to `leiden-scalability` + Theorem 5.)*

---

## 9. Scale-guard coherence

All bounds sit inside Graphos's existing scale guards:

- **10K code-node cap** and **log-linear dedup caps** (`bounded-edge-inference` "Log-linear edge
  deduplication") bound per-batch work; hot-reload is sub-linear in the *global* graph because
  only `Aff(Δ)` is recomputed.
- **Fan-out caps** (`maxCommunityBridges = 10000`, `maxLabelFanOut = 20`,
  `maxSemanticFanOut = 50`) keep infer work `O(|E_Aff|)` rather than quadratic.
- **Leiden CSR + constant-time moves** (`leiden-scalability`) keep re-clustering of `Aff(Δ)`
  proportional to affected edges, not the whole graph.
- **Content-addressed cache** (`C-CONTENT-KEY`) makes extract-skipping O(1) per unchanged file;
  the only per-batch write cost is one entry per changed file (`O(size_f)`).

---

## 10. References (prior art)

- M. E. J. Newman, *Modularity detection and community structure* (2004); directed modularity.
- V. A. Traag, L. Waltman, N. van Eck, *From the Louwitz method to the Leiden algorithm* (2019)
  — strictly-improved modularity, O(1)-ish local moves, CSR-friendly.
- [AVI-519](/AVI/issues/AVI-519) — bounded-delta for community assignments under churn (the
  foundation this doc composes with).
- FGL / `Data.Map.Strict` · `Data.Set` semantics for the `O(log n)` grounding in §5.
- Graphos specs: `02-incremental-pipeline`, `03-watch-mode`, `09-merge`, `checkpoint-controls`,
  `leiden-scalability`, `bounded-edge-inference`; Domain `Graph.Core`, `Graph.Diff`,
  `Infrastructure.FileSystem.Cache`, `UseCase.Pipeline.Incremental`.
