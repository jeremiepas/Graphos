# Merge / Consolidation as a Colimit — Mathematical Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-511](/AVI/issues/AVI-511) (child of [AVI-508](/AVI/issues/AVI-508), child (b))
**Status:** requirements document — grounded in the domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (shared glossary)

One notation throughout, sourced from the Graphos domain types. This is the same glossary used by the sibling graph-theory doc [AVI-510](/AVI/issues/AVI-510) (`docs/math-requirements/community-detection-modularity.md`); the two documents share it so claims about the merged graph are identical across both.

- `NodeId = Text` (`Node.hs:46`). `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`).
- `Edge = Edge { edgeId :: EdgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `edgeId = EdgeId (source <> "->" <> target <> ":" <> relationToText relation)` (`lsp-edge-extraction` §Requirement "Infrastructure.LSP.Extraction"). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:33`). `EdgeId = EdgeId Text` (`Edge.hs:21`). `Confidence = Confidence Double` (`Edge.hs:77`).
- **Extraction (a view).** `Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }` (`Types/Graph.hs:47`), accessors `extNodes`, `extEdges` (`Types/Graph.hs:66-69`). This is the per-file LSP *view*: a finite node map and a finite edge map. `emptyExtraction` (`Types/Graph.hs:54`) is the initial view.
- `LabeledGraph = LabeledGraph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId) }` (`Types/Graph.hs:83`).
- `Graph = Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }` (`Graph/Core.hs:47`). `gDirected :: Bool`.
- `CommunityMap = Map CommunityId [NodeId]`, `CohesionMap = Map CommunityId Double` (`Types/Graph.hs:43-45`).
- `neighbors g nid`, `degree g nid` (`Graph/Query.hs:37,44`): forward-only if `gDirected g`, else fwd ∪ bwd; returns a `Set NodeId`.
- Weighted context graph `G = (V, E, src, tgt, rel, w)` with `V = Map.keys (gNodes g)`, `E = Map.keys (gEdges g)`, `src/tgt : E → V`, `rel : E → Relation`, `w : E → ℝ≥0` (`edgeWeight`); simple **support graph** `G_s = (V, A)` with `{u,v} ∈ A ⟺ v ∈ neighbors(u) ∨ u ∈ neighbors(v)` (sibling doc §1.1).

**Merge primitives (the surfaces this doc constrains):**

| Name | Signature | Semantics (core) | Source |
|---|---|---|---|
| `buildGraph` | `Bool -> Extraction -> Graph` | drops dangling edges; builds adjacency; takes `directed` flag | `Core.hs:96` |
| `mergeExtractions` | `Extraction -> Extraction -> Extraction` | `Map.union` on nodes and edges — **second wins** on key conflict | `Core.hs:129` |
| `mergeGraphs` | `Graph -> Graph -> Graph` | `gNodes old <> gNodes new` (new/second wins), edges unioned then filtered to drop dangling, adjacency unioned, `gDirected old` preserved, hash recomputed | `Core.hs:140` |
| `mergeGraphsAndAnalyze` | `Graph -> Graph -> ... -> MergeResult` | `mergeGraphs → clusterGraphWithResolution → inferEdges → analyze`; source community IDs discarded and re-detected | `Merge.hs:37` |
| `GraphDiff` | `{diffAddedNodes, diffRemovedNodes, diffAddedEdges, diffRemovedEdges}` | per-file add/remove delta for incremental rebuild | `Types/Graph.hs:127` |

> **Constrains (required by the issue).** Every requirement below names one of these surfaces. Behavior-constraining requirements (marked 🔧) require a loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing the Haskell semantics.

---

## 1. Diagram of Extraction Views — the category 𝔻

### 1.1 Objects and morphisms of 𝔻

**Definition (category 𝔻 of views).**
- **Objects** of 𝔻 are `Extraction` values `V` (finite maps of nodes and edges), i.e. per-file LSP views.
- A **view-morphism** `(φ_N, φ_E) : V → W` consists of two **injective** functions
  `φ_N : Nodes(V) ↪ Nodes(W)` and `φ_E : Edges(V) ↪ Edges(W)` such that:
  - **edge identity preserved:** `edgeSource_W ∘ φ_E = φ_N ∘ edgeSource_V` and `edgeTarget_W ∘ φ_E = φ_N ∘ edgeTarget_V` (as maps on the edge domains),
  - **relation preserved:** `edgeRelation_W ∘ φ_E = edgeRelation_V` (φ_E never changes a relation),
  - **value preserved (embedding):** the `Node` at `φ_N(k)` in `W` equals the `Node` at `k` in `V`, and the `Edge` at `φ_E(e)` in `W` equals `e`.
  
  Morphisms that satisfy only the incidence + relation clauses (not value preservation) are called **view-homomorphisms**; they form a broader category 𝔻ₕ used to describe `mergeGraphs` legs when sources conflict (§3.3). 𝔻 is the subcategory of value-preserving **embeddings**.
- **Composition** is coordinate-wise: `(ψ_N, ψ_E) ∘ (φ_N, φ_E) = (ψ_N ∘ φ_N, ψ_E ∘ φ_E)` — composition of injective value-preserving maps is again injective and value-preserving. **Identity** on `V` is `(id, id)`. Axioms hold trivially, so 𝔻 is a category, and every morphism is a **monomorphism** (an embedding).

> **Constrains:** `Extraction` (`Types/Graph.hs:47`), edge accessors `edgeSource/edgeTarget/edgeRelation` (`Edge.hs:88`), `extNodes/extEdges` (`Types/Graph.hs:66-69`).

### 1.2 Shape of 𝔻 for (i) n-way merge and (ii) incremental add/remove

**(i) n-way merge.** Let `{V_i}`_{i∈I} be the per-file views.
- If the files are distinct, their node/edge keys are disjoint (`NodeId` carries a file prefix; `EdgeId` = `source→target:relation`), so the indexing diagram `I` is **discrete** and the colimit is the **coproduct** `⊔_i F(V_i)` (disjoint union of node maps and edge maps). This is the clean case: no key is shared, so nothing is identified.
- If some views overlap consistently (e.g. an incremental rebuild re-emits a superset of a prior view), `I` is a **poset under ⊑** where `V ⊑ W` iff there is a unique embedding `V ↪ W`; the colimit is the **union identifying each shared key once** — the sheaf-style local-to-global colimit over the cover `{V_i}`.

**(ii) Incremental add/remove via `GraphDiff`.** A `GraphDiff` delta is a *signed* view. Model it as a pair of embeddings into the running graph: an addition `(Δ⁺_N, Δ⁺_E)` (injective into the new graph, value-preserving on added keys) and a removal `(Δ⁻_N, Δ⁻_E)` (the reverse embedding deleting keys present in the previous snapshot). Categorically, an incremental step is a **pushout of a cospan**
```
   removed  ──▶  running        (delete the removed keys)
     ▟            ▟
    ▟              ▶  running'
   ▟               (add the added keys)
  added  ──▶  running
```
i.e. `running' = colim( added → running ← removed )` with the deletion leg a value-preserving restriction. On the concrete surface this is `UseCase.Build.buildGraph` applied after applying the delta, matching `02-incremental-pipeline` "merge old + new extractions via `buildGraph`" and the unchanged-file reuse scenario.

> **Constrains:** `GraphDiff` (`Types/Graph.hs:127`), `buildGraph` (`Core.hs:96`), `02-incremental-pipeline` §Requirement "Workflow 02 — incremental pipeline"; unchanged-file reuse = identical cached `Extraction` (scenario "Incremental skips unchanged files").

---

## 2. Functor F : 𝔻 → Graph

### 2.1 Definition

Fix a directed flag `d ∈ {True, False}`. Define `F_d : 𝔻 → Graph`:
- on objects: `F_d(V) = buildGraph d V` (a full `Graph`; `mergeGraphs` reads `gDirected` and the adjacency lists, so mapping into the full `Graph` — not just `LabeledGraph` — is required),
- on a morphism `(φ_N, φ_E) : V → W`: the graph morphism
  `F_d(φ) = (φ_N on gNodes, φ_E on gEdges, φ_N-on-adjacency, directed flag preserved)`,
  where adjacency is carried by `φ_N`: if `w ∈ gAdjFwd(F_d V)(v)` then `φ_N(w) ∈ gAdjFwd(F_d W)(φ_N(v))` (because `φ_E` carries that edge into `W`).

### 2.2 Functor axiom proof sketch

- **Identity.** `F_d(id_V) = F_d((id,id))`. Since `buildGraph` is defined pointwise from the node/edge maps and adjacency, the identity embedding induces the identity on `gNodes`, `gEdges`, `gAdjFwd`, `gAdjBack`, and preserves `gDirected`. Hence `F_d(id_V) = id_{F_d V}`.
- **Composition.** For embeddings `V --(φ)--> W --(ψ)--> X`, `F_d(ψ ∘ φ) = F_d(ψ) ∘ F_d(φ)`. This reduces to three equalities of composed functions on nodes/edges/adjacency, all of which hold because composition of embeddings is coordinate-wise and `buildGraph` builds adjacency solely from the (incidence-, relation-, value-preserving) node/edge maps. **Dangling-edge stability** closes the argument: an edge `e` dangling in `V` (its target `t ∉ Nodes(V)`) maps under `φ_E` to an edge whose target `φ_N(t)` is not represented in `W` (embeddings are value/key preserving and `t` is absent from `V`), so it is dangling in `W` too; thus `buildGraph`'s dangling filter commutes with every embedding, and `F_d` respects restriction. ∎

> **Constrains:** `buildGraph` (`Core.hs:96`), `Graph` (`Graph/Core.hs:47`), `neighbors` (`Query.hs:37`). The functoriality requirement is a *structure* requirement (F preserves identity + composition), checked by property tests on `buildGraph` re-indexed by arbitrary node permutations.

---

## 3. Merge as Colimit / Pushout / Universal Property

### 3.1 Merge = colimit of F (general)

**Claim.** For a consistent diagram `D = F_d ∘ 𝑫 : I → Graph` (all view-morphisms are embeddings, i.e. sources agree on every overlap),
```
mergeExtractions-fold  ≅  colim(F_d ∘ 𝑫),
mergeGraphs            ≅  colim(F_d ∘ 𝑫)        (lifted through buildGraph)
```
i.e. `mergeGraphs old new` is the colimit of the two-object consistent diagram, and the n-way fold `foldr mergeExtractions` is the colimit of the n-object consistent diagram.

**Universal property (stated).** Let `(P, {λ_V : D(V) → P})` be any cocone under `D`. Then there exists a **unique** morphism `u : colim D → P` with `u ∘ ι_V = λ_V` for every leg `ι_V : D(V) → colim D` of the colimit cocone. **Uniqueness up to unique isomorphism:** any two colimits of `D` are related by a unique isomorphism commuting with the cocone legs.

**Concrete realization.** `colim D` has node set `⋃_i Nodes(V_i)` (each shared key once) and edge set `⋃_i Edges(V_i)` (each shared key once); `Map.union` realizes both because it merges entries by key. The colimit legs `ι_V` are the canonical inclusions. This is the standard fact that *every colimit is a coequalizer of a coproduct* (Riehl, *Categorical Topology*, Ch. 2; Borceux–Brumberg, *Category Theory* vol. II): the union is the coequalizer of the two maps out of the overlap coproduct that send a shared object to its two inclusions.

**What the universal property fixes.** It fixes the colimit object **up to unique isomorphism** and fixes the **unique mediating morphism** to any cocone. It does **not** fix node *attribute values on a conflict* — that is the resolution policy (§3.3). For **consistent** inputs it additionally pins **order-independence**: `mergeGraphs` is associative and commutative up to canonical iso, so the merge result is independent of merge order (sheaf local-to-global consistency).

> **Constrains:** `mergeExtractions` (`Core.hs:129`), `mergeGraphs` (`Core.hs:140`), `buildGraph` (`Core.hs:96`).

### 3.2 2-way merge = pushout

**Claim.** For two views `A, B` with a **consistent** overlap `A ∩ B` (shared keys with identical values), `mergeGraphs A B` is the **pushout** of the span
```
   A ∩ B  ─▶  A
     ▟          ▟
    ▟           ▶  A ⊔_(A∩B) B  =  mergeGraphs A B
    ▟            (new wins on conflict)
   B  ─▶  B
```
The pushout square commutes (`ι_A ∘ inA = ι_B ∘ inB` on the shared keys) and is universal: any co-square (pairs of morphisms `A → Q`, `B → Q` agreeing on `A ∩ B`) factors uniquely through it. Pushout is the colimit over the span category `• ← • → •` (standard; Lemmermeyer, *graph pushouts*; Freyjá–Lack–Morrison, cospan gluing). When `A ∩ B = ∅` the pushout degenerates to the coproduct (disjoint-file merge).

### 3.3 ⚠️ Conflict case — "new wins" is a chosen cocone, not a universal property

**Honesty clause (required).** When `A` and `B` share a key `k` with **different** `Node` values, no value-preserving embedding `A ↪ B` or `B ↪ A` exists, so the pair is **not an object of 𝔻** and §3.1/§3.2 do not apply. `mergeGraphs` still returns a graph via `gNodes old <> gNodes new` (`Core.hs:142`) — `Data.Map.union` takes the **right/new** operand's value on conflict. This is a **resolution policy**: it selects a specific cocone apex (the "new wins" object) rather than a universal one.

**Consequence.** With conflicts, the merge result **depends on merge order** (the last merge partner's value wins at each conflicting key). This order-dependence is a **policy choice**, not a violation of the universal property (which pins results only up to canonical iso for consistent diagrams). The mediating-morphism universal property still holds *from* the chosen apex to any other cocone whose legs agree with "new wins" on the overlap.

> **Constrains:** `mergeGraphs` (`Core.hs:140-142`, `gNodes old <> gNodes new`), `mergeExtractions` (`Core.hs:131-132`). 🔧 Requires a loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm the second-wins semantics is intended for every conflicting key (node *and* edge), since `Map.union` second-wins on both.

---

## 4. Dedup as Uniqueness (coequalizer-like)

### 4.1 Dedup-by-NodeId / unique-EdgeId = coequalizer of the overlap coproduct

The colimit is built from a coproduct plus a coequalizer. Let `{V_i}` be the views and `R` the coproduct of the **overlaps** — one copy per pair of views sharing a key. The two maps `p, q : R ⇉ ⊔_i F(V_i)` send a shared object to its two inclusions. The **coequalizer** of `(p,q)` quotients the disjoint union by "same NodeId" / "same EdgeId", yielding exactly the deduplicated node/edge set. Concretely, `Map.union` realizes this coequalizer because it merges entries **by key**: `|keys colim| = |⋃ keys|`, each key present once.

### 4.2 Uniqueness clause = the dedup invariant

The **unique mediating morphism** `u : colim → P` in §3.1 is precisely the statement that the colimit carries **no extraneous duplicate copies**: any cocone that retained a duplicate receives a unique map that collapses it back onto the single key. Hence:

> **INV-MERGE (dedup well-definedness).** The merged graph has **exactly one** entry per `NodeId` and per `EdgeId`; the cocone legs are jointly surjective onto the merged node/edge sets and identify only the keys forced to be identified. Equivalently, the mediating morphism to any cocone is unique ⟺ dedup is well-defined.

> **Constrains:** `mergeExtractions` (`Core.hs:129-132`), `mergeGraphs` (`Core.hs:142-144`, dangling filter), `EdgeId` uniqueness (`lsp-edge-extraction` §Requirement "Unique EdgeId for every edge").

---

## 5. Functoriality of the Pipeline (preserved vs discarded)

The pipeline is `extract → merge → cluster → infer → analyze` (`Merge.hs:37`). Model it as functors between the relevant categories and ask which squares commute (naturality) and which are deliberately non-commuting.

**Preserved under merge (a genuine natural transformation).** The structural content — node identity, edge `(source, target, relation)`, `edgeWeight`, `edgeConfidence`, and `gDirected` (from the first graph) — is preserved by `mergeGraphs`:
- node identity & edge `(src,tgt,relation)` & weights/confidence: kept whole by `Map.union` (`Core.hs:142-144`);
- `gDirected`: explicitly `gDirected old` (`Core.hs:157`).

This gives a **natural transformation** η between "merge then read structure" and "read structure per view then merge": for the structural projection functor `Struct : Graph → (NodeMap, EdgeMap, Bool)`, `η` is an isomorphism and the square
```
   ⊔_i F(V_i)  ── coproduct ──▶  colim (=merged G)
        Struct                        Struct
         ▼                           ▼
   (⋃ nodes, ⋃ edges, gDirected old)  ==  Struct(merged G)
```
**commutes** (isomorphism). This is the local-to-global consistency statement for structure: local truth (per-view structure) and global truth (merged structure) agree on overlaps.

> **Constrains:** `mergeGraphs` (`Core.hs:140-162`), `buildGraph` (`Core.hs:96`).

**Discarded under merge (deliberate non-naturality).** Community membership is **not** preserved: `mergeGraphsAndAnalyze` discards source community IDs and re-detects (`Merge.hs:32-34`, comment "community IDs from sources no longer align, so we re-detect communities"). So `detectCommunities` does **not** commute with merge — the square
```
   merged G  ── detectCommunities ──▶  CommunityMap(merge(A,B))
      │                                  ▲  (no natural map)
   mergeGraphs                          cluster(merge(A,B))
      ▼                                  │
   G'    ── ? ───────────────────────────┘   ≠  merge(cluster A, cluster B)
```
has **no commuting witness**. This is a required *inequality*: the design mandates re-detection, so the requirement encodes the failure of naturality (a designated direction, not an iso).

> **Constrains:** `mergeGraphsAndAnalyze` (`Merge.hs:37-64`, lines 32-34 comment), `detectCommunitiesWithResolution` (`Community.hs:73`), `CommunityMap` (`Types/Graph.hs:43`). 🔧 Requires a loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm that no consumer relies on merged source community IDs before finalizing the "discarded" requirement.

---

## 6. ⚠️ Arbitration Flag — Local-to-Global Consistency

### 6.1 Community detection does not commute with merge (required)

**Question.** Is `cluster(merge(A, B)) == merge(cluster A, cluster B)`?
**Answer (code): NO.** `mergeGraphsAndAnalyze` re-detects communities on the merged graph; source IDs are discarded (`Merge.hs:32-34`). State as requirement **R-LG1**: community detection is **not** a functorial (natural) operation w.r.t. merge; the pipeline requires `detectCommunitiesWithResolution (mergeGraphs A B) res`, and the per-view-communities-merged square of §5 has no commuting witness.

> **Constrains:** `mergeGraphsAndAnalyze` (`Merge.hs:37-64`).

### 6.2 Reconcile cohesion with the sibling doc [AVI-510](/AVI/issues/AVI-510)

The sibling doc settles cohesion as the **average internal-neighbor ratio** (§6.1 of `community-detection-modularity.md`):
```
cohesion(c) = (1 / |c|) · Σ_{v ∈ c} |{ u ∈ c : u ∈ neighbors(g,v), u ≠ v }| / max(1, |neighbors(g,v)|)
```
with `neighbors` per `gDirected g` (`Query.hs:37`). This must be **consistent across the merge functor**:

> **R-LG2 (cohesion consistency under merge).** For a community `c` of the merged graph, cohesion computed on `merge(A,B)` equals the cohesion computed from the union of the constituent views' adjacency, because `mergeGraphs` preserves the node set and unions `gAdjFwd`/`gAdjBack` (`Core.hs:145-146`). Hence `neighbors` on the merged graph restricted to `c` is the union of `neighbors` in the views, so `cohesion ∈ [0,1]` (INV-5 of the sibling doc) and representative-quality thresholds (`< 0.3`, sibling doc §6.1) remain well-defined after merge.

> **Constrains:** `mergeGraphs` (`Core.hs:145-146`, adjacency union), `cohesionScore` (`Community.hs:385-403`), `neighbors` (`Query.hs:37`). The sibling doc's §6.3 already escalates this jointly with this doc; this requirement closes the loop by pinning that the merge functor preserves exactly the adjacency the cohesion formula reads.

### 6.3 Joint reconciliation with Graph Theory Expert (sibling (a))

The merged graph is simultaneously (i) a weighted multigraph `G = (V,E,src,tgt,rel,w)` on which Leiden runs and (ii) the colimit of extraction views (this doc). The two claims share notation (§0). The graph-theory acceptance bar (combinatorial validation of nodes/edges) applies to the colimit object: e.g., `|V| = |⋃ keys|` and `|E| = |⋃ keys|` after the dangling drop must match the sibling's degree/modularity computations on `G_s`. This doc supplies the *universal-property* claim; the sibling supplies the *combinatorial* check.

---

## 7. Acceptance Criteria (all four required by the issue)

### AC-1 — Universal-property property test (uniqueness of the mediating morphism)
For a small consistent diagram (two overlapping views sharing one node key and one edge key), construct `mergeGraphs A B` via `Map`-union merge and assert: for any cocone `(Q, λ_A, λ_B)` with `λ_A, λ_B` agreeing on the shared key, there is a **unique** `u : mergeGraphs A B → Q` with `u ∘ ι_A = λ_A` and `u ∘ ι_B = λ_B`. Concretely, `u` is forced on every node/edge key (`u(k) = λ_A(node_k)` on `A`-keys, `= λ_B(node_k)` on `B`-only keys), so `u` is uniquely determined and well-defined — no choice remains. Property-test over random small diagrams.

### AC-2 — Invariant assertion preserved under merge
After `mergeGraphs A B`: (a) every edge's `edgeRelation`, `edgeWeight`, `edgeConfidence` equal the source value carried by `Map.union` (relation label preserved); (b) node identity preserved (`Map.keys` of result = union of keys); (c) dedup yields a well-defined graph (exactly one entry per `NodeId`/`EdgeId`, INV-MERGE). Assert on fixtures with a shared conflicting node: relation + weight + confidence of the surviving edge are unchanged; key count is the union size, not the sum.

### AC-3 — Complexity / cost assertion
The colimit is realized by `Data.Map.union` at **O(n₁ + n₂)** per binary merge (`mergeExtractions` comment `Core.hs:124-128`), i.e. the categorical colimit adds **no asymptotic cost** over the concrete Map union. Consequently the n-way colimit must be a **single left-fold** `foldl' mergeExtractions` costing **O(Σᵢ |Vᵢ|)** total — linear in input — **not** a nested/right-fold, which the `Core.hs:127` comment flags as OOM-inducing over 1000+ files. Requirement: the n-way colimit cost = Σ of per-view sizes; verify no quadratic blowup in a benchmark over many views.

### AC-4 — Haskell-constraint statement per requirement (loop with Graphos Dev)
Every requirement in §1–§6 names its constrained surface (table in §0) and carries the acceptance test above. 🔧-marked requirements (§3.3 second-wins on both node and edge keys; §5 discarded-source-communities) must be **looped with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing any behavior-constraining requirement** — this doc constrains behavior only after that confirmation. Until then, the 🔧 requirements are stated as *proposed* semantics pending developer confirmation.

---

## 8. Requirements Traceability Table

| Req | Statement | Surface | Acceptance | Locked? |
|---|---|---|---|---|
| R1 | 𝔻 is a category; morphisms = injective value-preserving view-embeddings (§1.1) | `Extraction`, edge accessors | axiom check (identity+composition) | structural |
| R2 | n-way colimit = coproduct (disjoint) or union-over-cover (§1.2) | `mergeExtractions`, `GraphDiff` | AC-1 | structural |
| R3 | `mergeGraphs` = colimit of F (§3.1) | `mergeGraphs`, `buildGraph` | AC-1, AC-2 | structural |
| R4 | 2-way merge = pushout over consistent overlap (§3.2) | `mergeGraphs` | AC-1 | structural |
| R5 | "new wins" = chosen cocone, order-dependent on conflict (§3.3) | `Core.hs:142` | AC-2 | 🔧 loop w/ Dev |
| R6 | Dedup = coequalizer; unique mediating morphism (§4) | `mergeExtractions` | AC-2 | structural |
| R7 | Structural naturality (nodes/edges/weight/directed preserved) (§5) | `mergeGraphs` | AC-2 | structural |
| R8 | Communities discarded, re-detected; non-natural (§5, §6.1) | `mergeGraphsAndAnalyze` | behavior | 🔧 loop w/ Dev |
| R9 | Cohesion consistent across merge (§6.2) | `cohesionScore`, adjacency union | AC-2 | structural |
| R10 | n-way colimit cost = O(Σ\|Vᵢ\|), single fold (§7 AC-3) | `mergeExtractions` | AC-3 | structural |

---

## 9. Prior Art

- **Colimit = coequalizer of a coproduct;** pushout = colimit over the span category — standard categorical facts (Riehl, *Categorical Topology and Logic*, Ch. 2; Borceux & Brumberg, *Category Theory*, vol. II).
- **Graph gluing via pushout / cospan semantics** (Lemmermeyer, *Graph Categories* / graph pushouts; Freyjá, Lack & Morrison, "Cospan Semantics for Relational Systems") — the reference for "merge as pushout."
- **Sheaf-style local-to-global as a colimit over a cover** (Mac Lane & Moerdahl, *Sheaves in Geometry and Logic*; the global sections of a presheaf over a cover are its colimit, and the sheaf condition is the descent/colimit equality) — the reference for "merged graph = colimit of local views; local and global truth agree on overlaps."
- **`Data.Map.union` second-wins** — library semantics, not invented here (`containers`); the "new wins" policy (§3.3) is a Graphos choice layered on top.

---

## 10. Determinism Lens

- **Consistent diagrams:** `mergeGraphs`/`mergeExtractions` are associative and commutative up to canonical iso, so the merge result is **order-independent** (sheaf local-to-global consistency, AC-1).
- **Conflicting diagrams:** `Map.union` second-wins makes the result **order-dependent** on conflicting keys (§3.3) — a documented policy choice, not a universal-property violation. Requirement: the order-dependence is explicit and tested (AC-2 asserts the surviving value equals the *last* merge partner's value at each conflict).
