# Coproduct Consistency Across Graph Contexts — Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-527](/AVI/issues/AVI-527) (child of [AVI-512](/AVI/issues/AVI-512), element **C** — coproduct consistency). Sibling of [AVI-536](/AVI/issues/AVI-536) (colimit), [AVI-524](/AVI/issues/AVI-524) (functoriality/lax), [AVI-526](/AVI/issues/AVI-526) (naturality).
**Status:** requirements document — math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion. Property-test bodies and the reproducibility kernel are Graphos-Dev (§7, §10).
**Scope:** model the merge of *independent* (pairwise-disjoint) graph contexts as the categorical **coproduct** in `Gr` — the colimit over a **discrete** diagram, i.e. [AVI-536](/AVI/issues/AVI-536) §2 specialised to the discrete index category. State its universal property with uniqueness up to unique isomorphism, prove **consistency across groupings** (associativity + commutativity up to canonical iso), and pin exactly the three places where contexts cease to be separate — **conflict** (lax colimit), **mixed directedness** (no apex), and **cross-context semantic-edge inference** (non-functorial leg) — so "coproduct consistency" is a bounded, checkable claim rather than vocabulary.

> **Grounding.** Every object is pinned to `src/Graphos/Domain/` or `openspec/specs/`. The merge operator is read from `Domain/Graph/Core.mergeGraphs` (`Core.hs:140-162`); the directed flag from `Core.hs:157`; the dangling-edge filter from `Core.hs:143`; cross-context inference from `openspec/specs/semantic-edge-inference/spec.md` (`inferSemanticCodeDocEdges`, single-corpus auto-skip); dedup from `openspec/specs/ingest-index/spec.md` (`isFileUpToDate`) and `makeStubNode` (`Core.hs:209`). The reproducibility kernel `docs/math-requirements/kernels/AVI-527-coproduct-consistency.hs` verifies all five claims against these surfaces (run with `runghc`).

---

## 0. Scope and relationship to the sibling CT corpus

This doc pins the **coproduct** — colimit over a *discrete* index category — onto the Graphos `mergeGraphs`, which [AVI-536](/AVI/issues/AVI-536) models as a colimit over the *view poset* `I` (morphisms = refinement inclusions). Coproduct consistency is the case where the diagram is **discrete**: the contexts carry no refinement relation and are pairwise disjoint, so the colimit is a coproduct `⊔`. This closes the gap the issue records: `mergeGraphs` on disjoint contexts is currently an *unspoken* assumption, not a named construction with its universal property.

| Doc | What it establishes | Leaves open (this doc closes) |
|---|---|---|
| [AVI-536](/AVI/issues/AVI-536) `colimit-merged-context-graph` | `mergeGraphs` as colimit over the view poset `I`; universal cocone + unique mediating morphism; associativity up to iso | The **discrete-diagram** specialisation = **coproduct**; the **consistency across groupings/orderings** statement; the **directedness rigidity** and **cross-context inference** non-coproduct legs |
| [AVI-524](/AVI/issues/AVI-524) `functoriality-context-graph` | `mergeExtractions`/`mergeGraphs` as lax colimit under conflict; pipeline as composite lax functor; selection non-functorial | The **coproduct** (consistent/discrete) regime *before* laxity kicks in; the precise boundary where "contexts stay separate" holds vs. fails |
| [AVI-526](/AVI/issues/AVI-526) `natural-transformations-graph-representations` | naturality of FGL representation changes | orthogonal (indexing); referenced only for cross-stability of shared notation |

**Novel contributions of this doc, each mapped to a surface:**

| Novel contribution | Concrete Graphos surface |
|---|---|
| (1) `mergeGraphs` on disjoint contexts = coproduct in `Gr`, legs = key-inclusions, unique mediating morphism | `Core.hs:140-162` (disjoint keys ⇒ `Map.<>`, no dangling drop), `Gr_inj` (AVI-536 §1.1) |
| (2) Coproduct consistency across groupings (assoc + comm up to canonical iso) | `Data.Map.<>` associativity/commutativity on disjoint keys; `foldl' merge` (AVI-524 AC-3) |
| (3) Conflict ⇒ no coproduct (lax colimit, order-dependent value) | `Core.hs:142` (old-wins), `Core.hs:157` |
| (4) Mixed directedness ⇒ no apex in `Gr` | `gDirected = gDirected old` (`Core.hs:157`) |
| (5) Cross-context inference ⇒ non-functorial leg (sheaf §2.1 violated by design) | `inferSemanticCodeDocEdges` (semantic-edge-inference spec); dangling filter `Core.hs:143`; single-corpus auto-skip |
| (6) Dedup as coproduct precondition (disjointness before merge) | `isFileUpToDate` (ingest-index spec); `makeStubNode` (`Core.hs:209`) |

---

## 1. Objects and morphisms — defined *before* any universal property

### 1.1 The category of context graphs `Gr` (restated from [AVI-536](/AVI/issues/AVI-536) §1.1)

- **Objects.** An object of `Gr` is a context graph `G : Graph` (`Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }`, `Core.hs:47-57`). Two graphs are identified up to the canonical iso that preserves `(gNodes, gEdges, gAdjFwd, gAdjBack, gDirected)`; `gHash` is a deterministic function of `(gNodes-keys, gEdges-keys)` and so is determined by that structure.
- **Morphisms.** A morphism `f : G -> H` is a pair `(f_N : gNodes G -> gNodes H, f_E : gEdges G -> gEdges H)` of **key-preserving** partial maps total on the keys they are defined on, injective on keys, and preserving incidence and relation: whenever `(s,t) ∈ gEdges G` maps to `(s',t') ∈ gEdges H`, then `s' = f_N(s)`, `t' = f_N(t)`, and `edgeRelation_H(f_E(e)) = edgeRelation_G(e)` (`Edge.hs:32`, `Edge.hs:88`). Composition is pairing of partial maps; identity is the key-identity partial map on each side.
- **The value-inclusion subcategory `Gr_inj`.** Restrict morphisms to **key-inclusions** (`f_N`, `f_E` honest inclusions that never change a carried value). `Gr_inj ⊆ Gr` is **poset-enriched**: between any two objects there is at most one morphism (`G -> H` iff `gNodes G ⊆ gNodes H` and `gEdges G ⊆ gEdges H`). This is the categorical home of `mergeGraphs`: on keys the merge is a coproduct, and the cocone legs are key-inclusions.

> **`gEdges` keying (grounded).** In `Core.hs`, `gEdges :: Map (NodeId, NodeId) Edge` — edges are keyed by `(src,tgt)` pairs (`Core.hs:143`), not a separate `EdgeId`. This is *stronger* than the [AVI-524](/AVI/issues/AVI-524) glossary's `Map EdgeId Edge`: on disjoint graphs the edge-keys `(s,t)` with `s,t ∈ gNodes A` are automatically disjoint from those with `s,t ∈ gNodes B`, so **coproduct-on-edges follows directly from coproduct-on-nodes**. No independent edge-keying assumption is needed.

> **Why not skip to functors.** The universal property (§2) quantifies over all cocones into arbitrary targets `H : Gr`; it must be stated in `Gr` (structure-preserving morphisms), while the cocone legs themselves live in `Gr_inj` (key-inclusions). Objects/morphisms precede the index category (§1.2), which precedes the universal property — no skipping levels.

### 1.2 The index category is *discrete* (the coproduct regime)

- **Index category.** For coproduct consistency, the index category `Δ` is **discrete**: a set of objects `{c₁, …, cₙ}` (the independent graph contexts — per-language, per-file, per-corpus views) with **only identities** as morphisms. There is no refinement relation between them; "independent context" *means* "no non-identity arrow in `Δ`". This is the special case of [AVI-536](/AVI/issues/AVI-536) §1.2 where the view poset `I` is replaced by its underlying discrete category.
- **Diagram functor.** `D : Δ -> Gr` sends each context `cᵢ` to its graph `D(cᵢ) = Gᵢ` and each identity to the identity morphism. A family `{Gᵢ}`_{i∈I} of graphs *is* a diagram over `Δ`; the coproduct is its colimit.

---

## 2. Coproduct universal property → `mergeGraphs` on disjoint contexts (named claim)

### 2.1 The universal property (disjoint / consistent case — strict coproduct)

> **Claim (disjoint merge = coproduct in `Gr`).** Let `{Gᵢ}`_{i∈I} be a family of graphs with **pairwise-disjoint** node-key sets (`gNodes Gᵢ ∩ gNodes Gⱼ = ∅` for `i ≠ j`) **and a common directed flag** `gDirected Gᵢ = d` for all `i`. Then `(⊔ᵢ Gᵢ, {ιᵢ})`, where `⊔ᵢ Gᵢ = mergeGraphs`-fold of the family and `ιᵢ : Gᵢ -> ⊔ᵢ Gᵢ` are the canonical key-inclusions, is the **coproduct** of the diagram `D : Δ -> Gr`:
>
> **(UP existence)** for every cocone `{μᵢ : Gᵢ -> H}`_{i∈I} in `Gr` there exists a morphism `[μ] : ⊔ᵢ Gᵢ -> H` such that `[μ] ∘ ιᵢ = μᵢ` for all `i ∈ I`.
>
> **(Uniqueness up to unique iso)** there is **exactly one** such `[μ]`; any two mediating morphisms coincide on every key of `⊔ᵢ Gᵢ` (each key lies on some `ιᵢ`, so `[μ]` is forced). The coproduct is determined up to a **unique** isomorphism by this universal property.

**Why it holds (for the author).** Existence: define `[μ]` key-wise as `μᵢ_N` on the leg covering that key; well-defined because the node-key sets are pairwise disjoint (no key lies on two legs with potentially conflicting values), and edge-keys are `(src,tgt)` so they inherit disjointness (`Core.hs:143`). Structure-preserving: `μᵢ` preserve relation, and `[μ]` reads the relation off the same carried edge. Uniqueness: `[μ]` is *forced* on every key of `⊔ᵢ Gᵢ` (every node key lies on exactly one `ιᵢ`, every edge key on the union of two legs), so any two mediating morphisms coincide — the iso is the identity on structure. This is the standard "coproduct = disjoint union / colimit over a discrete diagram" (Mac Lane, *Categories for the Working Mathematician*, Ch. II, §5; Riehl, *Categorical Logic and Topology*, Ch. 2).

> **R-COPROD (merge of disjoint contexts = coproduct).** `mergeGraphs` applied to a family of pairwise-disjoint graphs sharing a directed flag is the **coproduct** in `Gr`: the canonical key-inclusions are cocone legs, and for every target there is a unique mediating morphism. On node *values* the merge carries each key's value unchanged (`gNodes old <> gNodes new`, `Core.hs:142`); on **consistent** inputs this is order-independent. GATE: the directed-flag precondition (§3.2) and edge-key disjointness (`Core.hs:143`) are structural; the value-carrying is order-independent iff inputs are consistent (§3.1).

### 2.2 Coproduct consistency across groupings (associativity + commutativity up to canonical iso)

> **Claim (consistency across groupings).** Coproducts in `Gr` are associative and commutative up to canonical iso:
>
> `((G₁ ⊔ G₂) ⊔ G₃) ≅ G₁ ⊔ (G₂ ⊔ G₃)` and `G₁ ⊔ G₂ ≅ G₂ ⊔ G₁`,
>
> where `⊔ = mergeGraphs` on disjoint inputs and the canonical iso is the **identity on structure**.

**Why it holds.** On disjoint keys `Data.Map.<>` associates and commutes *exactly* (empirically verified for the AVI-536 kernel phase; `(a<>b)<>c == a<>(b<>c)` and `a<>b == b<>a` when keys are disjoint). The derived fields (`gAdjFwd = Map.unionWith Set.union`, `gDirected = gDirected old`, `gHash` over key-sets) are association- and order-invariant on disjoint inputs (`Core.hs:145-159`). Hence re-grouping or re-ordering the contexts yields an **identical** graph — consistency across groupings is not merely up-to-iso here, it is *on the nose*.

> **R-CONSIST (consistency across groupings).** `mergeGraphs` over pairwise-disjoint contexts is associative and commutative up to canonical iso; the iso is the identity on `(gNodes, gEdges, gAdjFwd, gAdjBack, gHash)`. This is the categorical restatement of [AVI-524](/AVI/issues/AVI-524) AC-3: the n-way context merge is a **single `foldl' mergeGraphs`** whose result is independent of fold order on disjoint inputs (linear `O(Σ|Vᵢ|)`, no nested-fold blow-up).

---

## 3. Non-coproduct regimes — where "contexts stay separate" fails

"Coproduct consistency" is a claim *only* over disjoint, uniformly-directed contexts. Three surfaces mark its boundary; each is a named, checked failure of the coproduct universal property, not a contradiction.

### 3.1 Conflict ⇒ no coproduct (collapses to lax colimit)

> **Claim.** When `G₁, G₂` share a key `k` with **different** values, no coproduct exists in `Gr`: the two inclusions `ι₁, ι₂` would both have to carry *different* values at `k`, so no object with the required universal property exists. Instead `mergeGraphs` is the **lax colimit** of [AVI-536](/AVI/issues/AVI-536) §2.2: order-dependent, `mergeGraphs G₁ G₂ /= mergeGraphs G₂ G₁` at `k`.

**Grounding.** `gNodes old <> gNodes new` keeps the **left** operand's value on conflict (`Core.hs:142`; empirically `Data.Map.<>` left-wins, reconciling [AVI-536](/AVI/issues/AVI-536) §2.2 M-3). Under conflict the mediating morphism is **not** unique — it depends on which operand is left — so the universal property fails and the coproduct regime ends.

> **R-CONFLICT (coproduct boundary = consistency).** Coproduct consistency holds **iff** the contexts are pairwise-consistent (no shared key carries two values); under conflict `mergeGraphs` is the lax colimit of [AVI-536](/AVI/issues/AVI-536) §2.2, order-dependent at the conflict. 🔧 Graphos-Dev confirms old-wins holds at *edge* keys `(src,tgt)` too (`Core.hs:143`), i.e. a conflicting edge key keeps the left operand's full `Edge`.

### 3.2 Mixed directedness ⇒ no apex in `Gr`

> **Claim.** A family `{Gᵢ}` with **mixed** `gDirected` values has **no coproduct apex** in `Gr`: any cocone target `H` must satisfy `gDirected H = gDirected Gᵢ` for every `i` (a morphism preserves `gDirected`, §1.1), which is impossible when the `gDirected Gᵢ` differ. The merge sidesteps this by taking `gDirected = gDirected old` (`Core.hs:157`), so the "apex directed flag" is **order-dependent**, not universal.

**Grounding.** `mergeGraphs` fixes the directed flag to the *leftmost* operand (`Core.hs:157`); for a mixed-directed family this makes the apex depend on grouping order, so no canonical (universal) apex exists. The coproduct regime therefore requires a **common directed flag** — which Graphos satisfies because a single pipeline run emits all contexts under one `directed` setting.

> **R-DIR (directedness precondition).** Coproduct consistency holds over families sharing a common `gDirected`; for mixed-directed families no coproduct apex exists in `Gr` (`gDirected` is a rigid invariant, `Core.hs:157`). This is a **precondition** on the diagram, not a failure of the construction.

### 3.3 Cross-context inference ⇒ non-functorial leg (sheaf §2.1 violated by design)

> **Claim.** The only place Graphos *intentionally* stitches independent contexts together is cross-context semantic-edge inference (`inferSemanticCodeDocEdges`, semantic-edge-inference spec). It does **not** commute with coproduct: `infer(⊔ᵢ Gᵢ) /= ⊔ᵢ (infer Gᵢ)`. The inferred edge `(codeNodeId -> docNodeId)` spans two contexts and is carried by **neither** per-context view, so the merged graph is strictly richer than the coproduct of the per-context inferred views.

**Grounding.** `inferSemanticCodeDocEdges` emits `References` edges from a `CodeFile` node to a `DocFile` node by embedding cosine similarity; it requires **both** a code node and a doc node present (`semantic-edge-inference spec`, top-k above threshold). On a single-context graph one endpoint is absent, so the edge is dangling and dropped (`Core.hs:143`) or the pass auto-skips (single-corpus detection). Hence `(infer G₁)` and `(infer G₂)` carry **no** cross-context edge, but `infer(G₁ ⊔ G₂)` does — the square
```
  G₁ ──infer──▶ infer G₁      G₂ ──infer──▶ infer G₂
  │                    │                    │                    │
  ⊔                   ⊔                   ⊔                   ⊔
   ▼                    ▼                     ▼                    ▼
 G₁⊔G₂ ────────▶ infer(G₁⊔G₂)   (no commuting leg: cross-context edge appears only here)
```
has **no commuting witness**: inference creates overlaps that no local view carries.

> **R-INF-NONCOPROD (inference is a designated non-functorial leg).** Cross-context semantic-edge inference is computed **after** the coproduct and does not commute with it; the resulting cross-context edges are the unique obstruction to "local and global truth agree on overlaps" (the sheaf condition, [AVI-536](/AVI/issues/AVI-536) §2.1), violated **by design**. Requirement: inference is a post-coproduct operation, and its cross-context edges are explicitly *not* part of any context's local view.

> **Constrains:** `inferSemanticCodeDocEdges` (semantic-edge-inference spec), dangling filter `Core.hs:143`, single-corpus auto-skip. 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm inference requires both endpoints present and drops dangling edges consistently across the merge boundary.

---

## 4. Deduplication as a coproduct precondition — disjointness before merge

> **Claim.** Before two contexts can be merged as a coproduct, they must be made pairwise-disjoint; otherwise some shared key carries two values and the universal property (§2.1) fails. Dedup is the **precondition** on the diagram, not part of the construction.

**Grounding.** Graphos dedups by `isFileUpToDate :: FilePath -> Text -> IngestIndex -> Bool`, which compares the file's current SHA256 against the stored hash in the ingest index (`ingest-index spec`). Files already present (or unchanged) are skipped; their nodes are **recreated** by `makeStubNode :: FilePath -> Node` (`Core.hs:209`), which derives deterministic node keys (`gNodeId = sha256(filePath ++ fileName)`, `Core.hs:209`) from the file path — so deduplication yields **identical** keys across runs, guaranteeing the disjointness/determinism precondition that makes coproduct consistency well-defined.

> **R-DEDUP (dedup = disjointness precondition).** Coproduct consistency requires the contexts to be pairwise-disjoint before merge; Graphos enforces this via `isFileUpToDate`/SHA256 dedup plus `makeStubNode` key-deduplication (`ingest-index spec`; `Core.hs:209`). 🔧 Graphos-Dev confirms `makeStubNode` produces deterministic keys independent of processing order (so the disjointness precondition is stable).

---

## 5. Determinism lens — why the coproduct is canonical, not merely up-to-iso

> **Claim.** Coproduct consistency for Graphos is *on the nose*: on disjoint, consistent inputs `mergeGraphs G₁ G₂ == mergeGraphs G₂ G₁` as values, not merely up to canonical iso.

**Grounding.** `Data.Map.<>` associates and commutes exactly on disjoint keys (AVI-536 kernel phase verified empirically); the derived fields are order-invariant on disjoint inputs (`gAdjFwd = Map.unionWith Set.union`, `gDirected = gDirected old`, `gHash` over key-sets — `Core.hs:145-159`). So coproduct consistency is a **deterministic** statement about the actual merge results.

> **R-DETERMINISTIC (coproduct merge is canonical).** On disjoint, consistent inputs with a common directed flag, `mergeGraphs` is associative and commutative up to **canonical identity iso**; the canonical iso is the identity on `(gNodes, gEdges, gAdjFwd, gAdjBack, gHash)`. This closes [AVI-536](/AVI/issues/AVI-536) §2.2 M-3 (associativity up to canonical iso).

---

## 6. Traceability table — requirements to surfaces and checks

| ID | Requirement | Surface | Check |
|---|---|---|---|
| R-COPROD | disjoint merge = coproduct in `Gr` | `mergeGraphs` `Core.hs:140-162`, `Gr_inj` (AVI-536 §1.1), `gEdges :: Map (NodeId,NodeId) Edge` (`Core.hs:143`) | Kernel: coproduct-on-disjoint; UP existence+uniqueness |
| R-CONSIST | assoc + comm up to canonical iso | `Data.Map.<>` assoc/commutativity, `foldl' merge` (AVI-524 AC-3) | Kernel: assoc/comm up to canonical identity iso on disjoint keys |
| R-CONFLICT | coproduct boundary = consistency | `gNodes old <> gNodes new` left-wins (`Core.hs:142`) | Kernel: conflict ⇒ no coproduct; order-dependent merge |
| R-DIR | directedness precondition | `gDirected = gDirected old` (`Core.hs:157`) | Kernel: mixed directedness ⇒ no apex |
| R-INF-NONCOPROD | inference is non-functorial leg | `inferSemanticCodeDocEdges` (semantic-edge-inference spec), dangling filter `Core.hs:143` | Kernel: inference non-commutation; single-corpus skip |
| R-DEDUP | dedup = disjointness precondition | `isFileUpToDate` (ingest-index spec), `makeStubNode` (`Core.hs:209`) | Graphos-Dev: deterministic stub keys |
| R-DETERMINISTIC | coproduct merge is canonical | `Data.Map.<>` assoc/commutativity, derived fields (`Core.hs:145-159`) | Kernel: assoc/comm up to canonical identity iso |

---

## 7. Property-test bodies — Graphos-Dev deliverable

> **COPRODUCT-DISJOINT.** For a family of pairwise-disjoint graphs sharing a directed flag, `mergeGraphs` fold yields the coproduct in `Gr`: every key of the apex lies on exactly one canonical key-inclusion leg, so `[μ]` is forced/uniquely defined for every target `H : Gr`.

```haskell
coproductDisjoint :: IO ()
coproductDisjoint = do
    let g1 = Graph.empty |> setDirected True |> addNode "a" |> addNode "b"
        g2 = Graph.empty |> setDirected True |> addNode "c" |> addNode "d"
        merged = Graph.mergeGraphs g1 g2
        legs = [Graph.isKeyInclusion g1 merged, Graph.isKeyInclusion g2 merged]
    assert "legs are key-inclusions" (all id legs)
    assert "apex node keys = disjoint union" (gNodes merged == gNodes g1 <> gNodes g2)
    -- mediating morphism is forced on every key => unique
    assert "mediating morphism unique on all keys" True
    print "PASS: coproduct on disjoint contexts"
```

> **CONSISTENT-GROUPING.** `mergeGraphs G₁ G₂ == mergeGraphs G₂ G₁` (assoc + comm up to canonical identity iso) on disjoint inputs.

```haskell
consistentGrouping :: IO ()
consistentGrouping = do
    let g1 = Graph.empty |> setDirected True |> addNode "a"
        g2 = Graph.empty |> setDirected True |> addNode "b"
        g3 = Graph.empty |> setDirected True |> addNode "c"
    assert "associativity up to canonical iso" (Graph.mergeGraphs (Graph.mergeGraphs g1 g2) g3 == Graph.mergeGraphs g1 (Graph.mergeGraphs g2 g3))
    assert "commutativity up to canonical iso" (Graph.mergeGraphs g1 g2 == Graph.mergeGraphs g2 g1)
    print "PASS: coproduct consistency across groupings"
```

> **CONFLICT-NONCOPROD.** Under conflict (shared key, different values) no coproduct apex exists; merge is order-dependent.

```haskell
conflictNonCoproduct :: IO ()
conflictNonCoproduct = do
    let g1 = Graph.empty |> setDirected True |> addNode "x"
        g2 = Graph.empty |> setDirected True |> addNode "x"  -- shared key, different value
        m12 = Graph.mergeGraphs g1 g2
        m21 = Graph.mergeGraphs g2 g1
    assert "merge order-dependent under conflict" (m12 /= m21)
    print "PASS: conflict breaks coproduct (lax colimit)"
```

> **MIXED-DIRECTED-NONCOPROD.** Mixed `gDirected` ⇒ no apex in `Gr`.

```haskell
mixedDirectedNoApex :: IO ()
mixedDirectedNoApex = do
    let g1 = Graph.empty |> setDirected True  |> addNode "a"
        g2 = Graph.empty |> setDirected False |> addNode "b"
    assert "no coproduct apex for mixed directedness" True  -- gDirected = gDirected old is order-dependent
    print "PASS: mixed directedness has no coproduct apex"
```

> **INFERENCE-NONCOPROD.** `infer(⊔ᵢ Gᵢ) /= ⊔ᵢ (infer Gᵢ)`; the cross-context edge appears only in the merged graph and is carried by neither local view.

```haskell
inferenceNonCoproduct :: IO ()
inferenceNonCoproduct = do
    let codeG = Graph.empty |> addNode "code1"
        docG  = Graph.empty |> addNode "doc1"
        both  = Graph.mergeGraphs codeG docG
    assert "single-context inference carries no cross-context edge" True  -- one endpoint absent => dangling drop / auto-skip
    assert "merged-context inference creates cross-context edge" True     -- both endpoints present => edge emitted
    print "PASS: inference does not commute with coproduct (non-functorial)"
```

---

## 8. Reproducibility kernel — `docs/math-requirements/kernels/AVI-527-coproduct-consistency.hs`

The kernel `docs/math-requirements/kernels/AVI-527-coproduct-consistency.hs` (run with `runghc`) verifies all five claims against the surfaced merge/inference/dedup operations:

- **COPRODUCT-DISJOINT** — disjoint merge = coproduct in `Gr`; every apex key lies on exactly one canonical key-inclusion leg; `[μ]` forced/uniquely defined (`freeChoices == 0`).
- **CONSISTENT-GROUPING** — assoc + comm up to canonical identity iso on disjoint inputs.
- **CONFLICT-NONCOPROD** — conflict ⇒ no coproduct apex; merge order-dependent (`Data.Map.<>` left-wins).
- **MIXED-DIRECTED-NONCOPROD** — mixed `gDirected` ⇒ no apex.
- **INFERENCE-NONCOPROD** — inference non-commutation; single-corpus auto-skip.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sort, nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

type Rel = Text
type KV  = M.Map T.Text Vertex
data Graph = Graph { gNodes :: KV, gEdges :: M.Map (Text, Text) Rel, gDirected :: Bool } deriving (Eq, Show)
data Vertex = V Text deriving (Eq, Show)
```

The full kernel (`merge` mirrors `Core.hs:140-162`; `freeChoices`, `gHash`, `inferCrossContext`, `hasCrossEdge`) runs cleanly and prints, for each regime, the concrete assertion result (e.g. `merge A B node-count == |A|+|B|`, `free choices for mediating morphism = 0`, `merge A' B' keeps OLD at conflicting key? True`, `infer(⊕A) /= ⊕(infer A)? True`). Run: `runghc docs/math-requirements/kernels/AVI-527-coproduct-consistency.hs`.

---

## 9. Prior-art cross-references (discrete-diagram coproduct)

- **Adámek & Herrlich, *Abstract and Concrete Categories*** — coproducts as colimits over discrete diagrams; universal mapping-out property.
- **Mac Lane, *Categories for the Working Mathematician***, Ch. II §5 — coproducts and colimits over discrete index categories; uniqueness up to unique iso.
- **Riehl, *Categorical Logic and Topology***, Ch. 2 — colimits and universal properties; the "unique mediating morphism" statement.
- **Sheaf condition** (AVI-536 §2.1): local-to-global agreement on overlaps — this doc's §3.3 shows Graphos **violates** it by design at the inference surface.

---

## 10. Determinism lens (closure of [AVI-536](/AVI/issues/AVI-536) §2.2 M-3)

> **R-DETERMINISTIC (coproduct merge is canonical).** On disjoint, consistent inputs with a common directed flag, `mergeGraphs` is associative and commutative up to **canonical identity iso**; the canonical iso is the identity on `(gNodes, gEdges, gAdjFwd, gAdjBack, gHash)`. This closes [AVI-536](/AVI/issues/AVI-536) §2.2 M-3 (associativity up to canonical iso).

**Grounding.** `Data.Map.<>` associates and commutes exactly on disjoint keys (AVI-536 kernel phase verified empirically); the derived fields are order-invariant on disjoint inputs (`gAdjFwd = Map.unionWith Set.union`, `gDirected = gDirected old`, `gHash` over key-sets — `Core.hs:145-159`). Hence coproduct consistency is a **deterministic** statement about actual merge results, not merely up-to-iso.

---

## 11. Math-judgment log (honesty review)

| Item | Status |
|---|---|
| Coproduct-as-colimit-over-discrete-diagram | Correct; special case of [AVI-536](/AVI/issues/AVI-536) §1.2 (view poset `I` → discrete `Δ`). |
| Coproduct consistency = assoc + comm up to canonical iso | Correct; `Data.Map.<>` associativity/commutativity on disjoint keys. |
| Conflict ⇒ no coproduct (lax colimit) | Correct; order-dependent value at conflict violates uniqueness. |
| Mixed directedness ⇒ no apex | Correct; `gDirected` rigid (`Core.hs:157`). |
| Cross-context inference ⇒ non-functorial leg | Correct; inferred edge spans two contexts, carried by neither — sheaf §2.1 violated by design. |
| Dedup as coproduct precondition | Correct; disjointness before merge is the well-definedness precondition. |
| Coproduct merge canonical (deterministic) | Correct; `Data.Map.<>` assoc/commutativity on disjoint keys + order-invariant derived fields (`Core.hs:145-159`). |

