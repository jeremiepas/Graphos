# Query-Ergonomic Slim Store as a Projection — Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-566](/AVI/issues/AVI-566) (child of [AVI-563](/AVI/issues/AVI-563), rev `5ee70789`).
**Status:** requirements document — math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.
**Scope:** model the **slim store** as a projection `P` of the full context graph, where the hot query path reads `P(G)` rather than the fat `Graph`. Three requirements — slimming as projection (CT-Q1), colimit preservation (CT-Q2), no-loss contract (CT-Q3) — plus the joint determinism clause JOINT-Q with the Graph Theory Expert. Closes the root cause recorded in [AVI-563](/AVI/issues/AVI-563): the graph stores full source text per node, causing output explosion, the 4.5 GB reload, and the wrong top hit.

> **Grounding.** Every object is pinned to `src/Graphos/Domain/` or `openspec/specs/`. The merge operator is read from `Domain/Graph/Core.mergeGraphs` (`Core.hs:140`) / `mergeExtractions` (`Core.hs:128`); the node field set from `Domain/Types/Node.Node` (`Node.hs:120`). Notation is drawn verbatim from the shared glossary of [AVI-536](/AVI/issues/AVI-536) §0/§1 and [AVI-524](/AVI/issues/AVI-524) §1 (`Extraction`, `LabeledGraph`, `Graph`, `Gr`, `Gr_inj`, strict/lax colimit, comparison 2-cell `alpha`); this doc invents **no** names for those existing objects.

---

## 0. Notation (shared glossary — reused, not reinvented)

- **`NodeId = Text`** (`Node.hs:46`). Currently derived per source (file path for file nodes; `dirHash` + name for Haskell declarations — `Haskell.hs:43-52`); it may embed large text.
- **`Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }`** (`Node.hs:120`). The twelve canonical fields are fixed by `openspec/specs/node-schema` ("Canonical Node field set").
- **`Edge = Edge { edgeId, edgeSource, edgeTarget, edgeRelation, edgeWeight, edgeConfidence, edgeExtra }`** (`Edge.hs:88`). `Relation in {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:32`). `EdgeId = EdgeId Text` (`Edge.hs:21`).
- **`Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }`** (`Types/Graph.hs:47`), accessors `extNodes`, `extEdges` (`Types/Graph.hs:66-69`). An `Extraction` is a **local view** `V_i`.
- **`LabeledGraph = LabeledGraph { gNodes, gEdges, gAdjFwd, gAdjBack }`** (`Types/Graph.hs:83`).
- **`Graph = Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }`** (`Graph/Core.hs:47`). This is the fat object the store currently holds in full.
- **Merge as colimit.** `mergeExtractions :: Extraction -> Extraction -> Extraction` (`Core.hs:128`) and `mergeGraphs :: Graph -> Graph -> Graph` (`Core.hs:140`) are the (lax) colimit of two views: `gNodes old <> gNodes new` on node keys, edges unioned then dangling-filtered (`Core.hs:142-146`), `gDirected old` preserved (`Core.hs:157`). On **consistent** inputs this is the **strict colimit** (order-independent); on **conflicting** inputs it is a **lax colimit** with comparison 2-cell `alpha` = "keep one operand's value at each conflicting key" — direction pinned by [AVI-536](/AVI/issues/AVI-536) §2.2/§9 (M-3). `Gr` = category of context graphs (`Graph` objects, key-preserving structure morphisms); `Gr_inj subset Gr` = key-inclusion subcategory (poset-enriched).
- **Cluster is excluded.** `clusterGraphWithResolution'` (Leiden, re-detects, discards old IDs — [AVI-536](/AVI/issues/AVI-536) §4) is **not** colimit-preserving. CT-Q2 constrains only the merge colimit, never the cluster step.

---

## 1. CT-Q1 — Slimming as a projection morphism

### 1.1 Objects and morphisms — `P` before its universal property

**Definition (content / topology / identity partition of a node).** For a node `n : Node`, partition its fields:

- **identity** `Id(n) = (nodeId n)` — the key, used for dedup and join.
- **topology** `Top(n) = (nodeSourceFile n, nodeLineStart n, nodeLineEnd n, nodeKind n)` — enough to locate the node and read its kind on the query path; plus the edge/incidence structure `gAdjFwd`, `gAdjBack`, `gEdges`.
- **content** `Src(n) = (nodeLabel n, nodeSignature n)` — unbounded source text (the snippet and its structural fingerprint). `nodeExtra` is preserved **except** embedded source-text blobs it may carry.

**Definition (`P` as endofunctor on `Gr`).** Define `P : Gr -> Gr`:

- **on objects** `P(G)` erases every `Src(n)` while keeping `Id(n)`, `Top(n)`, and all incidence/edge structure; formally `gNodes(P G) = map erase (gNodes G)` where `erase` zeroes `nodeLabel` and `nodeSignature` (and any source-text in `nodeExtra`) and leaves the other fields untouched, and `gEdges(P G) = gEdges G`, `gAdjFwd(P G) = gAdjFwd G`, `gDirected(P G) = gDirected G`.
- **on morphisms** `P(f) = (f_N, f_E)` — the same key-preserving structure map, since `f` already preserves keys, incidence, and `edgeRelation`; content is not carried by morphisms.

`eta_G : G -> P(G)` with `eta_G_N = id` on keys is a valid `Gr`-morphism (injective on keys; `Gr` morphisms constrain only keys + incidence + relation, not node content — [AVI-536](/AVI/issues/AVI-536) §1.1). `P` is **idempotent**: `P (P G) = P(G)` on the nose, because erasing already-erased content is a no-op (`erase (erase n) = erase n`).

### 1.2 Universal property and uniqueness

> **Claim (`P` is the universal idempotent content-erasure — a coreflection).** `P` is an **idempotent endofunctor** with unit `eta : id => P` (`eta_G : G => P(G)`), exhibiting the **slim graphs** `SlimGr subset Gr` (objects with `Src` erased) as a **coreflective full subcategory**: the inclusion `I : SlimGr ↪ Gr` has `P` as its left reflector, `P (I x) = x` for `x : SlimGr`, and for every `G` the unit `eta_G` is universal from `G` into `SlimGr`. Equivalently, among all idempotent endofunctors `Q : Gr -> Gr` with a content-erasing natural transformation `id => Q` and `Q (Q x) = Q x`, there is a **unique** natural transformation factoring `Q` through `P`; the reflection is unique up to unique isomorphism.

> **R-Q1 (slimming = projection).** `P : Gr -> Gr` is an idempotent endofunctor (§1.1) that erases exactly the source-content fields (`nodeLabel`, `nodeSignature`, and source-text in `nodeExtra`) while preserving identity (`nodeId`), topology (`nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`, `nodeKind`), and all edge/incidence structure. `P` preserves every **query-visible graph invariant**: community (`nodeCommunityId`), degree (`nodeDegree` and `gAdjFwd`/`gAdjBack`), and adjacency are computed identically from `G` and `P(G)`. `P (P G) = P(G)`; the reflection is unique up to unique iso.

> **Why it holds.** Erasure is a uniform field-zeroing on a fixed field set, applied equally before or after any structure map; `eta` is natural because content is not part of a `Gr`-morphism's data, and idempotency is `erase (erase n) = erase n`. The invariant-preservation clause is definitional: `P` never touches keys, edges, adjacency, or the metadata columns, so communities/degrees/adjacency read off `P(G)` equal those of `G`.

### 1.3 Short stable IDs — a well-defined function of `(source_file, kind, line span, symbol name)`

**Definition (short-id function).** Let `Loc(n) = (nodeSourceFile n, nodeKind n, (nodeLineStart n, nodeLineEnd n), symbolName n)` where `symbolName` is the extracted symbol/identifier (the last token of the current `nodeId`). Define `hash_loc : Loc -> ShortId` as a **bounded token** — either an injective `file#line`-style encoding or a fixed-width hash of width `w` bits:

- **collision-free modulo hash width.** For the `file#line`-style encoding `hash_loc` is injective on `Loc`; for the fixed-width hash, two distinct locations collide only with probability bounded by the birthday bound over `w` bits — i.e. the preimage of any token has size 1 except negligibly many tokens whose preimage has size 2, and never exceeds `2^w` total collisions.
- **stable across rebuilds.** `Loc(n)` is a pure deterministic function of the extraction tree (source path, kind, line span, symbol), so `hash_loc` returns the same token for the same location under every independent extraction.

> **R-Q1a (short stable IDs).** The short node ID is the function `hash_loc : Loc -> ShortId` (§1.3), a well-defined bounded token of `(source_file, kind, line span, symbol name)` that is collision-free modulo hash width and stable across rebuilds. It is a canonicalization of the current `nodeId` (which derives from file path / module name and may embed large text): `hash_loc (Loc(n)) = shortId`, and two nodes with equal `Loc` carry equal `shortId`.

> **Surface — BREAKING note.** `openspec/specs/node-schema` fixes the twelve canonical fields; this requirement changes how `nodeId` is *derived* (from a bounded token rather than text), so it is **BREAKING** on the `enforce-query-token-budget` change: the short-ID requirement there ("identify nodes with a short stable identifier derived from file and location (or a content hash)") is exactly R-Q1a, and consumers relying on the old text-blob IDs break. This doc pins that requirement as a categorical well-definedness claim (deterministic function + injectivity mod width); [AVI-566](/AVI/issues/AVI-566) restates the same property in categorical language.

### 1.4 Acceptance (CT-Q1)

> **AC-1 (ID stability across independent extractions).** Run two independent extractions of the same source tree; for every node `n`, assert `hash_loc(Loc(n))` is identical in both (stability), and that distinct `Loc` values do not collide beyond the birthday bound over hash width `w` (collision-freeness modulo width). *Grounding:* §1.3 well-definedness; the token-budget short-ID scenario ("re-running extraction on unchanged input yields the same id").

---

## 2. CT-Q2 — Colimit preservation

### 2.1 Statement

> **Claim (P preserves the merge colimit).** Let `D : I -> Gr` be a finite diagram of local views `{V_i}` whose colimit is the merged graph `merge(D) = colim D` (the strict colimit for consistent inputs, the lax colimit with 2-cell `alpha` for conflicting inputs — [AVI-536](/AVI/issues/AVI-536) §2). Then projecting the merged graph equals merging the projected views, up to canonical iso:
>
> ```
>        P( colim D )   ≅   colim ( P ∘ D )   =   merge( P(V_i) )
> ```
>
> i.e. `P(merge(V_i)) ≅ merge(P(V_i))`.

### 2.2 Universal property and uniqueness

> **Claim (colimit preservation = unique canonical iso).** `P` is a **colimit-preserving functor** for the merge diagrams: the canonical comparison morphism `merge(P(V_i)) -> P(merge(V_i))` (induced by the cocone legs `P(V_i) -> P(merge(V_i))` composed with `eta_{merge(V_i)}`) is an **isomorphism**, and by the **uniqueness of colimits up to unique isomorphism** it is the unique morphism commuting with both cocones. Hence `P` sends the colimit cone over `D` to a colimit cone over `P ∘ D`.

> **R-Q2 (projection commutes with merge = colimit preservation).** `P : Gr -> Gr` satisfies `P(merge(V_i)) ≅ merge(P(V_i))` via a canonical iso that is the identity on all query-visible structure (§1.1). The comparison morphism is unique up to unique iso (uniqueness of colimits).

> **Why it holds.** Merge resolves conflicts on keys; `P` erases content uniformly. On **content keys** (`nodeLabel`, `nodeSignature`) both sides are erased, so no conflict survives projection in either order — the content 2-cell `alpha` collapses to the identity whether merge runs before or after `P`. On **preserved keys** (`nodeId`, `nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`, `nodeKind`, `nodeCommunityId`, `nodeDegree`, `nodeIsBridge`, edges) merge applies the same conflict-resolution (`alpha`) on both sides, and `P` leaves those values untouched, so `merge` and `P` commute pointwise. The result is therefore independent of the **direction** of `alpha` (old-wins vs new-wins — [AVI-536](/AVI/issues/AVI-536) M-3): erasure makes content conflict-free, and preserved fields resolve identically, so `P(merge(V_i)) = merge(P(V_i))` on every field. The iso is the identity on query-visible structure; it is unique by colimit uniqueness.

> **Scope note.** CT-Q2 concerns only the **merge** colimit. The subsequent **cluster** step is not colimit-preserving ([AVI-536](/AVI/issues/AVI-536) §4); R-Q2 makes no claim across `clusterGraphWithResolution'`.

### 2.3 Surface

> **Surface.** `openspec/specs/09-merge` ("merge two knowledge graphs: deduplicate by NodeId, last-write from B wins, union edges") — the merge colimit; `docs/math-requirements/AVI-536-colimit-merged-context-graph.md` (strict/lax colimit vocabulary). Note the 09-merge spec text says "last-write from B wins" while [AVI-536](/AVI/issues/AVI-536) §9 (M-3) empirically pins the runtime `Data.Map.<>` as **left/first-operand wins**; R-Q2 is **independent of this direction** (§2.2), so the two readings do not affect the colimit-preservation claim.

### 2.4 Acceptance (CT-Q2)

> **AC-2 (merge-then-project ≅ project-then-merge).** On a fixture of local views `{V_i}` (including overlapping keys with conflicting content): assert `P(merge(V_i)) == merge(P(V_i))` — strict equality of the slim projections on every field, and in particular that the merged-slim node values equal the project-then-merge node values at every key. *Grounding:* §2.2; the AVI-536 AC-3 associativity test shape lifted to projection.

---

## 3. CT-Q3 — No-loss contract

### 3.1 Statement

> **Claim (P loses only source content).** For every query-visible column `c` and every node `n`: `c(P(n)) = c(n)`. The only columns `P` may change are the source-content columns (`nodeLabel`, `nodeSignature`, and source-text within `nodeExtra`); every other canonical column is preserved exactly.

### 3.2 Definition of the query-visible column set

**Definition (query-visible columns).** Partition the twelve canonical `Node` fields (`node-schema`) into:

- **query-visible** `QueryVis = { nodeId, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeCommunityId, nodeKind, nodeDegree, nodeIsBridge }` (ten fields). These are the columns the query path reads for identity (`nodeId`), ranking (`nodeDegree`, `nodeKind`), grouping (`nodeCommunityId`), location (`nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`), and display (`nodeFileType`, `nodeIsBridge`).
- **content** `Content = { nodeLabel, nodeSignature }` (two fields) — the unbounded source text; plus any source-text blob inside `nodeExtra`.

> **R-Q3 (no-loss contract).** For every query-visible column `c in QueryVis` and every node `n`, `c(P(n)) = c(n)` (§3.1). `P` may alter only the content columns `Content = {nodeLabel, nodeSignature}` (and source-text within `nodeExtra`). Equivalently, `P` is the **maximal** endofunctor that acts as the identity on `QueryVis`: among all endofunctors `Q` with `c(Q n) = c(n)` for every `c in QueryVis`, `P` erases the greatest content, and any two such coincide on every node — unique up to the identity on `QueryVis`.

> **Why it holds.** By construction `erase` zeroes exactly `nodeLabel` and `nodeSignature` (`Node.hs:120-134`) and leaves the other ten fields intact; so restricting `P` to any `c in QueryVis` yields `c(n)`. Maximality/uniqueness: any endofunctor preserving all of `QueryVis` can only differ from `P` on `Content`, and `P` already erases the maximal content, so the difference is confined to the content columns — uniqueness up to the identity on `QueryVis`.

> **Arbitration flag.** Classifying `nodeSignature` as content (rather than query-visible) is a judgment: it is erased here because it carries structural text; if the Product Owner wants signatures retained in slim responses, move `nodeSignature` into `QueryVis` (the property test in AC-3 enumerates both columns so either classification is testable). This is open to Head of R&D arbitration.

### 3.3 Surface

> **Surface.** `openspec/specs/graph-json-contract` ("compact node serialization: `id`, `source_file`, `score`, `kind`, truncated `label`; SHALL NOT emit full source text in list responses") — the serialization contract that makes `QueryVis` the query-visible set; `openspec/specs/node-schema` ("Canonical Node field set", twelve fields) — the field set AC-3 enumerates.

### 3.4 Acceptance (CT-Q3)

> **AC-3 (no-loss over the full node-schema field set).** For a fixture of nodes covering present/absent optional fields, assert for each of the ten `QueryVis` columns that `c(P(n)) = c(n)` (preserved), and assert that `nodeLabel` and `nodeSignature` are the only columns `P` may change (content). *Grounding:* §3.2; the node-schema "Canonical Node field set" requirement.

---

## 4. JOINT-Q — determinism of the budget-cut order

**JOINT-Q — determinism of the budget-cut order.** The final output order (GT-Q1 rank × GT-Q3 kappa × deterministic tie-break) is a **total order that is a function of the graph as a mathematical object**, independent of Haskell `Map` iteration/insertion order. GT defines the order; CT proves it is well-defined (representation-independent). *Acceptance:* determinism property — the same graph built under two insertion orders ⇒ byte-identical slim output.

> **CT role.** The Graph Theory Expert defines the ranking order on slim nodes; CT proves that order is **representation-independent** — a function of the graph *as a mathematical object*, not of its `Data.Map` encoding. Concretely: let `order_G` be GT's total order on the slim nodes of `G`. CT asserts `order_G` depends only on `(V, E, src, tgt, rel, w)` and the query-visible columns (`CT-Q3`), **not** on key-set insertion order; equivalently, for the canonical iso `phi : G1 ≅ G2` (same structure, different `Map` insertion), `order_{G1} = order_{G2}` under the transported labeling. Since `P` preserves query-visible structure (`CT-Q3`) and GT's order is defined on that structure, the composite "project then rank" is well-defined: `order_{P(G1)} = order_{P(G2)}` whenever `G1 ≅ G2`. Uniqueness: the total order is unique once GT fixes the ranking keys and the tie-break (a total order has no non-trivial automorphism preserving it).

> **Surface.** `openspec/changes/enforce-query-token-budget/specs/query-response-budget/spec.md` ("deterministically rank and cut results"; "highest-scoring nodes retained") — the budget-cut surface; JOINT-Q pairs with the Graph Theory Expert, whose combinatorial validation of the total order is the acceptance bar.

> **Acceptance (JOINT-Q).** Build the same graph under two different `Map` insertion orders; assert the slim output ranked under GT's order is **byte-identical** in both (`order` independent of insertion). *Grounding:* §4 representation-independence; the byte-identical-slim-output determinism property.

---

## 5. Requirements traceability table

| Req | Statement | Surface | Acceptance | Locked? |
|---|---|---|---|---|
| R-Q1 | `P` idempotent endofunctor, erases content, preserves topology/identity/invariants (§1.2) | `Node.hs:120`, `Graph/Core.hs:47`, `Gr`/`Gr_inj` | AC-1 (id stability) | structural |
| R-Q1a | short ID = well-defined bounded function of `(source_file, kind, line span, symbol name)`, collision-free mod width, stable | `enforce-query-token-budget` (BREAKING), `node-schema` | AC-1 (stability + collision bound) | BREAKING note |
| R-Q2 | `P(merge(V_i)) ≅ merge(P(V_i))` — colimit preservation (§2.2) | `mergeGraphs`/`mergeExtractions` (`Core.hs:140`,`128`), `09-merge`, AVI-536 | AC-2 (merge-then-project ≅ project-then-merge) | structural |
| R-Q3 | no-loss: `c(P(n)) = c(n)` for `c in QueryVis`; only content lost (§3.2) | `node-schema`, `graph-json-contract` | AC-3 (field-by-field over 12 fields) | arb: `nodeSignature` |
| JOINT-Q | budget-cut order is a total order, function of graph object, independent of `Map` order (§4) | `query-response-budget` | determinism (byte-identical slim output) | pairs w/ GT expert |

---

## 6. Acceptance criteria — checkable property tests (Graphos-Dev implement)

> **AC-1 (CT-Q1: ID stability + collision bound).** Two independent extractions of the same tree; for every node, `hash_loc(Loc(n))` agrees across runs (stability); the number of colliding `Loc` pairs is bounded by the birthday bound over hash width `w` (collision-free mod width). *Grounding:* §1.3; token-budget "re-running extraction on unchanged input yields the same id".

> **AC-2 (CT-Q2: colimit preservation).** Fixture of views `{V_i}` with overlapping keys and conflicting content; assert `P(merge(V_i)) == merge(P(V_i))` on every field (node fields, edges, adjacency). *Grounding:* §2.2; independent of `alpha` direction.

> **AC-3 (CT-Q3: no-loss over field set).** Fixture of nodes with present/absent optional fields; for each of the ten `QueryVis` columns assert `c(P(n)) = c(n)`; assert only `nodeLabel`, `nodeSignature` (and source-text in `nodeExtra`) may change. *Grounding:* §3.2; node-schema "Canonical Node field set".

> **AC-4 (JOINT-Q: determinism).** Build the same graph under two insertion orders; assert the ranked slim output is byte-identical in both. *Grounding:* §4 representation-independence.

---

## 7. Determinism lens

- **Projection determinism (§1.1):** `erase` is a uniform field-zeroing on a fixed field set; it is a pure function of the node, so `P(G)` is a deterministic function of `G`. No nondeterminism enters at projection.
- **Colimit-preservation determinism (§2.2):** `P(merge(V_i))` and `merge(P(V_i))` are equal on every field regardless of merge operand order and `alpha` direction, because content is made conflict-free by erasure and preserved fields resolve identically. The result is a function of the diagram, not of fold order.
- **ID determinism (§1.3):** `hash_loc` is a pure function of `Loc` (a pure function of the extraction tree), so short IDs are stable across rebuilds and independent of any `Map` iteration order.
- **Budget-cut order determinism (§4):** GT's total order is defined on query-visible structure; CT proves it representation-independent (§4), so the combined project-then-rank pipeline outputs a function of the graph object, independent of `Map` insertion. Byte-identical slim output under reordered insertion (AC-4).

---

## 8. Math-judgment / escalation log

| # | Decision | Rationale / source |
|---|---|---|
| M-1 | `P` idempotent endofunctor, coreflection onto `SlimGr` | Erasure is uniform field-zeroing; `Gr` morphisms constrain keys + incidence + relation only ([AVI-536](/AVI/issues/AVI-536) §1.1). |
| M-2 | `Gr` morphisms do not constrain node content | [AVI-536](/AVI/issues/AVI-536) §1.1: `f_N, f_E` key-preserving + preserve `edgeRelation`; node content is not morphism data, so `eta_G : G -> P(G)` is a valid morphism. |
| M-3 | R-Q2 independent of `alpha` direction (old-wins vs new-wins) | Erasure makes content keys conflict-free on both sides; preserved fields resolve identically under either policy ([AVI-536](/AVI/issues/AVI-536) §2.2, M-3). |
| M-4 | `nodeSignature` classified as content (arbitrable) | It carries structural text; erasable under R-Q3. If the Product Owner wants it retained, move it to `QueryVis`; AC-3 tests both (AC-3). Open to Head of R&D. |
| M-5 | Cluster excluded from CT-Q2 | `clusterGraphWithResolution'` is not colimit-preserving ([AVI-536](/AVI/issues/AVI-536) §4); R-Q2 constrains only merge. |
| M-6 | short-ID well-definedness = deterministic function + injectivity mod width | Definition of `hash_loc` (§1.3); the token-budget BREAKING requirement. |

---

## 9. Prior art

- **Idempotent functors / coreflections and uniqueness up to unique iso.** Mac Lane, *Categories for the Working Mathematician*, Ch. IV, §9 (idempotent functors, reflections); Riehl, *Categorical Logic and Topology*, Ch. 2 (reflective subcategories, uniqueness of colimits).
- **Colimit preservation by a functor; uniqueness of colimits up to unique iso.** Mac Lane & Moerdahl, *Sheaves in Geometry and Logic*, Ch. II (local-to-global = colimit over a cover; sheaf condition = colimit equality) — the reference for "project commutes with merge."
- **Lax colimit / comparison 2-cell.** Street, *Fibrations and Yoneda's theorem in an n-category*; Street & Walters, *Fibrations = Yoneda* — the reference for §2.1 "commutes only up to `alpha`."
- **Deterministic canonical ordering independent of hash/Map iteration.** standard in canonical-DAG / canonical-serialization literature (e.g. CID / content-addressable canonical forms); the claim that a total order defined on structure is representation-independent is the defining property of a canonical order.

---

## 10. Next Actions (Graphos-Dev owns the concrete next step)

1. **Property tests (AC-1..AC-4).** Implement the four property tests: ID stability + collision bound (AC-1), merge-then-project ≅ project-then-merge (AC-2), field-by-field no-loss over the 12 node-schema fields (AC-3), byte-identical slim output under reordered insertion (AC-4). *Grounding:* §6.
2. **Resolve M-4 (nodeSignature).** Confirm with the Product Owner whether `nodeSignature` is query-visible or content; lock the `QueryVis`/`Content` partition in AC-3 before finalizing R-Q3.
3. **Loop JOINT-Q with the Graph Theory Expert.** CT proves the budget-cut order is representation-independent once GT defines it; confirm GT's total-order definition and that the byte-identical-slim-output test (AC-4) holds. This is the joint surface set by the Head of R&D.
4. **Escalate** any category-level judgment above confidence to the Head of R&D (per execution contract); e.g., M-4 (`nodeSignature` classification) and whether `SlimGr` should carry the erased content as a labeled "missing" marker rather than fully erasing it.

---

## 11. Definition of done

- [x] Three requirements, each with statement + universal property/uniqueness + Graphos surface + checkable acceptance criterion (CT-Q1 §1.2/R-Q1, CT-Q2 §2.2/R-Q2, CT-Q3 §3.2/R-Q3).
- [x] JOINT-Q stated verbatim (§4), paired with the Graph Theory Expert.
- [x] Notation reused from the shared glossary (§0); no invented names for existing objects.
- [ ] Property tests AC-1..AC-4 implemented by Graphos-Dev (child follow-up).
- [ ] M-4 (nodeSignature) resolved with the Product Owner before finalizing R-Q3.
