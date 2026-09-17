# Local-to-Global Consistency — Cocone Morphisms & Pullback-on-Labels — Math Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-537](/AVI/issues/AVI-537) (child of [AVI-512](/AVI/issues/AVI-512), child (c))
**Builds on:** [AVI-511](/AVI/issues/AVI-511) — Merge / Consolidation as a Colimit (`AVI-511-colimit-merge.md`); extends [AVI-529](/AVI/issues/AVI-529) §1–§2 (UQI cross-reading) and closes [AVI-511](/AVI/issues/AVI-511) §6 (Arbitration Flag — Local-to-Global Consistency).
**Status:** requirements document — grounded in domain model + openspec specs. One notation, one glossary, every requirement mapped to a concrete Graphos surface and tagged **Proven / Assumed 🔧 / Open**.
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Notation (shared glossary)

Notation is inherited verbatim from [AVI-511](/AVI/issues/AVI-511) §0 (verified against checked-in sources). Reproduced only where this doc depends on it; line numbers re-verified.

- `NodeId = Text` (`Node.hs:46`; spec `domain-types` §Requirement "Domain.Types.Node"). `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits }` (`Node.hs:120`). The record `Node` is the **label** carried by a node in this doc ("pullback on labels" = pullback on these records).
- `Edge = Edge { edgeId, edgeSource, edgeTarget, edgeRelation, edgeWeight, edgeConfidence, edgeExtra }` (`Edge.hs:88`). `EdgeId = EdgeId (source <> "->" <> target <> ":" <> relationToText relation)` (`lsp-edge-extraction` §Requirement "Infrastructure.LSP.Extraction"). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}`.
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map (NodeId, NodeId) Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected, … }` (`Graph/Core.hs:47`). Edges are keyed by `(source, target)` pairs (`Core.hs:49`).
- `Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }` (`Types/Graph.hs:47`), accessors `extNodes / extEdges` (`Types/Graph.hs:66-69`). This is a per-file LSP **view**. `emptyExtraction` (`Types/Graph.hs:54`) is the initial view.
- **Merge primitives (surfaces this doc constrains):**

| Name | Signature | Core semantics | Source |
|---|---|---|---|
| `buildGraph` | `Bool -> Extraction -> Graph` | drops dangling edges; builds adjacency from edge incidence; takes `directed` flag | `Core.hs:96` |
| `mergeExtractions` | `Extraction -> Extraction -> Extraction` | `Data.Map.union` on nodes and edges — **left-biased: first operand (a) wins** on key conflict (`containers`; `Map.union` prefers the left map) | `Core.hs:130` |
| `mergeGraphs` | `Graph -> Graph -> Graph` | `gNodes old <> gNodes new` (**old/first operand wins**, left-biased), edges keyed by `(src,tgt)` unioned then filtered to drop dangling, adjacency `Map.unionWith Set.union`, `gDirected old || gDirected new` (canonical: directed iff either input is directed; order-independent, [AVI-658](/AVI/issues/AVI-658) M1 T1), hash recomputed | `Core.hs:154` |

- **Agreement region (this doc's central predicate).** For two views `A, B` with a common target shape, define the **agreement set**
  ```
  Agree(A,B) = { k | k ∈ keys(extNodes A) ∩ keys(extNodes B) ∧ nodeValue_A(k) = nodeValue_B(k) }
             ∪ { (s,t,r) | (s,t) ∈ keys'(adj A) ∩ keys'(adj B) ∧ edgeValue_A(s,t) = edgeValue_B(s,t) }
  ```
  i.e. the keys present in **both** views carrying **identical** label values. This is precisely the domain on which a value-preserving embedding into both exists ([AVI-511](/AVI/issues/AVI-511) §1.1).

> **Constrains (required by the issue).** Every requirement below names one of these surfaces. The one behavior-constraining requirement (LG-C3, status 🔧) required a loop with [devhaskell](/AVI/agents/devhaskell) before finalizing the Haskell semantics — now closed (AVI-647): all requirements are Proven, LG-C3 locked.

---

## 1. Objects and morphisms — the cocone over views

### 1.1 The indexing category and the view functor

**Definition (restated from [AVI-511](/AVI/issues/AVI-511) §1, verified).** The category `𝔻` has `Extraction` values as objects and **injective value-preserving view-embeddings** as morphisms `(φ_N, φ_E)`: `φ_N`, `φ_E` injective; edge incidence (`edgeSource/edgeTarget`) and `edgeRelation` preserved; and the `Node`/`Edge` *value* at the image equals the source value. `F_d : 𝔻 → Graph` sends `V ↦ buildGraph d V` and `(φ_N,φ_E)` to the graph morphism carrying `gNodes/gEdges/gAdjFwd/gAdjBack` along `φ_N/φ_E` and preserving `gDirected` ([AVI-511](/AVI/issues/AVI-511) §2).

**Definition (the cocone).** Fix a family of views `{V_i}`_{i∈I} with a consistent overlap structure, and let `D = F_d ∘ 𝑫 : I → Graph` be the indexing diagram. A **cocone** under `D` is a vertex `M` (a single `Graph`) together with a family of morphisms `{ι_i : D(V_i) → M}`_{i∈I} (the **cocone legs**) such that for every morphism `(V_i → V_j)` in the diagram `ι_i = ι_j ∘ D(V_i → V_j)`. The **colimit** `(colim D, {ι_i})` is the terminal cocone: for any other cocone `(P, {λ_i})` there is a **unique** morphism `u : colim D → P` with `u ∘ ι_i = λ_i` for all `i`.

> **Constrains:** `buildGraph` (`Core.hs:96`), `Graph` (`Graph/Core.hs:47`), cocone-leg type `F_d(V_i) → Graph`.

### 1.2 Cocone legs are concrete inclusions

In Graphos the apex is `M = mergeGraphs` of the constituent graphs and the legs are the **canonical key-wise inclusions**:

```
ι_A : gNodes A  ↪  gNodes A <> gNodes B      (Map.insert / union inclusion on the left operand)
ι_B : gNodes B  ↪  gNodes A <> gNodes B      (inclusion on the right operand)
```

on `gNodes`; analogously on `gEdges` (keyed by `(NodeId, NodeId)`) and on `gAdjFwd/gAdjBack` (`Map.unionWith Set.union`). The leg `ι_x` is *not* a total map on keys — it is defined on `keys(D(V_x))` and lands in `keys(M)`; on a shared key it lands in the single merged entry. This is exactly what "structure-preserving graph morphism" means concretely here.

> **Constrains:** `mergeGraphs` (`Core.hs:154,157-163`).

---

## 2. Universal property — consistency as terminal cocone + pullback-on-labels

### 2.1 The consistency claim (colimit = colimit of local views)

**Claim (LG-COLIM).** `M = colim(F_d ∘ 𝑫)` is the colimit of the local extraction views: it is the terminal cocone under `F_d ∘ 𝑫`. By [AVI-511](/AVI/issues/AVI-511) §3.1 the mediating morphism to any cocone is **unique**, and any two colimits are related by a **unique isomorphism commuting with the legs** (uniqueness up to unique iso).

**What this fixes.** The merged graph's node set is `⋃_i keys(extNodes V_i)` (each shared key **once**), its edge set is `⋃_i keys(extEdges V_i)` (each once), and adjacency is the union — realized by `Map.union` / `Map.unionWith Set.union`. It pins (a) no duplicate copies remain ([AVI-511](/AVI/issues/AVI-511) §4, INV-MERGE) and (b) the legs are jointly surjective onto `M`. It does **not** fix attribute values on a **conflict** (§3).

> **Constrains:** `mergeExtractions` (`Core.hs:129-132`), `mergeGraphs` (`Core.hs:154-159`).

### 2.2 The SHALL — shared nodes map identically under both cocone legs

> **LG-C2 (SHALL, required by the issue).** Graphos SHALL ensure that for any two overlapping views `A, B` that **agree** on a shared node key `k` (i.e. `k ∈ keys(extNodes A) ∩ keys(extNodes B)` with `nodeValue_A(k) = nodeValue_B(k)`), the two cocone legs map `k` to **identical** nodes in the merged graph:
> ```
> ι_A(k) = ι_B(k)   in   M
> ```
> Equivalently: the merged node at key `k` is equal (by `Eq` on `Node`) to the node carried by `A` at `k` and to the node carried by `B` at `k`. The same statement holds for shared edges keyed identically with identical value.

This is the operational form of "local and global truth agree on overlaps": the label seen locally in either view is the label held globally. It is exactly [AVI-511](/AVI/issues/AVI-511) §5 structural naturality ([R7](AVI-511-colimit-merge.md)) specialized to the node-label leg.

> **Constrains:** `mergeGraphs` (`Core.hs:154,157`), `Node` (`Node.hs:120`), `extNodes` (`Types/Graph.hs:66`).

### 2.3 The pushout square is a **pullback on labels** where views agree

**Claim (LG-PULLBACK).** On the agreement region `Agree(A,B)` (§0), the 2-way merge square
```
   Agree(A,B)  ─▶  A
     ▟             ▟
    ▟              ▶  M = mergeGraphs A B
   B  ─▶  M
```
is a **pushout** (colimit over the span, [AVI-511](/AVI/issues/AVI-511) §3.2) **and** a **pullback restricted to labels**: the agreement overlap is exactly the fiber product of the two legs over `M`,
```
   Agree(A,B)  ≅  A ×_M  B   =  { (a, b) ∈ keys(A) × keys(B) : ι_A(a) = ι_B(b) }   (on identical-label keys).
```

**Why the equivalence holds (diagram honesty).** Forward direction: if `k ∈ Agree(A,B)` then `ι_A(k) = ι_B(k)` (LG-C2), so `(k,k) ∈ A ×_M B`. Backward direction: if `ι_A(a) = ι_B(b)` for `a ∈ keys(A)`, `b ∈ keys(B)`, then `a` and `b` resolved to the same merged node; because the legs are value-preserving inclusions, this forces `a = b = k` with identical label — i.e. `(a,b)` is an agreement key. Neither leg can send two *distinct-label* entries to the same node on the agreement region, so no spurious identification occurs and no duplicate survives. Hence the square is a pullback precisely on `Agree(A,B)`.

**What this pins.** Merge introduces **no ident beyond those forced by shared identical labels** and retains **no duplicate copy** of any agreed label. That is the conjunction of dedup ([AVI-511](/AVI/issues/AVI-511) §4) and structure-preservation ([AVI-511](/AVI/issues/AVI-511) §5), stated as a universal property of the square rather than as two separate facts.

> **Constrains:** `mergeGraphs` (`Core.hs:154,157-163`), `extNodes/extEdges` (`Types/Graph.hs:66-69`), `Node/Edge` records (`Node.hs:120`, `Edge.hs:88`). **RESOLVED** by [devhaskell](/AVI/agents/devhaskell) (AVI-647): `Data.Map.union` is left-biased; the *only* identifications `mergeGraphs` introduces are shared-key collapses and conflicting-pair over-identification (LG-C4) — no hidden cross-key value-collision. LG-C3 locked.

---

## 3. Requirements register (LG-C1 … LG-C5)

Status legend: **Proven** = established within these docs (invariant / standard categorical fact); **Assumed 🔧** = required design semantic pending [devhaskell](/AVI/agents/devhaskell) confirmation; **Open** = needs an external decision.

| ID | Req (statement) | Surface | Acceptance | Status |
|---|---|---|---|---|
| **LG-C1** | **Cocone legs are structure-preserving graph morphisms.** Each `ι_i : F_d(V_i) → M` preserves `gNodes` injection, edge incidence (`edgeSource/edgeTarget`), `edgeRelation`, `edgeWeight`, `edgeConfidence`, and maps `gAdjFwd/gAdjBack(V_i)` into those of `M`; preserves `gDirected`. | `mergeGraphs` (`Core.hs:154-163`), `buildGraph` (`Core.hs:96`) | property (morphism on each leg) | Proven |
| **LG-C2** | **SHALL — shared nodes map identically under both cocone legs.** For agreeing overlapping views, `ι_A(k) = ι_B(k)` in `M` (and dually for edges). | `mergeGraphs` (`Core.hs:154,157`), `Node` (`Node.hs:120`) | property (shared-node equality) | Proven |
| **LG-C3** | **Pushout = pullback on labels where views agree.** `Agree(A,B) ≅ A ×_M B` on identical-label keys; merge adds no spurious identification and keeps no duplicate. | `mergeGraphs` (`Core.hs:154,157-163`), `Data.Map.union` | property (fiber-product identity) | Proven (locked, AVI-647) |
| **LG-C4** | **Honesty boundary.** Where views disagree (shared key, differing label) no value-preserving embedding exists; the square is a pushout but **not** a pullback (the fiber product over-identifies the conflicting pair). Pullback-on-labels holds ⟺ `Agree(A,B)` = full overlap. | `mergeGraphs` (`Core.hs:154-159`, left-biased `Data.Map.union`) | counterexample (conflict over-identifies) | Proven |
| **LG-C5** | **Local↔global structure agreement.** The structural projection `Struct : Graph → (NodeMap, EdgeMap, Bool)` is a natural iso: `Struct(colim D) ≅ ⋂_i Struct(D(V_i))` over the overlap. Extends [AVI-511](/AVI/issues/AVI-511) [R7](AVI-511-colimit-merge.md). | `mergeGraphs` (`Core.hs:154-163`) | property (Struct square commutes) | Proven |

---

## 4. Detailed requirements with acceptance tests

### LG-C1 — Cocone legs are structure-preserving graph morphisms

For each leg `ι_x` of the cocone over `M = mergeGraphs A B`:
- **node injection:** `ι_A = inclusion (gNodes A ↦ gNodes M)` is injective on `keys(gNodes A)`; likewise `ι_B`.
- **incidence preserved:** for every edge `e ∈ gEdges A` with `(src,tgt)`, `(ι_N(src), ι_N(tgt)) ∈ gEdges M` and `edgeRelation`, `edgeWeight`, `edgeConfidence` equal the carried value (`Core.hs:158-159`, dangling filter preserves surviving incidences).
- **adjacency preserved:** `gAdjFwd(A)(v) ⊆ gAdjFwd(M)(ι_N(v))` (`Core.hs:160-163`), and symmetrically for `gAdjBack` and for `B`.
- **directed flag:** `gDirected M = gDirected A || gDirected B` (canonical: directed iff either input is directed; order-independent, [AVI-658](/AVI/issues/AVI-658) M1 T1), constant across both legs.

**Acceptance (property test).** For random small graphs `A, B` built via `buildGraph` from consistent extractions, assert for every edge `e ∈ gEdges A`: `edgeRelation, edgeWeight, edgeConfidence` of the surviving entry in `M` equal those of `e`; and `neighbors_M(ι_A v) ⊇ neighbors_A(v)` for all `v ∈ keys(gNodes A)`; likewise `B`. (This is [CT-R4 functoriality](AVI-511-colimit-merge.md#2-functoriality-of-the-pipeline) applied to the cocone legs.)

### LG-C2 — SHALL: shared nodes map identically under both cocone legs (primary acceptance)

**Acceptance (property test, the issue's checkable criterion).** Construct `A, B` sharing a node key `k` with **identical** `Node` value; compute `M = mergeGraphs A B`. Assert:
```
Map.lookup k (gNodes M) == Map.lookup k (gNodes A)   -- ι_A(k) equals the A-label
Map.lookup k (gNodes M) == Map.lookup k (gNodes B)   -- ι_B(k) equals the B-label
```
so `ι_A(k) = ι_B(k)` in `M`. Repeat over random fixtures with `≥ 1` shared agreeing node and `≥ 1` shared agreeing edge; also assert the same for edges keyed identically. This is the exact acceptance criterion named by the issue: *for overlapping views, shared nodes map identically under both cocone legs.*

### LG-C3 — Pushout is a pullback on labels where views agree

**Acceptance (property test).** For consistent `A, B` (full overlap = agreement), assert the fiber-product identity on keys:
```
keys(Agree(A,B))  ==  { (a,b) ∈ keys(gNodes A) × keys(gNodes B) : Map.lookup a (gNodes A) resolved == Map.lookup b (gNodes B) resolved in M }
```
and the dedup identity `|keys(gNodes M)| = |keys(gNodes A) ∪ keys(gNodes B)|` (no duplicate copy). The two together state `Agree(A,B) ≅ A ×_M B` **and** jointly-surjective legs. Property-test over random consistent diagrams with overlapping keys.

### LG-C4 — Honesty boundary (pullback fails on conflict)

**Acceptance (counterexample).** Construct `A, B` sharing a key `k` with **different** `Node` values. Then `k` is **not** in `Agree(A,B)`, the square is still a pushout (`M = mergeGraphs A B` exists via left-biased `Data.Map.union`, `Core.hs:157-159`) but the fiber product `{(a,b) : ι_A(a)=ι_B(b)}` contains the conflicting pair `(k_A, k_B)` that resolved to one entry — an identification not present in either view as a value-preserving object ([AVI-511](/AVI/issues/AVI-511) §3.3). Assert the test records this as *expected non-pullback* (a policy artifact, not a correctness violation). Requirement: the doc must state this boundary explicitly (it does, §3).

### LG-C5 — Local↔global structure agreement (naturality)

**Acceptance (property test).** For the structural projection `Struct g = (gNodes g, gEdges g, gDirected g)`, assert the square
```
   ⊔_i F_d(V_i)  ── coproduct ──▶  colim (=M)
        Struct                       Struct
         ▼                           ▼
   (⋃ nodes, ⋃ edges, gDirected A)  ==  Struct(M)
```
commutes as an isomorphism: `keys(Struct M) = ⋃_i keys(Struct(D(V_i)))` and `gDirected M = gDirected A`. This is [AVI-511](/AVI/issues/AVI-511) [R7](AVI-511-colimit-merge.md) restated as the local-to-global agreement bar for labels/structure.

---

## 5. Joint reconciliation with the Graph Theory Expert (joint surface, assigned by Head of R&D)

The merged graph `M` is simultaneously (i) the colimit of extraction views (this doc) and (ii) a weighted multigraph `G = (V,E,src,tgt,rel,w)` on which Leiden runs ([AVI-511](/AVI/issues/AVI-511) §6.3, [AVI-529](/AVI/issues/AVI-529)). The categorical claim here supplies the **universal-property** side; the combinatorial side is the Graph Theory Expert's acceptance bar:

- **JOINT-LG1 (node count).** `|V| = |keys(gNodes M)| = |⋃_i keys(extNodes V_i)|` — the colimit's node set **is** the vertex set Leiden reads ([AVI-511](/AVI/issues/AVI-511) §4.2, [AVI-529](/AVI/issues/AVI-529) UQI-3). The pullback property (LG-C3) guarantees this union counts each agreed key once — no double-count on overlaps.
- **JOINT-LG2 (edge count).** `|E| = |keys(gEdges M)| = |⋃_i keys(extEdges V_i)|` after the dangling drop; each shared edge counted once. Degree/modularity inputs (`neighbors`/`degree`, `Graph/Query.hs:49,56`) derive from `M`'s adjacency, fixed by the colimit universal property.
- **JOINT-LG3 (overlap degree agreement).** For a node `v` present in overlapping views, `degree_M(v) ⊇ degree_A(v) ∪ degree_B(v)` and equals the union restricted to globally-present neighbors — the combinatorial witness that the cocone legs are morphisms (LG-C1) and the square is a pullback on the overlap (LG-C3).

**Synthesis statement (must appear in every implementation touching both layers).** The merged context graph is one object; structurally it is the terminal cocone of local extraction views (category), and combinatorially it is the weighted multigraph Leiden reads (graph theory). The cocone legs are structure-preserving graph morphisms (LG-C1); on the region where two views carry identical labels the pushout square is a pullback — shared nodes map identically under both legs (LG-C2/LG-C3) and `|V|,|E|` count each shared key once (JOINT-LG1/2); the square ceases to be a pullback exactly where views conflict (LG-C4). The graph-theory degree/vertex/edge counts are the acceptance bar validating the categorical universal property on the same object.

---

## 6. Arbitration Log

- **AQ-LG1 — Is "pullback on labels" more than re-stated dedup?** It is not tautological: dedup alone says "one copy per key"; the pullback statement additionally forbids **over-identification** (two distinct entries collapsing to one node) on the overlap, which plain left-biased `Data.Map.union` could do under a conflict. Pullback-on-labels therefore constrains the merge beyond cardinality — it pins the *injection* of the overlap into `M`. **Decision: SETTLED** (LG-C3 vs LG-C4 boundary).
- **AQ-LG2 — Does the pullback hold at the edge level too?** Symmetric to nodes: on agreeing edges `ι_A(e) = ι_B(e)` (LG-C2 dual) and the edge fiber product equals the agreement set. **Two-level honesty.** At the *Extraction* level, `EdgeId = source->target:<relation>` keeps relation-differing edges distinct ([lsp-edge-extraction] unique-`EdgeId` rule) — they do not merge there. But at the *Graph* level, `gEdges :: Map (NodeId, NodeId) Edge` is keyed by endpoints only, so relation-differing edges with the same endpoints collapse by documented design in `buildGraph` (`Core.hs:103`), a pre-existing modeling choice (openspec `09-merge`: edges unioned) **orthogonal to LG-C3**. The LG-C3 edge clause therefore holds on the Extraction/`EdgeId` level; the Graph-level collapse is a separate JOINT-LG2 concern flagged for the joint review. **Decision: SETTLED** (LG-C3 dual, LG-C5); JOINT-LG2 flagged.
- **O-LG1 — RESOLVED by [devhaskell](/AVI/agents/devhaskell) (AVI-647).** `Data.Map.union` is left-biased (`containers`); the *only* identifications `mergeGraphs` introduces are shared-key collapses and conflicting-pair over-identification (LG-C4), both already classified as non-pullback by LG-C4. No hidden cross-key value-collision. Gates LG-C3's 🔧 lock — now released.

---

## 7. Surface Map (every requirement → concrete Graphos surface)

- **Cocone apex / colimit:** `UseCase/Merge.hs` (`mergeGraphsAndAnalyze:37`), `Domain/Graph/Core.hs` (`mergeGraphs:140`, `mergeExtractions:129`, `buildGraph:96`).
- **Cocone legs (inclusions):** `gNodes <>` (`Core.hs:157`), `gEdges` keyed by `(NodeId,NodeId)` (`Core.hs:158-159`), `gAdjFwd/gAdjBack Map.unionWith Set.union` (`Core.hs:160-163`).
- **Views (objects of 𝔻):** `UseCase/Extract/LSP.hs` (`extractFromFile:127`, `extractionFromPortSymbols:107`), `UseCase/Extract/Core.hs` (`mergeIntoRunning:123`). Each `Extraction` is one file's view.
- **Labels (Node/Edge records):** `Domain/Types/Node.hs` (`Node:120`), `Domain/Types/Edge.hs` (`Edge:88`).
- **Structural projection `Struct`:** `gNodes/gEdges/gDirected` (`Graph/Core.hs:47`).
- **Specs:** openspec `09-merge` (§Requirement "Workflow 09 — merge two knowledge graphs": dedup by NodeId, union edges, preserve directed flag; **code/prose mismatch — the spec prose says 'last-write-from-B wins' but the implementation (`Data.Map.union`) is left-biased / `old`-wins; flagged to the PO, closed by AVI-647**), `lsp-extraction` (§Requirement "Infrastructure.LSP.Extraction"), `domain-types` (§Requirement "Domain.Types.Node"/"Domain.Types.Edge"), `lsp-edge-extraction` (unique `EdgeId` per `(source,target,relation)`).

---

## 8. Acceptance Bar Met

- **Universal property with uniqueness:** LG-COLIM states the terminal-cocone property and uniqueness up to unique iso ([AVI-511](/AVI/issues/AVI-511) §3.1); LG-C2 is the operational SHALL.
- **Property tests:** cocone legs as graph morphisms (LG-C1), shared-node identity under both legs (LG-C2, the issue's criterion), fiber-product / dedup identity (LG-C3), conflict counterexample (LG-C4), structural naturality (LG-C5).
- **Boundary statement (diagram honesty):** pullback-on-labels holds ⟺ agreement; conflicts are a documented non-pullback (LG-C4).
- **Haskell-constraint statement per requirement:** §2/§3 name the constrained surface for every `LG-C-*` ID; `LG-C3` carried a required [devhaskell](/AVI/agents/devhaskell) loop ([§6 O-LG1](AVI-537-local-to-global-consistency.md#6-open-items), AVI-647) — now closed, LG-C3 locked.
- **Joint validation path:** JOINT-LG1/2/3 give the graph-theory combinatorial acceptance bar on the same colimit object; owner the Graph Theory Expert via the Head of R&D joint issue.
- **No silently dropped requirement:** all 5 `LG-C-*` rows appear in §2 with surface + acceptance; LG-C4's "skip" is an explicit boundary, not an omission.

---

## 9. Prior Art

- **Colimit = terminal cocone; pushout = colimit over the span; pullback = fiber product** — standard categorical facts (Riehl, *Categorical Topology and Logic*, Ch. 2; Borceux & Brumberg, *Category Theory*, vol. II).
- **Graph gluing via pushout / cospan semantics** (Lemmermeyer, *Graph Categories*; Freyjá, Lack & Morrison, "Cospan Semantics for Relational Systems").
- **Sheaf-style local-to-global as a colimit over a cover, with the sheaf condition as descent/colimit equality** (Mac Lane & Moerdahl, *Sheaves in Geometry and Logic*) — the reference for "local and global truth agree on overlaps."
- **Pullback as agreement / fiber product over a common codomain** — standard (e.g. Riehl, *Categorical Topology and Logic*, Ch. 2); this doc applies it to the merge square restricted to identical-label keys.
- **`Data.Map.union` left-biased (first operand wins)** — `containers` library semantics; the conflict policy ([AVI-511](/AVI/issues/AVI-511) §3.3) is a Graphos choice layered on top.

---

## 10. Determinism Lens

- **Consistent diagrams (full overlap = agreement):** the pushout square is both a pushout and a pullback on labels (LG-C3); `M` is order-independent ([AVI-511](/AVI/issues/AVI-511) §3.1), so shared nodes map identically under both legs regardless of merge order (LG-C2). Local and global truth agree on the entire overlap.
- **Conflicting diagrams:** left-biased `Data.Map.union` over-identifies the conflicting pair (LG-C4); the square is a pushout but **not** a pullback, and shared-node identity holds only on the non-conflicting (agreeing) keys. Requirement: the doc states this boundary explicitly and tests it (LG-C4 asserts the expected non-pullback).
- **Deterministic label identity on agreement:** given agreeing views, `Map.lookup k (gNodes M)` is a pure function of the inputs and equals both view labels; the cocone legs are deterministic inclusions, so LG-C2 is order-independent on the agreement region.

(End of file)
