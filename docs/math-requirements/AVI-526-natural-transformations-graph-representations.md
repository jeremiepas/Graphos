# Natural Transformations Between Graph Representations — Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-526](/AVI/issues/AVI-526) (child of [AVI-512](/AVI/issues/AVI-512), element C2)
**Status:** requirements document — grounded in the domain model + openspec specs.
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion. A reproducible `runghc` kernel (§6) is the concrete deliverable.

> **Grounding.** Every object is pinned to `src/Graphos/Domain/` or `openspec/specs/`. Graphos maintains *several* representations of "the graph" — an extraction view, a labeled graph, the core `Graph`, an FGL graph (`CachedFGL`), Aeson `Value`, and a pushed graph-database representation — produced by different functors from a common source. This doc formalizes the **natural transformations** between those representations and pins each to a surface with a checkable criterion. It complements [AVI-524](/AVI/issues/AVI-524) (functors: extraction→merge→cluster) and [AVI-536](/AVI/issues/AVI-536) (colimit of views); together the three establish "objects/morphisms → functors → natural transformations".

---

## 0. Scope and relationship to the sibling CT corpus

| Doc | What it establishes | Leaves open (this doc closes) |
|---|---|---|
| [AVI-524](/AVI/issues/AVI-524) `functoriality-context-graph` | `ℛ`-labeled graphs; extraction→merge pipeline as composite lax functor | The **natural transformations** between the *concrete* representations (FGL, JSON, push) — this doc |
| [AVI-536](/AVI/issues/AVI-536) `colimit-merged-context-graph` | `mergeGraphs` as colimit/lax-colimit; universal cocone + unique mediating morphism | How the merged `Graph` relates to its *other* encodings (FGL / JSON / push) |
| **This doc (AVI-526)** | Natural transformations `η : F ⟶ G` between representation functors; where each is an iso, where it fails, and its acceptance test | — |

**Novel contributions of this doc, each mapped to a surface:**

| Novel contribution | Concrete Graphos surface |
|---|---|
| (1) FGL conversion as a natural transformation `η : toFGL ⟶ toSeqFGL` (`fgl-adapter` spec bijectivity) | `toFGL` (`Domain/Graph/FGL.hs:58`), `nidToInt` (`FGL.hs:55`), `toCachedFGL` (`Domain/Graph/Analysis.hs:64`) |
| (2) Forgetful `Graph → LabeledGraph` loses `gDirected`; preserves `gHash`; edge re-key partial | `buildGraph` (`Core.hs:96`), `mergeGraphs` (`Core.hs:157`), `computeGraphHash` (`Core.hs:259`), `EdgeId` encoding (`UseCase/Infer.hs:330`) |
| (3) Serialization round-trip as the unit of the serialize/parse adjunction | `ToJSON`/`FromJSON Graph` (`Core.hs:64`); `graph-json-contract` spec (symmetric key sets) |
| (4) Push representation as a functorial sub-transformation (representative selection) | `pushToNeo4j`/`pushToMemgraph` (`Infrastructure/Export/Neo4j.hs:71`, `Memgraph.hs:93`); `12-neo4j-push` spec |

> **Constrains (required by the issue).** Every requirement below names one surface and carries a checkable acceptance criterion. Behavior-constraining requirements (marked 🔧) require a loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing the Haskell semantics.

---

## 1. Objects and morphisms — defined *before* any natural transformation

A natural transformation `η : F ⟶ G` requires two functors `F, G : C → D`. We therefore define the categories first; no naturality claim is made until §2.

### 1.1 The category of context graphs `Gr` (restated from [AVI-536](/AVI/issues/AVI-536) §1.1)

- **Objects.** `G : Graph` (`Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }`, `Core.hs:47`). Two graphs are identified up to the canonical iso preserving `(gNodes, gEdges, gAdjFwd, gAdjBack, gDirected)`; `gHash` is a deterministic function of `(gNodes, gEdges)` (`computeGraphHash`, `Core.hs:259`).
- **Morphisms.** `f : G → H` is a pair `(f_N : gNodes G → gNodes H, f_E : gEdges G → gEdges H)` of key-preserving structure maps preserving incidence and `edgeRelation` (`Edge.hs:32`, `Edge.hs:88`). Composition pairs of maps; identity is the key-identity.

> **Why not skip.** The natural transformations below quantify over `Gr`-morphisms; their components `η_G` must therefore be defined on objects of `Gr`, and naturality must hold for `Gr`-morphisms. Objects/morphisms precede the functors precede the transformations — no skipping levels.

### 1.2 The representation categories

Each concrete representation is an object of a category; the conversion functors land in these.

- **Labeled graphs `LCat`.** Objects `LabeledGraph = LabeledGraph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd, gAdjBack :: Map NodeId (Set NodeId) }` (`Types/Graph.hs:83`); morphisms preserve `(gNodes, gEdges, gAdjFwd, gAdjBack)`.
- **FGL graphs `FGLCat`.** Objects `FGLGraph = Gr (NodeId, Node) ((Relation, Confidence, Edge))` (`Domain/Graph/FGL.hs:51`); morphisms are FGL graph homomorphisms `(nodeRelabeling, edgeRelabeling)` preserving incidence.
- **Interchange values `ValCat`.** Objects are Aeson `Value`; the serialization functors land here (see §4).
- **Pushed representation `RepDB`.** Objects are graph-database rows/Cypher state; the push functors land here (`IO`-valued, see §5).

> **Edge-key variance (structural, constrains NT2).** `Extraction.extractionEdges :: Map EdgeId Edge` and `LabeledGraph.gEdges :: Map EdgeId Edge` use **`EdgeId`** keys, while `Graph.gEdges :: Map (NodeId, NodeId) Edge` (`Core.hs:49`) uses a **node-id pair**. The conversion `Graph → LabeledGraph` must therefore re-key edges; this is the source of partial invertibility (§3).

---

## 2. NT1 — FGL conversion is a natural transformation (the centerpiece)

### 2.1 Two functors, one natural transformation

There are two ways to encode a `Graph` as an `FGLGraph`, differing **only** in the node-indexing scheme:

- `toFGL : Gr → FGLGraph`, hashed index `nidToInt :: NodeId → Int` (`Domain/Graph/FGL.hs:55,58`).
- `toSeqFGL : Gr → FGLGraph`, sequential bijective index `0..N-1` via `toCachedFGL` (`Domain/Graph/Analysis.hs:64`; `cfgNidMap`, `cfgIdxMap`).

Both are functors `Gr → FGLCat` (a `Gr`-morphism relabels node/edge labels and hence both index schemes identically). Define **η : toFGL ⟶ toSeqFGL** with component at `G`:

> **η_G : toFGL(G) → toSeqFGL(G)** relabels each node's FGL `Int` index by `seqIndex ∘ (nidToInt)⁻¹` and each edge index by the same map; it preserves incidence because both encodings derive edges from the same `(edgeSource, edgeTarget)` pair (`FGL.hs:68`).

### 2.2 The naturality square and where it commutes

For a `Gr`-morphism `h : G → H` (node relabeling `h_N`, edge relabeling `h_E`):

```
   toFGL(G) ──toFGL(h)──▶ toFGL(H)
     │                        │
    η_G                      η_H
     ▼                        ▼
  toSeqFGL(G) ──toSeqFGL(h)▶ toSeqFGL(H)
```

**Claim (naturality).** The square **commutes**: index-relabeling (`η`) and node-id-relabeling (`h_N`) operate on *independent coordinates* of an FGL node `(intIndex, (nodeId, payload))`, so they compose in either order. Requirement: document that `η` is a natural transformation whose components are graph homomorphisms preserving `src/tgt/relation/confidence`.

### 2.3 When `η_G` is an iso — and when it fails (diagram honesty)

**Claim.** `η_G` is a graph **isomorphism** iff `nidToInt` is **injective** on `gNodes G` (two distinct NodeIds must not share an FGL `Int`). If two NodeIds collide under `nidToInt`, `toFGL(G)` collapses them to one FGL node — **data loss** — and `η_G` is undefined.

> **R-FGL1 (bijective index ⇒ natural iso).** `η_G : toFGL(G) ⟶ toSeqFGL(G)` is a **natural transformation**; its component `η_G` is an **isomorphism preserving all FGL-query structure** (articulation points, biconnected components, dominators, shortest paths) iff `nidToInt` is injective on `gNodes G`. This is exactly the `fgl-adapter` spec's **bijective node-index** requirement (`two distinct NodeIds MUST NOT share an Int index`; reverse mapping covers every index). On collision the transformation is undefined and `toFGL` loses nodes.

> **Constrains:** `nidToInt` (`FGL.hs:55`, hash mod `maxBound :: Int` — finite image ⇒ not injective in principle), `toCachedFGL` sequential indices (`Analysis.hs:64-82`, `cfgIdxMap :: Map NodeId Int`), `fgl-adapter` spec (Bijective node-index mapping; FGL-backed algorithms preserve semantics under sequential indexing). 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm `cachedFindIdx`/sequential indices are used everywhere FGL is built (not the hashed `nidToInt`).

### 2.4 Structural queries are invariant under `η` (the point of the iso)

The `fgl-adapter` spec requires `articulationPoints`, `biconnectedComponents`, `dominators` to be **equivalent up to element order** after switching to sequential indexing. Categorical reading: these are functors `FGLCat → Set` (to the set of results), and `η` being an iso implies the diagram

```
  toFGL(G) ──queries──▶ Set(results)
     │                    ‖  (iso on structure)
    η_G
     ▼
  toSeqFGL(G) ──queries──▶ Set(results)   (equal as Sets, order-independent)
```

commutes. Requirement: the sequential-index conversion is a natural iso that leaves query results unchanged **as a set** (`Set`-comparison), never as an ordered list — the fgl-adapter requirement restated.

> **Constrains:** `fgl-adapter` spec ("results equivalent ... up to element order", comparing as `Set`s); `articulationPoints`/`biconnectedComponents`/`dominators` (`Domain/Graph/Analysis.hs`).

---

## 3. NT2 — The forgetful functor `Graph → LabeledGraph`

### 3.1 Definition

Define **U : Gr → LCat** sending a `Graph` to its `(gNodes, gEdges, gAdjFwd, gAdjBack)` projection, re-keying edges `(NodeId, NodeId) → EdgeId` via `edgeId = src <> "->" <> tgt <> ":" <> relationToText (edgeRelation e)` (`UseCase/Infer.hs:330`), and dropping `gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath`.

### 3.2 What `U` preserves and what it loses (diagram honesty)

- **`gHash` is preserved.** `computeGraphHash : Map NodeId Node → Map (NodeId,NodeId) Edge → Text` (`Core.hs:259`) is a **deterministic function of structure**, so `gHash` is recoverable from `(gNodes, gEdges)`. `U` reflects it: two graphs with different `gHash` (but same structure) are distinguished by recomputation. Requirement: the round-trip `LabeledGraph → Graph` recomputes `gHash` and matches the source.
- **`gDirected` is lost.** `gDirected :: Bool` is **not** determined by `(gNodes, gEdges, gAdjFwd, gAdjBack)`: it is a build-time flag (`buildGraph :: Bool → Extraction → Graph`, `Core.hs:96`; `mergeGraphs` carries `gDirected old`, `Core.hs:157`). Two graphs identical in structure but differing only in `gDirected` map to the **same** `LabeledGraph`. Hence `U` is **not faithful** (it collapses directed vs undirected into one object). Requirement: the `LabeledGraph`/JSON round-trip must carry `directed` as an explicit field (it does: `toJSON Graph` writes `"directed"`, `Core.hs:70`) so the lost information is restored on re-instantiation — i.e. `U` has a section only when `gDirected` is persisted alongside structure.
- **Edge re-key is partial.** Recovering `(NodeId, NodeId)` from `edgeId :: Text` requires parsing `src "->" tgt ":" rel`. This is unambiguous **iff** node ids contain no `->` or `:` delimiter; otherwise the forward map `Graph → LabeledGraph` is defined but the reverse is **partial/lax**. Requirement: document the re-key as a natural transformation defined on the subcategory of delimiter-free node ids; outside it, edge re-keying is a chosen comparison (lax), not an iso.

> **R-LG1 (forgetful functor, information accounting).** `U : Gr → LCat` is a **functor**; it **preserves** `gNodes`, `gEdges` (modulo edge re-key), `gAdjFwd`, `gAdjBack`, and the derivable `gHash`; it **loses** `gDirected` (not structurally determined) and the `(NodeId,NodeId)` edge key (recovered only when node ids are delimiter-free). Requirement: every dropped field is either (a) a deterministic function of the retained structure (`gHash`), or (b) explicitly persisted (`directed`), so that `Graph → LabeledGraph → Graph` is isomorphic to the source.

> **Constrains:** `buildGraph` (`Core.hs:96`, `Bool` directed arg), `mergeGraphs` (`Core.hs:157`, `gDirected old`), `computeGraphHash` (`Core.hs:259`), `EdgeId` encoding (`UseCase/Infer.hs:330`), `LabeledGraph` (`Types/Graph.hs:83`). 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm the JSON `directed` field round-trips and that no node id can contain `->`/`:` in practice (node-id generator).

---

## 4. NT3 — Serialization round-trip as the unit of an adjunction

### 4.1 The serialize/parse pair

`Serialize : Gr → ValCat`, `toJSON Graph` (`Core.hs:64`), writes exactly the keys `"nodes","edges","adj_fwd","adj_back","directed","compositions","hash"` and, conditionally, `"embeddings_path"` (`Core.hs:71-76`). `Parse : ValCat → Gr`, `FromJSON Graph` (`Core.hs:78-88`), reads them back; `gEmbeddings` is hard-parsed as `Nothing` (`Core.hs:87`) and reloaded from the sidecar.

### 4.2 The unit `η : Id ⟶ Parse ∘ Serialize`

The **unit** of the serialize/parse adjunction is `η_G : G → Parse(Serialize(G))`.

**Claim.** `η_G` is an **isomorphism** on every field except `gEmbeddings`/`gEmbeddingsPath`, which are **transient** (loaded from a sidecar, always `Nothing` after parse). Hence `Parse ∘ Serialize ≅ Id` on `(nodes, edges, adj_fwd, adj_back, directed, compositions, hash)` — the **round-trip** invariant.

> **R-SER1 (round-trip = natural iso on persisted structure).** `Parse ∘ Serialize ⟶ Id` is a **natural transformation** whose component `η_G` is an **isomorphism** on all persisted fields; it is **not** an iso on the transient `gEmbeddings` (reloaded from sidecar, per `graph-json-contract`). Requirement: every key written by the exporter is read back (`graph-json-contract`: "reader and writer key sets are symmetric"; "community_aggregates round-trips"), and the only structural non-preservation is the documented transient embeddings.

> **Constrains:** `ToJSON`/`FromJSON Graph` (`Core.hs:64-88`); `graph-json-contract` spec ("Reader and writer key sets are symmetric"; "Unknown enum values degrade"; "Optional node fields"). 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm the exported key set equals the loader's required key set for the current `Graph` record (the `embeddings_path` conditional is the one asymmetry).

---

## 5. NT4 — Push representation as a functorial sub-transformation

### 5.1 The push functors

`pushToNeo4j :: Graph → Text → Text → Text → IO (Text, Int, Int)` (`Infrastructure/Export/Neo4j.hs:71`) and `pushToMemgraph :: Graph → ... → IO (...)` (`Memgraph.hs:93`) encode a `Graph` as parameterized Cypher statements in a graph-database representation. Both are functors `Gr → RepDB` (a `Gr`-morphism relabels nodes/edges, and the statement generation `generateParameterizedStatements` relabels variables structurally).

### 5.2 Representative selection as a natural sub-object

The subgraph push mode selects **≤7 representatives per community** — centroid (highest degree) + top-N by degree + bridges (articulation points) + entry points (file nodes) — marking them `representative=true` (`12-neo4j-push` spec; `Neo4j.hs:401-407`). This is a **natural transformation** `Sub ⊆ Full` selecting a canonical sub-object per community.

> **R-PUSH1 (push is functorial; representative selection is canonical).** The push conversion `Gr → RepDB` is a **functor** commuting with `Gr`-morphisms (node/edge relabeling ⟷ variable binding); representative selection is a **natural sub-transformation** `Sub ⊆ Full` choosing a canonical ≤7-representative sub-object per community. Requirement: statements are **parameterized** (`{$param}` syntax, no string interpolation — the morphism is structural, not syntactic; `12-neo4j-push` spec) and push preserves node/edge incidence.

> **Constrains:** `pushToNeo4j`/`generateParameterizedStatements` (`Infrastructure/Export/Neo4j.hs:71`), `pushToMemgraph` (`Memgraph.hs:93`); `12-neo4j-push` spec (parameterized Cypher; ≤7 representatives/community). 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm parameterization holds across all three entity types (Node, Community, BELONGS_TO) and that representative selection is deterministic.

---

## 6. Concrete deliverable — reproducible `runghc` kernel

`docs/math-requirements/kernels/AVI-526-fgl-naturality.hs` empirically verifies NT1 (§2) against the runtime `containers`/`text` semantics the property tests run against:

```
-- nidToInt image is bounded Int (maxBound = 9223372036854775807); injective? NO in principle
-- concrete nidToInt collision found over 8x8 id space? False
-- sequential 0..N-1 bijective over same space? True
-- toFGL node count under forced collision (hashed idx shared)? 1
-- toSeqFGL node count (distinct sequential idx)? 2
```

Interpretation:
- **nidToInt is a hash into a bounded `Int`** → not injective in principle; the component `η_G` of R-FGL1 is therefore *conditionally* defined (only when no two NodeIds collide). This is the categorical statement of why the `fgl-adapter` spec mandates **bijective** indexing.
- **sequential `0..N-1` is bijective by construction** → `η_G` is an iso for every `Graph`.
- **forced collision**: when two NodeIds share a hashed index, `toFGL` merges them into one FGL node (data loss: node count `1`), while `toSeqFGL` keeps them distinct (`2`). This is the failure mode of `η_G` when the injectivity precondition fails.

---

## 7. Acceptance criteria — checkable property tests (Graphos-Dev implement)

> **AC-1 (FGL natural iso, injective case).** For a `Graph` whose NodeIds are `nidToInt`-injective, assert `η_G : toFGL(G) ≅ toSeqFGL(G)` is a structure-preserving isomorphism: same node count, same edge incidence, and `articulationPoints`/`biconnectedComponents`/`dominators` equal **as sets** before and after switching index schemes. *Grounding:* NT1 §2.2-2.4.

> **AC-2 (FGL data loss under collision).** Construct a `Graph` whose NodeIds collide under `nidToInt` (two distinct ids with equal `nidToInt`); assert `toFGL` merges them (node count drops) while `toSeqFGL` preserves both, and that `cachedFindIdx` returns `Just` for both only under sequential indexing. *Grounding:* NT1 §2.3; `fgl-adapter` spec (bijective node-index).

> **AC-3 (forgetful `U`, information accounting).** For a `Graph`, assert `gHash` recomputes from `(gNodes, gEdges)` after `U`; assert two graphs differing only in `gDirected` map to the same `LabeledGraph` (U not faithful); assert the JSON `"directed"` field round-trips so the section restores it. *Grounding:* NT2 §3.

> **AC-4 (serialization round-trip).** For a clustered `Graph`, assert `Parse(Serialize(G))` equals `G` on `(nodes, edges, adj_fwd, adj_back, directed, compositions, hash)`; assert the only non-preserved field is the transient `gEmbeddings` (reloaded from sidecar = `Nothing`). *Grounding:* NT3 §4.2; `graph-json-contract`.

> **AC-5 (push functoriality + representative selection).** Assert push statements use parameterized `{$param}` syntax (no string interpolation); assert ≤7 representatives per community; assert node/edge incidence is preserved by the push. *Grounding:* NT4 §5; `12-neo4j-push` spec.

---

## 8. Requirements traceability table

| Req | Statement | Surface | Acceptance | Locked? |
|---|---|---|---|---|
| R-FGL1 | `η : toFGL ⟶ toSeqFGL` natural; `η_G` iso iff `nidToInt` injective (§2.3) | `FGL.hs:nidToInt`, `Analysis.hs:toCachedFGL`, `fgl-adapter` spec | AC-1, AC-2 | structural |
| R-LG1 | forgetful `U : Gr → LCat`; preserves `gHash`, loses `gDirected`, edge re-key partial (§3) | `Core.hs:96,157,259`, `Infer.hs:330`, `Types/Graph.hs:83` | AC-3 | 🔧 loop w/ Dev |
| R-SER1 | `Parse ∘ Serialize ≅ Id` on persisted fields; transient embeddings (§4) | `Core.hs:64`, `graph-json-contract` spec | AC-4 | structural |
| R-PUSH1 | push `Gr → RepDB` functorial; representative selection canonical (§5) | `Neo4j.hs:71`, `Memgraph.hs:93`, `12-neo4j-push` spec | AC-5 | 🔧 loop w/ Dev |

---

## 9. Prior art

- **Natural transformations / functor categories** — the standard reference: Mac Lane, *Categories for the Working Mathematician*, Ch. III (§ naturural transformations); Riehl, *Categorical Logic and Topology*, Ch. 3.
- **Adjunctions and the unit `Id ⟶ R∘L`** — Mac Lane Ch. IV; the serialize/parse round-trip (§4) is the unit of an insertion/forgetting adjunction between structure and its interchange form.
- **Representations of a category as functors (Yoneda)** — the claim that "a graph = its encodings" rests on the principle that structure is what its functors preserve (Mac Lane, Ch. V; the natural transformation is the invariant).
- **Bijective hashing / collision-free indexing for graph algorithms** — the `fgl-adapter` spec's switch from hashed `nidToInt` to sequential indexing; standard in graph-library adapters (avoiding hash collisions in node-index mapping).
- **Parameterized query builders (injection-safe morphisms)** — the push requirement that statements are parameterized (`{$param}`), not string-interpolated: the morphism is structural, not syntactic (standard prepared-statement practice).

---

## 10. Determinism lens

- **FGL index determinism (NT1):** the sequential index is a **canonical** function of the canonical node order (`Map.toList (gNodes g)`, `Analysis.hs:66-70`), so `toSeqFGL(G)` is representation-independent (order-independent over `Map`), while `toFGL(G)` depends only on `nidToInt` (deterministic hash). Both are deterministic; they agree up to `η_G` when injective.
- **Forgetful-functor determinism (NT2):** `U` is deterministic; the only non-determinism risk is edge re-key ambiguity if node ids contain delimiters — a documented partial case (§3.2).
- **Serialization determinism (NT3):** `toJSON Graph` writes `Map`s in `Data.Map` canonical (sorted) order, so the serialized form is order-independent; the round-trip iso holds regardless of insertion order.
- **Push determinism (NT4):** representative selection must be deterministic (canonical argmax tie-break on degree) so the sub-object is well-defined; 🔧 confirm with [Graphos Dev](/AVI/agents/graphos-dev).

---

## 11. Math-judgment / escalation log

| # | Decision | Rationale / source |
|---|---|---|
| M-1 | `η : toFGL ⟶ toSeqFGL`; component iso iff `nidToInt` injective | `fgl-adapter` spec (bijective index) + empirical kernel (§6). The natural transformation is between the two index-scheme functors, not a universal property — it is the object of this issue. |
| M-2 | `U : Gr → LCat` is **not faithful** (collapses `gDirected`) | `gDirected` is a build-time flag (`Core.hs:96,157`), not structurally determined — honest diagram claim, not invented failure. |
| M-3 | edge re-key `Graph → LabeledGraph` is **partial** (delimiter-free node ids only) | `edgeId = src "->" tgt ":" rel` (`Infer.hs:330`); reverse parse ambiguous otherwise. |
| M-4 | serialization round-trip iso on persisted fields, not on transient `gEmbeddings` | `FromJSON` hard-parses `gEmbeddings = Nothing` (`Core.hs:87`); sidecar reload. Honest non-preservation. |
| M-5 | push representative selection is a **canonical** sub-object (≤7/community) | `12-neo4j-push` spec; requires deterministic argmax (GATE w/ Dev). |
| M-6 | NT4 is the weakest claim (analogy to sub-object) | Included because push *is* a real representation surface; scope held to parameterization + incidence preservation. Escalate if [Graphos Dev](/AVI/agents/graphos-dev) disputes functoriality. |

---

## 12. Next Actions (Graphos-Dev owns the concrete next step)

1. **Property tests (AC-1, AC-2)** — delegate to [Graphos Dev](/AVI/agents/graphos-dev): FGL natural iso under injective indexing (AC-1); collision-detection + data-loss under `nidToInt` (AC-2). The reproducible kernel (§6) is the verification-ready core.
2. **Round-trip test (AC-3, AC-4)** — assert `gHash` recomputation, `directed` round-trip, and `Parse∘Serialize ≅ Id` on persisted fields; confirm no node id can contain `->`/`:` (else NT2 re-key becomes lax). 🔧 loop with Dev.
3. **Push test (AC-5)** — parameterization + ≤7 representatives/incidence. 🔧 loop with Dev.
4. **Escalate** any category-level judgment above confidence to the Head of R&D (per execution contract); e.g., M-6 whether the push representation is genuinely functorial vs. merely representable.
