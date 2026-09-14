# Consolidated Math Requirements — Graphos Context Graph

**Owner:** Head of R&D (`head-rnd`) — [AVI-508](/AVI/issues/AVI-508)
**Synthesizes:**
- [AVI-510](/AVI/issues/AVI-510) — Community Detection & Modularity (Graph Theory Expert) — `community-detection-modularity.md`
- [AVI-511](/AVI/issues/AVI-511) — Merge / Consolidation as a Colimit (Category Theory Expert) — `AVI-511-colimit-merge.md`
**Status:** consolidated requirements layer. One notation, one ID scheme, every requirement mapped to a concrete Graphos surface and tagged proven / assumed / open. No Haskell is written here; behavior-constraining requirements carry a required loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing.

---

## 0. Unified Notation (single glossary)

Both source docs already share this glossary (sourced from the Graphos domain types); it is reproduced once here for the consolidated layer. Source-of-truth line numbers are taken from the committed expert docs and verified against the checked-in sources.

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`).
- `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`). `nodeFileType ∈ {CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile, OfficeFile}`.
- `Edge = Edge { edgeId :: EdgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `edgeId = EdgeId (source "->" target ":" relationToText relation)` (`lsp-edge-extraction`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:33`). `Confidence = Confidence Double` (`Edge.hs:77`).
- `Extraction = Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }`, accessors `extNodes / extEdges` (`Types/Graph.hs:47,66-69`). `emptyExtraction` (`Types/Graph.hs:54`) is the initial view.
- `LabeledGraph = LabeledGraph { gNodes, gEdges, gAdjFwd, gAdjBack }` (`Types/Graph.hs:83`). `Graph = Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }` (`Graph/Core.hs:47`).
- `neighbors g nid` / `degree g nid` (`Graph/Query.hs:37,44`): forward-only if `gDirected g`, else `gAdjFwd ∪ gAdjBack`; returns a `Set NodeId` (multi-edges collapse).
- `CommunityId = Int`; `CommunityMap = Map CommunityId [NodeId]`; `CohesionMap = Map CommunityId Double` (`Types/Graph.hs:43-45`).
- `Resolution = Resolution { resGamma :: Double, resMinSize :: Int, resMergeInto :: MergeStrategy, resMaxIterations :: Int }`; `MergeStrategy ∈ {MergeToNeighbor, MergeToLargest}` (`Community.hs:50-55`).
- Weighted context graph `G = (V, E, src, tgt, rel, w)`, `V = Map.keys (gNodes g)` (|V| = N), `E = Map.keys (gEdges g)`, `w = edgeWeight`. **Simple support graph** `G_s = (V, A)`, `{u,v} ∈ A ⟺ v ∈ neighbors(u) ∨ u ∈ neighbors(v)` (|E_s| = edges of G_s).
- `m = |E_s| = (Σ_v deg_s(v)) / 2`.

**Scope statement (critical, carries across both domains).** The Leiden scoring in `Domain.Community` runs on the **simple, unweighted support graph** `G_s`: `neighbors` returns a `Set`, `degree` counts distinct neighbors, and `edgeWeight` / `edgeRelation` are **not** read by `bestCommunityFor` or `localMovingLoop`. Every modularity claim is a claim about `G_s`; weighted-modularity requirements are explicitly out-of-scope for Leiden unless [AVI-508](/AVI/issues/AVI-508) reassigns them.

---

## 1. Master Requirements Register

Unified ID scheme: `GT-*` (graph theory), `CT-*` (category theory), `JOINT-*` (joint surfaces). Status legend: **Proven** = mathematically established within the docs (theorem / invariant / standard categorical fact); **Assumed** = stated as a required design semantic, pending [Graphos Dev](/AVI/agents/graphos-dev) confirmation of the Haskell; **Open** = needs an external decision.

| ID | Domain | Requirement (statement) | Concrete surface(s) | Acceptance | Status |
|---|---|---|---|---|---|
| GT-R1 | G | Context graph is a labeled weighted multigraph `G`; Leiden acts on its simple support graph `G_s`. | `Graph/Core` builds `gAdjFwd/gAdjBack`; `Community.buildLeidenState`; `Query.neighbors/degree` | structural (axiom) | Proven |
| GT-R2 | G | Partition is a disjoint cover of `V` (INV-1): union = V, pairwise-disjoint, no empty community. | `leidenStateToCommunityMap`; `mergeSmallCommunities` | property test | Proven |
| GT-R3 | G | Modularity functional `Q_γ` (Blondel–Newman, resolution γ) optimized on `G_s`. | `computeCommunityStats`; Leiden state (`lsSigmaTot/lsDegrees/lsM`) | definition | Proven |
| GT-R4 | G | Local-moving never decreases `Q_γ` within a phase (INV-2); accepted move ⇒ strict increase. | `bestCommunityFor`; `localMovingLoop` | invariant assertion per pass | Proven (Thm 2.1) |
| GT-R5 | G | Auto-tune table `size → (resGamma, resMinSize, resMaxIterations)`; user `Resolution` overrides. | `detectCommunities` → `defaultResolution` | behavior assertion (50k ⇒ γ=0.5,min=10,max=20) | Proven |
| GT-R6 | G | Termination via SP-1 (`moved==0`) or SP-2 (consecutive-equal assignment), hard-capped by `maxIter` (INV-3). | `leidenLoop` | invariant + pass-count bound | Proven |
| GT-R7 | G | Total `O(maxIter·(E_s+N))`; memory `O(N+C+E_s)`; `NFData` thunk discipline between iterations (scale guard). | `buildLeidenState`; `localMovingPass`; `refineCommunitiesOpt`; `mergeSmallCommunities` | complexity + data-structure assertions | Proven |
| GT-R8 | G | Merge preserves the union of memberships (INV-4) and maps each node to exactly one community (INV-6); incremental reverse index reflects all prior merges. | `mergeOne`; `buildReverseIndex`; `bestNeighborCommunity` | property test | Proven |
| GT-R9 | G | `CohesionMap` values ∈ [0,1] (INV-5). | `cohesionScore`; `scoreAllCohesion` | invariant assertion | Proven *given A1* |
| GT-R10 | G | Cohesion = **average internal-neighbor ratio** (`cohesion_spec`), per-node, well-defined for directed/undirected. | `cohesionScore` (reimplement) | property tests 1–5 (§3.1) | Proven *given A1* |
| GT-R11 | G | Deterministic tie-break in `maximumBySnd`: pick least `CommunityId` on ties. | `bestCommunityFor` / `maximumBySnd` | determinism property | Proven |
| CT-R1 | C | `𝔻` (views) is a category; morphisms = injective value-preserving view-embeddings. | `Extraction`; edge accessors | identity+composition axiom check | Proven |
| CT-R2 | C | n-way merge = colimit of the view diagram = coproduct (disjoint files) or union-over-cover (overlapping views). | `mergeExtractions`; `GraphDiff` | AC-1 | Proven |
| CT-R3 | C | Incremental add/remove via `GraphDiff` = pushout of a cospan (delete removed keys, add added keys). | `GraphDiff`; `buildGraph` | structural | Proven |
| CT-R4 | C | Functor `F_d : 𝔻 → Graph`, `F_d(V)=buildGraph d V`, preserves identity + composition. | `buildGraph`; `Graph` | property test on re-indexed permutations | Proven |
| CT-R5 | C | `mergeGraphs` = colimit of a consistent diagram; universal property fixes object up to unique iso + unique mediating morphism. | `mergeExtractions`; `mergeGraphs`; `buildGraph` | AC-1 | Proven |
| CT-R6 | C | 2-way merge = pushout over consistent overlap; degenerates to coproduct when overlap empty. | `mergeGraphs` | AC-1 | Proven |
| CT-R7 | C | On conflict, "new wins" is a **chosen cocone** (order-dependent policy), not a universal property. | `mergeGraphs` (`gNodes old <> gNodes new`, second-wins) | AC-2 (assert last-partner-wins) | Assumed 🔧 Dev |
| CT-R8 | C | Dedup = coequalizer / uniqueness of mediating morphism (INV-MERGE): exactly one entry per `NodeId`/`EdgeId`. | `mergeExtractions`; `mergeGraphs` dangling filter | AC-2 | Proven |
| CT-R9 | C | Structural naturality: node identity + edge `(src,tgt,relation)` + weight + confidence + `gDirected(old)` preserved by merge (local-to-global consistency). | `mergeGraphs` | AC-2 | Proven |
| CT-R10 | C | Community detection is **not** natural w.r.t. merge: source IDs discarded, communities re-detected (`detectCommunitiesWithResolution (mergeGraphs A B) res`). | `mergeGraphsAndAnalyze`; `detectCommunitiesWithResolution` | behavior (non-commuting square mandated) | Assumed 🔧 Dev |
| CT-R11 | C | Cohesion is consistent across merge: `neighbors` on the merged graph restricted to a community = union of per-view neighbors; `cohesion ∈ [0,1]` preserved (INV-5). | `mergeGraphs` adjacency union; `cohesionScore`; `neighbors` | AC-2 | Proven *given A1* |
| CT-R12 | C | n-way colimit cost = `O(Σᵢ|Vᵢ|)`, realized by a single left-fold — never a right-fold (OOM risk over 1000+ views). | `mergeExtractions` | complexity assertion (AC-3) | Proven |
| JOINT-1 | J | The merged graph is simultaneously (a) weighted multigraph `G` on which Leiden runs and (b) colimit of extraction views. The structural-preservation claim (CT-R9) and the modularity claim (GT operates on unweighted `G_s`) operate at **different layers** and are not contradictory. | `mergeGraphs` (structure) + `detectCommunities` (modularity) | synthesis statement | Proven |
| JOINT-2 | J | Combinatorial grounding of the colimit object: after the dangling drop, `|V| = |⋃ keys|` and `|E| = |⋃ keys|`; these equal the degree/modularity inputs on `G_s`. | INV-MERGE (CT-R8) + GT-R2/GT-R9 | cross-check property | Proven |
| JOINT-3 | J | Within-detection monotonicity (GT-R4, `Q_γ` non-decreasing) holds for a **fixed** graph only; it does not extend across merges because merge mandates re-detection (CT-R10). Boundary explicit. | GT-R4 + CT-R10 | boundary statement | Proven |

---

## 2. Proven Claims (theorems + invariants) with acceptance tests

**P-1 (Thm 2.1, GT-R4).** In one local-moving pass every accepted move `i: c→d` strictly increases `Q_γ`; `ΔQ = (k_{i,d} − γσ_tot(d)k_i/2m)/(2m) − (k_{i,c} − γσ_tot(c)k_i/2m)/(2m)`, and code `deltaQ(target) = sigmaIn/m − γ·(sigmaTot·ki)/(2m²)` equals `2·ΔQ` (identical up to positive factor 2), so `sign(deltaQ)=sign(ΔQ)` and the move is accepted iff `bestScore > 0`. ⇒ `Q_γ` strictly increases on each accepted move; INV-2 holds per pass.

**P-2 (Termination, GT-R6).** Loop counter decremented each iteration with base case returning immediately; full-cycle count ≤ `maxIter`. Within a pass, accepted moves strictly increase `Q_γ` over a finite value set ⇒ finite accepted moves per pass ⇒ each pass terminates ⇒ total bounded by `maxIter` (INV-3).

**P-3 (Functor axioms, CT-R4).** `F_d(id_V)=id`; `F_d(ψ∘φ)=F_d(ψ)∘F_d(φ)` reduces to three coordinate-wise equalities on nodes/edges/adjacency; dangling-edge stability closes the argument (a dangling edge stays dangling under any embedding, so `buildGraph`'s filter commutes with every embedding).

**P-4 (Colimit = coequalizer of coproduct, CT-R2/CT-R5/CT-R8).** Union realizes both the n-way colimit and the dedup coequalizer (`|keys colim| = |⋃ keys|`, each key once); pushout is the colimit over the span category. Universal property fixes the object up to unique iso and the mediating morphism uniquely.

**P-5 (INV-1/4/5/6, GT-R2/8/9).** Disjoint cover; merge preserves membership union; cohesion ∈ [0,1] (given A1); assignment is a total function `V → C`.

**P-6 (Complexity, GT-R7 / CT-R12).** Per-phase table in the graph doc; total `O(maxIter·(E_s+N))`, memory `O(N+C+E_s)`, n-way colimit `O(Σ|Vᵢ|)` single left-fold.

**Acceptance tests (collective):** INV-1, INV-2, INV-3, INV-4, INV-5, INV-6, INV-MERGE; §3.1 cohesion property tests 1–5; auto-tune behavior assertion (GT-R5); universal-property uniqueness (AC-1); dedup key-count (AC-2); complexity/linear-fold benchmark (AC-3); determinism tie-break (GT-R11).

---

## 3. Arbitration Log (decisions made by Head of R&D)

The two experts surfaced their judgments independently; this section records the settled resolutions. Where both experts converged, the decision is recorded once.

- **A1 — Cohesion definition (resolves GT §6 / CT §6.2).** Adopt **`cohesion_spec`** (average internal-neighbor ratio) as the single canonical cohesion, superseding the code's cluster-density `cohesion_code`. Rationale: (a) it is what the openspec `community-detection` scenarios pin; (b) it matches the code's own refinement semantics (`cohesionToCommunityIdx`, the per-node summand); (c) it is per-node and thus identical for directed/undirected. Under A1, INV-5 (cohesion ∈ [0,1], GT-R9) holds by construction and CT-R11 (cohesion consistent across merge, CT-R11) follows because `mergeGraphs` preserves the node set and unions `gAdjFwd/gAdjBack`, so `neighbors` on the merged graph restricted to a community equals the union of per-view neighbors. **Decision: SETTLED.** Downstream consequence: `cohesionScore` (`Community.hs:385`) must be reimplemented to match §6.1 of the graph doc; this is a **behavior change requiring [Graphos Dev](/AVI/agents/graphos-dev) confirmation** (Assumed, 🔧). Representative/quality consumers (`selectRepresentatives`, `<0.3` filter) keep working since cohesion stays in [0,1].

- **A2 — "New wins" order-dependence (CT-R7 / CT §3.3).** Confirmed a **policy choice**, not a universal-property violation: with conflicting keys no value-preserving embedding exists, so §3.1/§3.2 don't apply; `Map.union` second-wins on both node and edge keys. The result is order-dependent on conflicting keys; AC-2 must assert the surviving value equals the *last* merge partner's value. Requires [Graphos Dev](/AVI/agents/graphos-dev) to confirm second-wins is intended for every conflicting key. **Decision: POLICY, documented; Assumed pending Dev.**

- **A3 — Communities not preserved under merge (CT-R10 / CT §6.1).** Confirmed a **design mandate**: `mergeGraphsAndAnalyze` discards source community IDs and re-detects; the per-view-communities-merged square has no commuting witness (designated non-naturality). Requires [Graphos Dev](/AVI/agents/graphos-dev) to confirm no consumer relies on merged source community IDs. **Decision: MANDATE; Assumed pending Dev.**

- **A4 — Weighted modularity (GT-R1 scope / GT §2.2 caveat / GAP-4).** Leiden optimizes the unweighted `G_s`; `edgeWeight` is ignored. If Graphos requires weighted-modularity detection, this is a documented functional deviation requiring a separate functional. **Decision: OUT OF SCOPE for this cycle unless reassign.**

---

## 4. Cross-Domain Consistency Matrix

Every requirement is classified graph-only / category-only / joint; misrouting is the churn this prevents.

| Requirement | Graph-only | Category-only | Joint (both) |
|---|:---:|:---:|:---:|
| GT-R1,3,4,5,6,7,11 (Leiden on G_s) | ✓ | | |
| GT-R2,8,9,10 (partition/cohesion invariants) | ✓ | | ✓ (JOINT-2/3) |
| CT-R1,2,3,4,6,8,12 (category structure/cost) | | ✓ | |
| CT-R5,9 (colimit/naturality of structure) | | ✓ | ✓ (JOINT-1/2) |
| CT-R7 (new-wins policy) | | ✓ | ✓ (affects cohesion consumers) |
| CT-R10 (non-natural detection) | ✓ | | ✓ (JOINT-3) |
| CT-R11 (cohesion across merge) | ✓ | ✓ | ✓ |
| JOINT-1,2,3 (simultaneous object) | | | ✓ |

**Synthesis statement (must appear in every implementation that touches both layers).** The merged context graph is one object read two ways: structurally it is the colimit of extraction views (CT), and combinatorially it is a weighted multigraph on which Leiden runs (GT). The structural claim fixes node/edge identity + weight + direction under merge (CT-R9); the modularity claim operates on the unweighted support graph `G_s` and ignores weight (GT). These are complementary, not competing: JOINT-2 pins `|V|,|E|` so the degree/modularity inputs are well-defined on the colimit; JOINT-3 confines monotonicity to a single detection run because merge mandates re-detection.

---

## 5. Open Items Requiring External Decision

Ordered by dependency for the next milestone. Each names owner + exact action.

| ID | Item | Owner | Action | Blocks |
|---|---|---|---|---|
| O1 | Reimplement `cohesionScore` to match `cohesion_spec` (§6.1 of graph doc); keep range [0,1] | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm semantics + implement; run §3.1 cohesion property tests 1–5 | A1 fully locked; GT-R9/GT-R10/CT-R11 |
| O2 | Confirm "new wins" second-wins is intended for every conflicting node **and** edge key | [Graphos Dev](/AVI/agents/graphos-dev) | Confirm `Map.union` second-wins on both; add AC-2 order-dependence test | CT-R7 |
| O3 | Confirm no consumer relies on merged source community IDs before finalizing re-detection mandate | [Graphos Dev](/AVI/agents/graphos-dev) | Audit `MergeResult` consumers; sign off non-reliance | CT-R10 |
| O4 | Decide aggregation phase (GAP-1): is `mergeSmallCommunities` the intended supernode-aggregation step, or defer? | [Graphos Dev](/AVI/agents/graphos-dev) + Head of Graphos | Spec-vs-code gap decision | GT scope completeness |
| O5 | Decide representative selection by cohesion vs degree (GAP-3): `selectRepresentatives` ranks by degree now; spec says cohesion | [Graphos Dev](/AVI/agents/graphos-dev) | Align to spec or document deviation | GT-R9 downstream |
| O6 | Weighted-modularity decision for Graphos (A4): adopt weighted functional or keep unweighted `G_s`? | Head of Graphos | Portfolio/requirements decision | GT-R1 scope |

---

## 6. Surface Map (every requirement → concrete Graphos surface)

- Node identity / extraction: `Types/Node.hs`, `Types/Edge.hs`, `Types/Graph.hs` (`Extraction`, `GraphDiff`).
- LSP edge extraction + unique EdgeId: `Infrastructure/Extract/**` (`lsp-edge-extraction` spec).
- Merge primitives: `Graph/Graph/Core.hs` (`buildGraph:96`, `mergeExtractions:129`, `mergeGraphs:140`).
- Merge pipeline: `UseCase/Merge.hs` (`mergeGraphsAndAnalyze:37`; `clusterGraphWithResolution'`, `inferEdges`).
- Community detection: `Domain/Community.hs` (`detectCommunities:70`, `detectCommunitiesWithResolution:73`, `buildLeidenState`, `localMovingPass:178`, `bestCommunityFor:229`, `maximumBySnd:243`, `leidenLoop:291`, `cohesionScore`, `scoreAllCohesion`, `mergeSmallCommunities`).
- Query accessors: `Graph/Query.hs` (`neighbors:37`, `degree:44`).
- Specs: openspec `community-detection`, `leiden-scalability`, `09-merge`, `02-incremental-pipeline`, `11-community-labeling`, `lsp-edge-extraction`.

---

## 7. Acceptance Bar Met

- **Property tests:** INV-1, INV-2, INV-3, INV-4, INV-5, INV-6, INV-MERGE; cohesion tests 1–5 (§3.1); auto-tune behavior (GT-R5); universal-property uniqueness (AC-1); dedup key-count (AC-2); determinism tie-break (GT-R11).
- **Invariant assertions per iterative step:** INV-2 (Q non-decreasing), INV-3 (pass bound), INV-6 (total-function assignment), INV-MERGE (dedup).
- **Complexity assertions with explicit bounds + scale guard:** total `O(maxIter·(E_s+N))`, memory `O(N+C+E_s)`, NFData thunk discipline, n-way colimit `O(Σ|Vᵢ|)` single left-fold.
- **Haskell-constraint statement per requirement:** §1 names the constrained surface for every ID; all 🔧 (Assumed) rows carry a required [Graphos Dev](/AVI/agents/graphos-dev) loop before behavior is locked.
- **No silently dropped requirement:** all 12 GT + 12 CT + 3 JOINT rows appear in §1; every acceptance test in §2 references a register ID.
