# Consolidated Math Requirements — AVI-563: fixing `graphos query` ergonomics

**Owner:** Head of R&D (`head-rnd`) — [AVI-563](/AVI/issues/AVI-563) (consolidates [AVI-565](/AVI/issues/AVI-565) GT + [AVI-566](/AVI/issues/AVI-566) CT)
**Status:** consolidated requirements layer. One notation, one ID scheme, every requirement mapped to a concrete Graphos surface and tagged proven / assumed / open. No Haskell is written here; behavior-constraining requirements carry a required loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing.
**Sources (verifications against the checked-in source):**
- Graph-theory unit: `docs/math-requirements/AVI-565-query-output-size-canonicality-store-split.md` (issue AVI-565) — REQ-O1..O5, REQ-C1..C4, REQ-S1..S4.
- Category-theory unit: `docs/math-requirements/AVI-563-query-ergonomics-category.md` (issue AVI-566) — R-Q1, R-Q1a, R-Q2, R-Q3, JOINT-Q.
**Consolidated IDs introduced here:** `GT-Q1..GT-Q3`, `CT-Q1..CT-Q3`, `JOINT-Q` (see master register, [AVI-508](/AVI/issues/AVI-508) §1).

---

## 0. Unified notation (reuse — no new names)

All symbols are taken verbatim from the shared Graphos glossary in [AVI-508](/AVI/issues/AVI-508) §0 / [AVI-563-query-ergonomics-category.md](AVI-563-query-ergonomics-category.md) §0. Consolidation introduces **no** new notation; the single renames made for consistency are:
- Plan alias `κ` (canonicality in the plan §2) is bound to the GT score **`rel : Node → [0,1.1]`** defined by AVI-565 §1.2 / §3. `κ` is **not** used as a symbol elsewhere; every occurrence reads `rel`.
- Plan alias `π : G ↦ G_st` (store projection, plan §2 GT-Q2) is bound to the CT functor **`P : Gr → Gr`** (AVI-566 §1.1). `π` is an alias for `P`; `π(G) = P(G)`.

Shared symbols (for reference; source lines in `src/Graphos/Domain/`):
- `NodeId = Text` (`Node.hs:46`); `Node` with the 12 canonical fields (`Node.hs:120`); `Edge` (`Edge.hs:88`); `Graph` (`Graph/Core.hs:47`); `Extraction`/`LabeledGraph` (`Types/Graph.hs:47,83`); `mergeExtractions`/`mergeGraphs` (`Core.hs:128,140`); `rel` canonicality/relevance score (`Score.hs:166,190` used in `Query.hs:149,178,221`); token estimator `κ_tok(P) = countContextTokens(P)` (`FormatContext.hs:12,50-127`).

Scope statement (carries across GT/CT): the scoring surfaces read **labels, source paths, and degree**. They do **not** read `edgeWeight`/`edgeRelation`/`nodeSignature` (except where `nodeSignature` is classified as content under CT-Q3 / M-4). Weighted-modularity claims remain governed by [AVI-508](/AVI/issues/AVI-508).

---

## 1. Problem — three failures, one root cause

| ID | Failure (symptom) | Math content required | Unit |
|---|---|---|---|
| F1 | Output explosion: default budget ⇒ 114 165 lines; `--json --budget 60000` ⇒ 2.16 GB output. | Hard output-size cap + deterministic budget cut | GT-Q1 |
| F2 | Every call reloads the 4.5 GB `graph.json` (~1.5 min) because nodes carry full source text. | Topology/content store split (complexity/scale guards) | GT-Q2 + CT-Q1 |
| F3 | Top hit was a test wrapper (`logger-wrapper.test.ts`), not the canonical logger definition — relevance ≠ canonicality. | Canonicality scoring on the graph (definitions vs wrappers/tests) | GT-Q3 |

Root cause: the graph stores **full source text per node**. Outputs (F1/F2/F3) are corrected by making retrieval a **structure-preserving projection** (`P`) onto a **slim** store, ranked by a deterministic score that separates relevance from canonicality.

Symptom-level surfaces already exist and are **not** duplicated here: `enforce-query-token-budget` (slim lists + hard cap), `jgf-graph-serialization` (versioned format), `atomic-graph-output-writes`, `fix-runtime-ram-crash`. This doc supplies the **formal requirements** those changes must satisfy, plus F3, which had no active surface before AVI-563.

---

## 2. GT-Q1 — Query output-size contract (F1)  `[AVI-565 §2, §3]`

**Statement.** For every rendered response `P` and budget `B ∈ ℕ`:
1. `κ_tok(P) ≤ B` is a **hard cap**, not a hint (estimate `κ_tok = countContextTokens`, the `~1.33 × word-count` heuristic — G4 boundary).
2. The cut is deterministic: rank-descending by `rel`, tie-break by least `NodeId` (stable index position).
3. Conservation: `rendered + omitted = matched` — every dropped node/edge is accounted in the omitted footer.
4. Top-node preservation: even when the best node alone exceeds `B`, the single highest-ranked node is always emitted (minimal necessary overrun is reported).
5. Hub/god nodes are **not** force-included; `### Hub Nodes` section omitted when no hub is query-relevant; expansion hints capped at top `N` communities under a max member size.

**Requirement IDs (source):** REQ-O1..O5, REQ-C1..C4 (AVI-565).
**Surfaces:** `UseCase/FormatContext.hs` (`formatContextForLLMBudgeted`, `countContextTokens`, `formatExpansionHintsBudgeted`), `UseCase/SelectContext.hs` (`scGodNodes`, `matchScore`), `UseCase/Query.hs` (`qrespNodes`), `Domain/Graph/Score.hs` (`normalizeScore`, `fullLabelBoostForTerms`), specs `query-noise-control`, `query-serving`, `context-budget-enforcement`, `query-relevance-scoring`, and `enforce-query-token-budget`.
**Acceptance:** AVI-565 §8 AC-1/AC-2/AC-4; property tests — over-budget, minimal-budget, normal cases; permutation-invariance of `qrespNodes`.

**Rigor notes (from AVI-565 §7 gaps):**
- G1 `fullLabelBoost` vs `fullLabelBoostForTerms` must be collapsed (Assumed, 🔧 Dev).
- G2 hub force-inclusion filter must be confirmed present in `SelectContext` (Assumed, 🔧 Dev).
- G4 cap measured on the estimate, not true token count (Open).

---

## 3. GT-Q2 — Topology/content store split (F2)  `[AVI-565 §4, §5]`

**Statement.** Define the **slim projection** `π ≡ P` (see §4) by the identity/metadata tuple `I(n) = (nodeId, nodeSourceFile, nodeKind, rel(n))` plus `Topology(n) = (nodeSourceFile, nodeLineStart, nodeLineEnd, nodeKind, nodeCommunityId, nodeDegree, edge incidence)`; the **heavy payload** `S(n) = (nodeLabel, nodeSignature)` plus any source-text in `nodeExtra` is **never** materialized on the hot query path. Consequences:
1. Hot query path reads only `G_st = P(G)`; `|G_st| = O(N + E)` with a **bounded per-node constant** (target ≤ 256 B + truncated label; no source text).
2. Reload and peak-memory bounds follow from the split (scale guard: memory `O(budget)` per segment, not `O(graph size)`).
3. `--path <glob>` restricts candidates to `C_g = { n : nodeSourceFile n ≡ g }` *before* traversal/scoring (path filter before traversal).
4. Round-trip: given `I(n)` plus an on-demand fetch of `S(n)`, the original node is reconstructible — the budgeted listing is a projection, not a replacement (cost changes, semantics do not).
5. Size savings: rendered cost `κ_tok ≤ Σ c(I(n)) + O(1)`, independent of `F = Σ c(S(n))`.

**Requirement IDs (source):** REQ-S1..S4 (AVI-565).
**Surfaces:** `Domain/Types/Node.hs` (`nodeSourceFile/nodeLineStart/nodeLineEnd/nodeKind`), `Domain/Graph/Index.hs` (`pathGlobFilter`), `Domain/Graph/Score.hs`, specs `node-schema`, `graph-json-contract`, `query-scoping`, `jgf-graph-serialization`, and `fix-runtime-ram-crash`.
**Acceptance:** AVI-565 §8 AC-3; identity-only listing assertion; path-filter scenarios; complexity assertion on `formatContextForLLMBudgeted`.

---

## 4. GT-Q3 — Canonicality score (F3)  `[AVI-565 §1.2, §3]`

**Statement.** Define the **canonicality/relevance score**
```
rel(n) = normalizeScore(raw(n), q) + boost(n)
```
where `raw(n) = |{ t ∈ T : n matches t }|`, `normalizeScore(m,q) = m/q` (0 if q=0), and `boost(n) = 0.1` iff some query term equals `nodeLabel n` fully. Properties:
1. Range: `rel ∈ [0, 1.1]` (plus `0` for empty query); additivity `result_match/10 + boost/0.1`.
2. Non-matching context nodes score exactly `0.0` (no flat "full-label boost"; boost is derived from query terms, **not** node label tokens) — this is the F3 correction: relevance no longer implies the score is a canonicality proxy.
3. Deterministic matched-first ranking: all matched nodes (`rel > 0`) precede all `0.0` neighbors, tie-broken by stable index position.
4. Canonical selection is a pure function of `(query, graph)` — no RNG, no insertion-order leakage; within each strategy the canonical subgraph is the highest-scoring prefix that fits `B`.

**Requirement IDs (source):** REQ-C1..C4 (AVI-565). Note: F3 ("relevance ≠ canonicality") has **no** openspec surface today beyond `query-relevance-scoring` refinement; the combination function `rel_combined = relevance × κ_candidate`? is deliberately left as the surface doc / product-owner decision (Open per plan §6).
**Surfaces:** `Domain/Graph/Score.hs` (`computeScore`, `normalizeScore`, `fullLabelBoostForTerms`), `UseCase/Query.hs` (`scoredNodesSorted`, `qrespNodes`), `UseCase/SelectContext.hs` strategies; spec `query-relevance-scoring`.
**Acceptance:** AVI-565 §8 AC-2/AC-4; `rel` range + non-match-zero property tests; `structuredLogger` canonical definition ranks above all test wrappers (fixture).

---

## 5. CT-Q1 — Slimming as projection morphism  `[AVI-566 §1]`

**Statement.** `P : Gr → Gr` is an **idempotent endofunctor** (`P(P G) = P(G)`) coreflecting the slim subcategory, erasing exactly the content fields `nodeLabel`, `nodeSignature`, and source-text in `nodeExtra`, while preserving identity (`nodeId`), topology (`nodeSourceFile, nodeLineStart, nodeLineEnd, nodeKind`), and all edge/incidence structure. `P` preserves every query-visible invariant (community, degree, adjacency). `eta_G : G → P(G)` is a valid `Gr`-morphism since `Gr`-morphisms constrain only keys + incidence + relation, not content; the reflection is unique up to unique iso.
**Sub-requirement CT-Q1a — short stable IDs.** `shortId = hash_loc(Loc(n))` where `Loc = (source_file, kind, (lineStart,lineEnd), symbolName)`. Collision-free modulo hash width (birthday bound; injective for `file#line` encodings) and stable across rebuilds. **BREAKING** on `node-schema` / `enforce-query-token-budget` (old text-blob IDs no longer valid) — flagged, not silently assumed.
**Requirement IDs (source):** R-Q1, R-Q1a.
**Surfaces:** `Domain/Types/Node.hs` (`:120`), `Domain/Graph/Core.hs` (`:47`), `Gr`/`Gr_inj` (AVI-536 §1.1), `enforce-query-token-budget` (BREAKING), `openspec/specs/node-schema`.
**Acceptance (AVI-566 AC-1):** ID stability across two independent extractions of the same tree; collision bound.

---

## 6. CT-Q2 — Colimit preservation  `[AVI-566 §2]`

**Statement.** `P` preserves the AVI-511 merge colimit: `P(merge(V_i)) ≅ merge(P(V_i))` — projecting the merged graph equals merging the projected views. Independent of the `alpha` direction (old-wins/new-wins): erasure makes content keys conflict-free on both sides; preserved fields resolve identically.
Cluster (`clusterGraphWithResolution'`) is **explicitly excluded** — not colimit-preserving.
**Requirement ID (source):** R-Q2.
**Surfaces:** `mergeGraphs`/`mergeExtractions` (`Core.hs:140,128`), `openspec/specs/09-merge`, `AVI-511-colimit-merge.md`.
**Acceptance (AVI-566 AC-2):** merge-then-project ≅ project-then-merge on a fixture.

---

## 7. CT-Q3 — No-loss contract  `[AVI-566 §3]`

**Statement.** `P` may lose **source text and nothing else**: for every query-visible column `c ∈ QueryVis`, `c(P(n)) = c(n)`. `QueryVis` = `{nodeId, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeKind, nodeCommunityId, nodeDegree, nodeIsBridge, nodePresentBits, edge incidence/structure}`. **`nodeLabel` and `nodeSignature` are content**, not query-visible. `nodeExtra` is preserved except embedded source-text blobs. M-4 (whether `nodeSignature` should be reclassified into `QueryVis`) is an Open decision (see §9).
**Requirement ID (source):** R-Q3.
**Surfaces:** `node-schema` (12 canonical fields), `graph-json-contract`.
**Acceptance (AVI-566 AC-3):** field-by-field property test over the full node-schema field set.

---

## 8. JOINT-Q — determinism of the budget-cut order  `[AVI-566 §4]`

**Statement.** The final output order (GT-Q1 rank × GT-Q3 `rel` × deterministic tie-break) is a **total order that is a function of the graph as a mathematical object**, independent of `Data.Map` iteration/insertion order. GT defines the order; CT proves it is well-defined (representation-independent): for a canonical iso `φ : G1 ≅ G2` (same structure, different `Map` insertion), `order_{G1} = order_{G2}` under the transported labeling. Byte-identical slim output under reordered insertion is the accepting property test.
**Acceptance (AVI-566 AC-4 + JOINT property):** build the same graph under two insertion orders; assert byte-identical slim ranked output.

---

## 9. Consolidated requirements table (IDs ↔ sources ↔ status)

| ID | Unit | Statement (short) | Source IDs | Key surfaces | Accept. | Status |
|---|---|---|---|---|---|---|
| GT-Q1 | GT | Hard token cap + deterministic cut + omitted footer + top-node + hub/hint bounds | REQ-O1..O5, REQ-C3/C4 | FormatContext, SelectContext, Score, Query | AC-1/AC-4 | Proven (O1/O3 code); Assumed (O4 G2) |
| GT-Q2 | GT | Topology/content split; bounded per-node; path filter before traversal; round-trip; savings | REQ-S1..S4 | Node/Index, graph-json-contract | AC-3 | Assumed 🔧 Dev (G3) |
| GT-Q3 | GT | Canonicality score `rel ∈ [0,1.1]`, non-match zero, matched-first, canonical prefix under `B` | REQ-C1..C4 | Score, Query, SelectContext | AC-2 | Proven (C1/C2 code); Open (combination) |
| CT-Q1 | CT | `P` idempotent endofunctor (content-erasure, invariant-preserving); short stable IDs (BREAKING) | R-Q1, R-Q1a | Node/Core, Gr, node-schema | AC-1 | Proven (structural) |
| CT-Q2 | CT | `P` preserves merge colimit: `P(merge(V_i)) ≅ merge(P(V_i))` (cluster excluded) | R-Q2 | mergeGraphs/mergeExtractions, 09-merge, AVI-511 | AC-2 | Proven (structural) |
| CT-Q3 | CT | No-loss: `c(P(n)) = c(n)` for all `c ∈ QueryVis`; only label/signature/extra-text may change | R-Q3 | node-schema (12 fields), graph-json-contract | AC-3 (field-by-field) | Proven (modulo M-4) |
| JOINT-Q | J | Budget-cut order is a total order over the graph object, independent of Map insertion order | JOINT (both) | enforce-query-token-budget (budget-cut) | byte-identical slim output | Proven (structural) |

No requirement from either expert doc is dropped here: every `REQ-*` and `R-*` row in the source docs appears in §2–§8 of this document and in the source ID column above. Uncovered sub-claims are recorded in §9 under Assumed/Open.

---

## 10. Proven / Assumed / Open (consolidated)

- **Proven (code-verified or standard categorical fact).**
  - GT-Q1 REQ-O1/O2/O3: `countContextTokens`-based hard cap with top-node survival and omitted footer exists in `FormatContext.hs`.
  - GT-Q3 REQ-C1/C2/C3: `normalizeScore`/`fullLabelBoostForTerms` match ranges and non-match zero (Query.hs uses `fullLabelBoostForTerms`, not the flat variant).
  - CT-Q1/CT-Q2/CT-Q3 structural claims and JOINT-Q reflection/uniqueness (standard idempotent functor / colimit-preservation / representation-independence).
- **Assumed (needs Graphos Dev confirmation):**
  - GT-Q1 REQ-O4 hub force-inclusion filter (`SelectContext.hs`) — pending verification (G2).
  - GT-Q2 round-trip reconstruction and short-ID collision profile — pending (G3).
  - CT-Q1 short-ID derivation change is **BREAKING** for consumers of text-blob IDs; migration scope is Dev-owned.
  - `fullLabelBoost` vs `fullLabelBoostForTerms` collapse (G1).
  - `nodeExtra` source-text blob inventory unknown; loss boundary rests on schema convention (M-3).
- **Open (needs external decision):**
  - G4: `κ_tok(P) ≤ B` is as tight as the estimator; estimator error bound to document.
  - M-4: whether `nodeSignature` is `QueryVis` (CT-Q3 boundary); resolve with Product Owner before finalizing R-Q3.
  - Whether `rel` becomes part of `query-relevance-scoring` spec and how `κ` (canonicality candidates) combines with `rel` (relevance) — product-owner decision per plan §6.
  - Short-ID hash width fixed by `node-schema` (open per plan §6).

---

## 11. Handoff

1. **To Graphos Dev** (behavior-constraining requirements; required loop before finalizing): G1, G2, G3, G4, REQ-O4, REQ-S3, short-ID migration, M-4 schema decision. See plan §5 step 4.
2. **To Product Owner** (openspec decomposition; this cell created no openspec changes): GT-Q1/Q2/Q3 and CT-Q1/Q2/Q3 require spec refinement against `context-budget-enforcement`, `jgf-graph-serialization`, `query-relevance-scoring`, `query-scoping`, `query-serving`, `node-schema`, `graph-json-contract`, `09-merge`, `query-noise-control`. F3 needs a new openspec surface today.
3. **To Head of Graphos** (upward note): proven items above; assumed items requiring Dev confirmation; open items requiring PO decision. Consolidated doc succeeds the plan's success bar (no notation conflicts with AVI-508 §0; no requirement dropped).

---

## Definition of done (AVI-563)

- [x] Both expert docs (`AVI-563-query-ergonomics-category.md`, `AVI-565-query-output-size-canonicality-store-split.md`) meet their own bar (statement + surface + checkable acceptance; property/invariant/complexity assertions).
- [x] JOINT-Q stated identically in both expert docs; restated here verbatim as a cross-reference.
- [x] Consolidated doc reuses AVI-508 §0 notation without conflict; introduces only the two aliases `κ → rel`, `π → P`.
- [x] No requirement silently dropped: every REQ-O/C/S and R-Q row appears in §2–§8 and in §9.
- [x] Master register (`consolidated-requirements.md` §1) extended with `GT-Q1..3`, `CT-Q1..3`, `JOINT-Q` rows.
- [ ] Behavior-constraining requirements looped with Graphos Dev (follow-up required before finalizing).
- [ ] Consolidated register handed to Product Owner for openspec decomposition (follow-up).

---

*Prepared by head-rnd from the accepted plan rev `5ee70789` and expert units AVI-565 / AVI-566.*