# Query Output-Size Contract, Canonicality Scoring & Store Split — Mathematical Requirements (Graphos Context Graph)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-565](/AVI/issues/AVI-565) (child of [AVI-563](/AVI/issues/AVI-563), the "improuvement" / context-optimization worktree)
**Status:** requirements document — grounded in domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

**Deliverable topics (from the issue title):**
1. **Query output-size contract** — bound the *serialized* size of MCP/query context payloads by a hard token budget, with deterministic truncation order and an omitted-count footer. (§2)
2. **Canonicality / relevance score** — formalize the node-relevance score used to rank what is "canonical" (most representative) for a query, its bounds `[0,1]`, and the deterministic ranking that orders matched nodes above context neighbors. (§3)
3. **Store split** — formalize the separation between lightweight identity/metadata (always materialized) and heavyweight source text (fetched on demand), and the source-path scoping that restricts the token budget to in-scope nodes. (§4)

These three topics are the graph-theory half of the AVI-563 worktree; the category-theory half ([AVI-566](/AVI/issues/AVI-566)) concerns colimit-as-projection slimming. The two halves compose through the unified notation of §0 and the consolidated requirements layer of [AVI-508](/AVI/issues/AVI-508).

---

## 0. Notation (single glossary)

Notation is exactly the shared Graphos domain glossary (as reproduced in [AVI-508](/AVI/issues/AVI-508) and the consolidated doc), specialized to the query/format surfaces this issue constrains:

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`).
- `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`).
- `Edge = Edge { edgeId :: EdgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `edgeId = EdgeId (source "->" target ":" relationToText relation)`.
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd, gAdjBack, gDirected, … }` (`Graph/Core.hs:47`). `V = Map.keys (gNodes g)` (|V| = N), `E = Map.keys (gEdges g)`.
- `terms :: [Text]` — the normalized query terms (lowercased, whitespace-split).
- `budget :: Int` — a **token budget** (see §2.1), not a node count or character count.
- `token_estimate :: Int` — the reported serialized size, computed by `countContextTokens` (§2.1).
- `rel : Node → [0,1]` — the canonicality/relevance score function (§3).

**Weighted-vs-unweighted scope statement (reused).** The scoring surfaces here read node *labels*, *source files* and *degree*; they do **not** read `edgeWeight`/`edgeRelation`/`nodeSignature`. Every claim in this document is a claim about label/path/degree structure as materialized in the query index, not about weighted modularity. Weighted-modularity claims remain governed by [AVI-508](/AVI/issues/AVI-508).

---

## 1. Formal Definitions

### 1.1 The query response as a size-bounded payload
A **query response** is a serialized payload `P` (markdown for `select_context`, JSON for `query_graph`) together with a non-negative integer `token_estimate = κ(P)` where `κ` is the token-cost estimator (§2.1). Let `B ∈ ℕ` be the requested budget. The **output-size contract** is the assertion `κ(P) ≤ B` on every *rendered* response, subject to the minimal-preservation rule that the single highest-relevance node is always kept even when it alone exceeds `B`.

### 1.2 The canonicality / relevance score function
Fix a query with normalized terms `T = terms`, `|T| = q ≥ 1`, and a node `n`. Define the **canonicality score**
```
rel(n) = normalizeScore(raw(n), q) + boost(n)
```
where `raw(n) = |{ t ∈ T : n matches t }| ∈ {0,…,q}` is the number of distinct query terms matched by `n` (`Score.computeScore`), `normalizeScore(m,q) = m/q` for `q>0` and `0` for `q=0` (`Score.hs:166`), and
```
boost(n) = 0.1  if ∃ t ∈ T : lower(t) == lower(nodeLabel n)   (any-term full-label exact match)
         0    otherwise                                          (Score.fullLabelBoostForTerms, Score.hs:190)
```
The score is a **bounded, additive decomposition**: a normalized term-match component in `[0,1]` plus a bounded label-exactness bonus in `{0, 0.1}`.

### 1.3 Store split and source-path scoping
Define a **split view** of a node as the pair `(I(n), S(n))` where
- `I(n) = (nodeId, nodeSourceFile, nodeKind, rel(n))` is the **identity/metadata tuple** — lightweight, constant-size per node, always materialized;
- `S(n) = nodeLabel` (which embeds/points to the full source-text snippet) is the **heavy payload** — fetched on demand, never assumed present in a budgeted listing.

The **store split** is the invariant that a budgeted listing carries only `I(n)` and never `S(n)`, except inside a fully-rendered node body that has been budget-checked (§2). Formally, for a listing `L ⊆ Nodes`, its serialized cost satisfies `κ(serialize L) ≤ Σ_{n∈L} c(I(n)) + c(header)`, i.e. it is bounded by the sum of identity costs plus a fixed header — never by the unbounded source-text costs.

A **source-path scope** is a glob `g ∈ Subtree(FilePath)`. The scoped candidate set is
```
C_g = { n ∈ V : nodeSourceFile n ≡ g }   (via Domain.Graph.Index.pathGlobFilter)
```
and the query MUST restrict traversal to `C_g` before scoring (§4.2).

---

## 2. Query Output-Size Contract

### 2.1 Hard token cap measured in tokens, not characters
**REQ-O1 (hard cap).** For every rendered response `P` and budget `B`, the contract SHALL assert `κ(P) ≤ B`, where `κ = FormatContext.countContextTokens` (the `~1.33 × word-count` heuristic). Raw character length is **not** an acceptable proxy.
> **Grounding.** `formatContextForLLMBudgeted` computes `headerToks = countContextTokens header` and every body segment's token count via `countContextTokens` (`FormatContext.hs:55,66,76,86,95,104,127`).
> **Acceptance (scenario):** `select_context budget=3000` against a 9000-token graph yields a response whose `token_estimate ≤ 3000`.

### 2.2 Deterministic truncation order: lowest rank dropped first
**REQ-O2 (drop-lowest-first ordering).** Truncation SHALL remove nodes and edges ordered by **non-increasing canonicality** `rel(·)` — i.e. the lowest-ranked candidates are dropped first. Because `rel` is a deterministic function of `(terms, nodeLabel, index position)`, the drop set is a total order (tie-break by stable index position), so the result is reproducible.
> **Grounding.** `goNodes`/`goEdges` fold over `sortedNodes`/`sortedEdges` (already relevance-sorted), appending until `newToks > budget` then stopping (`FormatContext.hs:68,78`); the highest-ranked element is consumed before any budget test (`goNodes` begins with the first node unconditionally).
> **Theorem (top-node preservation).** If the sorted list is non-empty, the first node is always emitted: the budget test for the accumulator occurs only after the first node is already included, so `κ({top node}) ≤ κ(P)` and the top node survives even when `κ({top node}) > B`.
> **Acceptance (scenario):** a payload smaller than the best node still returns exactly that one node; the footer reports the minimum-necessary overrun.

### 2.3 Omitted-count footer
**REQ-O3 (omitted footer).** When any node/edge/bridge/hub is dropped, the payload SHALL end with a footer stating the omitted counts: `- _omitted: <k> nodes, <e> edges_`. The footer is itself budget-checked and dropped when it would overflow.
> **Grounding.** `totalOmitted = omittedNodes + omittedEdges + omittedBridges + omittedHubs`; `footerTxt` is emitted only when `totalOmitted > 0`, and `final` appends it only if it fits (`FormatContext.hs:121-129`).
> **Acceptance (scenario):** an over-budget response ends with a footer reporting the omitted node/edge counts.

### 2.4 Hub/god nodes are not force-included
**REQ-O4 (no unconditional hub inclusion).** Context-selection strategies `selectCommunityAware` / `selectRelevanceWeighted` / `selectPathBased` SHALL NOT seed `scGodNodes` from `analysisGodNodes analysis` unconditionally. A hub node appears only if it is query-relevant (`matchScore n terms > 0`) or reached by BFS within depth; the `### Hub Nodes` section is omitted when no hub node is selected.
> **Grounding.** `scGodNodes = [(gnId gn, gnEdges gn) | gn ← analysisGodNodes analysis, …]` must be filtered by relevance/depth, not taken wholesale (`SelectContext.hs:174,223`).
> **Acceptance (scenario):** a focused Parser-community query does not render the degree-246 `Main` god node from an unrelated community.

### 2.5 Bounded, relevance-filtered expansion hints
**REQ-O5 (bounded hints).** `formatExpansionHintsBudgeted` SHALL cap suggested communities at the top `N` ranked by relevance (default `N = 8`), omit any community whose member count exceeds `--max-hint-community-size` (default `50`), omit the chat-history community (`chatCommunityId`), and omit the section entirely when nothing passes.
> **Grounding.** `formatExpansionHintsBudgeted maxHints maxCommSize sc` (`FormatContext.hs:223`).
> **Acceptance (scenario):** a 2563-member community is hidden from the expansion hints under the default size cap.

---

## 3. Canonicality / Relevance Score

### 3.1 Range and additivity
**REQ-C1 (range).** For `q ≥ 1`, `rel(n) ∈ [0, 1.1]`; specifically `rel(n) = raw(n)/q + boost(n)` with `raw(n)/q ∈ [0,1]` and `boost(n) ∈ {0, 0.1}`. For `q = 0` (empty query) every score is `0`.
> **Grounding.** `normalizeScore` returns `fromIntegral matched / fromIntegral queryTotal` (`Score.hs:169`); `fullLabelBoostForTerms` returns `0.1` or `0` (`Score.hs:192`).
> **Acceptance (scenario):** a 2-term query matching 1 term yields `0.5`; adding a full-label exact match yields `0.6`.

### 3.2 Non-matching context nodes score exactly zero
**REQ-C2 (zero for non-matches).** A node in the expanded neighborhood that matches no query term has `rel(n) = 0.0` — it receives **no** flat full-label boost, because the boost is derived from the query terms, not from the node's own label tokens.
> **Grounding.** `boost(n) = 0.1` only when a query term equals the node's full label (`Score.hs:192`); a non-matching node has `raw(n) = 0` and, absent a full-label hit against the *query*, `boost(n) = 0`.
> **Acceptance (scenario):** a single-word neighbor matching no query term scores `0.0`, not `0.1`.

### 3.3 Deterministic, matched-first ranking
**REQ-C3 (ranking total order).** The ranked result list `qrespNodes` SHALL be ordered by `rel` descending, with all matched nodes (`rel > 0`) before all non-matching context neighbors (`rel = 0.0`), tie-broken by stable index position so identical graphs+queries yield byte-identical orderings.
> **Grounding.** `scoredNodesSorted` assembled at `Query.hs:149,178` (`normalizeScore rawScore (length terms) + fullLabelBoostForTerms …`); `qrespNodes = scoredNodesSorted` (`Query.hs:221`).
> **Acceptance (scenario):** with both matched and non-matching single-word neighbors present, every matched node precedes every `0.0` neighbor.

### 3.4 Canonicality as a selection key
**REQ-C4 (canonical selection key).** Within each strategy, the canonical subgraph is selected by ranking candidates on `rel(·)` and taking the highest-scoring prefix that fits `B`. Because `rel` depends only on `(terms, nodeLabel, index)`, the canonical selection is a pure function of the query and graph — no RNG, no insertion-order leakage beyond the stable tie-break.
> **Grounding.** Strategies filter to `{ n : matchScore n terms > 0 }` (`SelectContext.hs:138,192,241`) then order by relevance for budgeted formatting.

---

## 4. Store Split & Source-Path Scoping

### 4.1 Identity/metadata vs source-text split
**REQ-S1 (lightweight identity always materialized).** A budgeted listing carries only `I(n) = (nodeId, nodeSourceFile, nodeKind, rel(n))` per node and never the full `nodeLabel`/source snippet, so its serialized size is bounded by `Σ c(I(n)) + c(header)` (§1.3).
> **Grounding.** List responses return `id, source_file, score, kind, <short truncated label>` rather than full source text (`enforce-query-token-budget` proposal); short stable node IDs (hash or `file#line`) replace text-blob IDs.
> **Acceptance (scenario):** a list entry exposes `source_file` and `kind` but not the embedded snippet; node IDs do not contain snippet text.

### 4.2 Source-path scope restricts the budget to in-scope nodes
**REQ-S2 (path filter before traversal).** `graphos query --path <glob>` SHALL restrict candidate matches to `C_g = { n : nodeSourceFile n ≡ g }` **before** traversal/scoring, so the token budget is spent only on in-scope nodes.
> **Grounding.** `pathGlobFilter :: Map NodeId Node -> Text -> Set NodeId -> Set NodeId` (`Index.hs:170`) yields the in-scope candidate set fed to scoring.
> **Acceptance (scenario):** `graphos query --path 'src/cli/**' "observability"` returns only nodes under `src/cli/`; a scope with no in-scope match reports verdict `none`.

### 4.3 Round-trip reconstruction invariant
**REQ-S3 (lossless recovery on demand).** The split is lossless: given `I(n)` plus an on-demand fetch of `S(n)`, the original node is reconstructible. Therefore the split changes **cost**, not **semantics** — the budgeted listing is a projection, and the full node is recoverable.
> **Grounding.** `nodeSourceFile`/`nodeLineStart`/`nodeLineEnd` (`Node.hs:120`) index into the source file; the truncated `label` preview is a *projection* of the full snippet, not a replacement.
> **Acceptance (scenario):** a caller that fetches the preview target rebuilds the exact original node text.

### 4.4 Size-savings bound
**REQ-S4 (split cost bound).** For a listing `L`, let `H = Σ_{n∈L} c(I(n))` (identity cost) and `F = Σ_{n∈L} c(S(n))` (full-source cost). The split guarantees the rendered budgeted payload cost `κ ≤ H + c(header)`, i.e. `κ ≤ H + O(1)` independent of `F`. Hence for dense graphs where source snippets dominate `F`, the asymptotic savings are `Θ(F) → Θ(H)`.
> **Grounding.** Budgeted formatting never folds `S(n)` into the counted payload (§2.2, §4.1).

---

## 5. Complexity Bounds

Let `q = |terms|`, `L_n = |nodeLabel n|` (source-text length), `D = max degree`.
- **Scoring.** Computing `rel(n)` for one node is `O(q·L_n)` worst-case over the label scan, realized by `computeScore` (term lookups) + `fullLabelBoostForTerms` (`Score.hs:173,190`). For the whole candidate set it is `O(Σ_n q·L_n)`.
- **Path filter.** `pathGlobFilter` is `O(|C| · |pattern|)` over candidates (`Index.hs:170`); applied once per query.
- **Budgeted format.** `formatContextForLLMBudgeted` folds over sorted nodes/edges once, each step doing one `countContextTokens` (`O(segment length)`), so it is `O(Σ segment lengths)` = `O(total rendered size)` — linear in the (bounded) output, never in the unbounded input graph.
- **Hint cap.** `formatExpansionHintsBudgeted` sorts communities by relevance (`O(C log C)`) then caps at `maxHints` (`FormatContext.hs:223`).
> **Scale guard.** Because `countContextTokens` bounds the work to the *rendered* size, memory is `O(budget)` per segment, not `O(graph size)` — the split is what makes a sub-linear-in-graph cost possible for super-budget graphs.

---

## 6. Determinism Lens

All three surfaces are deterministic functions of `(graph, terms, budget, config)`:
- `rel` is pure on `(terms, nodeLabel, index position)`; no `IO`, no RNG.
- Truncation order uses the total order induced by `rel` + stable index tie-break, so identical inputs yield byte-identical payloads.
- `pathGlobFilter` and `fullLabelBoostForTerms` are pure string operations.
> **Property test:** for two graph instances that are index-permutations of each other (same edges/nodes, reordered), `query` under the same `(terms,budget)` yields equal `qrespNodes` up to the canonical permutation and equal `token_estimate`.

---

## 7. Spec-vs-Code Gaps Flagged (for Graphos Dev)

| # | Gap | Surface | Action |
|---|---|---|---|
| G1 | `fullLabelBoost` vs `fullLabelBoostForTerms` — two functions with overlapping semantics; the query path uses `fullLabelBoostForTerms`. Confirm the multi-term version is the canonical one and collapse. | `Score.hs:181,190` | dedup + assert equal behavior |
| G2 | REQ-O4 (no hub force-inclusion) is a **proposed settled semantic**; verify `scGodNodes` in `SelectContext.hs:174,223` filters by relevance/depth rather than taking `analysisGodNodes` wholesale. | `SelectContext.hs` | confirm filter present |
| G3 | REQ-S3 round-trip: confirm the truncated preview is documented as a projection and that a fetch path reconstructs the full node byte-exact. | `Report.hs` / format layer | add round-trip test |
| G4 | `countContextTokens` heuristic (`~1.33 × word-count`) is itself an estimate; the contract `κ(P) ≤ B` is only as tight as this estimator. Treat the cap as `≤ B` on the *estimate*, not the true token count. | `FormatContext.hs` | document estimator error bound |

---

## 8. Acceptance-Criteria Mapping (issue checklist)

- **AC-1 (output-size contract).** `κ(P) ≤ B` with lowest-rank-first truncation, top-node preservation, omitted footer — REQ-O1/O2/O3. *Check:* scenario tests for over-budget, minimal-budget, and normal cases.
- **AC-2 (canonicality score).** `rel ∈ [0,1.1]`, non-matching nodes score `0.0`, matched-first ranking — REQ-C1/C2/C3. *Check:* property tests on normalized scores + ranking order.
- **AC-3 (store split / scoping).** budgeted listing carries identity only; `--path` restricts candidates before traversal; round-trip lossless — REQ-S1/S2/S3/S4. *Check:* path-filter scenario + identity-only listing assertion.
- **AC-4 (determinism).** identical graph+query ⇒ identical payload — REQ-C3/D §6. *Check:* permutation-invariance property test.
- **AC-5 (complexity).** budgeted format is linear in rendered size — §5. *Check:* complexity assertion on `formatContextForLLMBudgeted`.

---

## 9. Surface Grounding (line references)

| Topic | Surface | Location |
|---|---|---|
| Token estimator | `countContextTokens` | `src/Graphos/UseCase/FormatContext.hs:12,50-127` |
| Budgeted format + footer | `formatContextForLLMBudgeted` | `FormatContext.hs:53-129` |
| Hint cap | `formatExpansionHintsBudgeted` | `FormatContext.hs:223` |
| Relevance score assembly | `normalizeScore`, `fullLabelBoostForTerms` | `src/Graphos/Domain/Graph/Score.hs:166,190` |
| Score used in query | `Query.hs:149,178,221` (`qrespNodes`) | `src/Graphos/UseCase/Query.hs` |
| Strategy relevance filter | `matchScore`, `scGodNodes` | `src/Graphos/UseCase/SelectContext.hs:138,174,192,223,241` |
| Path filter | `pathGlobFilter` | `src/Graphos/Domain/Graph/Index.hs:170` |
| Node identity tuple | `Node.hs:120` (`nodeSourceFile/nodeLineStart/nodeLineEnd`) | `src/Graphos/Domain/Types/Node.hs` |

**Specs grounded:** `openspec/specs/context-budget-enforcement/spec.md`, `openspec/specs/query-relevance-scoring/spec.md`, `openspec/specs/query-scoping/spec.md`, and the `enforce-query-token-budget` change proposal (`openspec/changes/enforce-query-token-budget/proposal.md`).

---

## Proven / Assumed / Open legend

- **Proven:** established within this document by direct code inspection (the functions above exist and their behavior is read directly).
- **Assumed 🔧 Dev:** a required design semantic pending Graphos Dev confirmation of the Haskell — REQ-O4 (G2), REQ-S3 (G3).
- **Open:** needs an external decision — the tightness of the `κ(P) ≤ B` cap relative to the `countContextTokens` estimate (G4); whether `fullLabelBoost` should be collapsed into `fullLabelBoostForTerms` (G1).
