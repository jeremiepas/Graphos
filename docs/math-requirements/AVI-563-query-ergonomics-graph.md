# Query Output-Size Contract, Canonicality Score & Slim Store — Mathematical Requirements (Graphos, Graph-Theory Half)

**Author:** Graph Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-565](/AVI/issues/AVI-565) (child of [AVI-563](/AVI/issues/AVI-563), plan rev `5ee70789`) · GT counterpart to [AVI-566](/AVI/issues/AVI-566) (Category Theory Expert).
**Status:** requirements document — math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.
**Scope:** three graph-theoretic requirements — output-size contract (GT-Q1), topology/content store split (GT-Q2), canonicality score (GT-Q3) — plus the joint determinism clause JOINT-Q. No OpenSpec changes are proposed by this document. It constrains the AVI-563 surfaces `enforce-query-token-budget`, `jgf-graph-serialization`, `fix-runtime-ram-crash`, and F3 (which has no active surface today).

> **Grounding.** Every object is pinned to `src/Graphos/Domain/` or `openspec/specs/`. Notation is taken **verbatim** from the unified glossary of `docs/math-requirements/consolidated-requirements.md` §0 (AVI-508); this document invents **no** names for existing objects. The category-theory counterpart is `docs/math-requirements/AVI-563-query-ergonomics-category.md` (AVI-566); the two compose through §0 of both and through JOINT-Q §4.

---

## 0. Notation (reused from the consolidated glossary, §0 — not reinvented)

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`).
- `Node = Node { nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId :: Maybe Int, nodeKind, nodeDegree :: Maybe Int, nodeIsBridge :: Maybe Bool, nodeExtra :: Maybe Value, nodePresentBits :: Word64 }` (`Node.hs:120`).
- `Edge = Edge { edgeId :: EdgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:33`).
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd, gAdjBack, gDirected, … }` (`Graph/Core.hs:47`). `V = Map.keys (gNodes g)` (|V| = N), `E = Map.keys (gEdges g)`.
- `terms :: [Text]` — normalized query terms (lowercased, whitespace-split), `q = |terms|`.
- `budget :: Int` — **token budget**, not node count or character count.
- `τ(P) :: Int` — token estimate of a serialized payload `P`, defined as `FormatContext.countContextTokens(P)` (the `~1.33 × word-count` heuristic); the contract is always stated on `τ`, never on raw character length.
- `rel : Node → [0,1.1]` — the existing query-relevance score (`Query-relevance-scoring` spec; `Score.hs:166,190`), retained unchanged by this document.
- `κ : Node → [0,1]` — the **canonicality score** defined in GT-Q3 §3 (new).
- `rankKey : Node → (Double, Double, NodeId)` — the composite ranking key defined in GT-Q3 §3.4.
- `π : G ↦ G_st` — the slim-store projection defined in GT-Q2 §2 (graph-structural half of the GT-Q2 surface; the categorical universal property is AVI-566's R-Q1).
- `source_file`, `kind`, `line span`, `community`, `degree` — columns of `Node` (`Node.hs:120` and surrounding); see §2.

**Weighted-vs-unweighted scope statement.** This document's scoring and projection surfaces read node *labels*, *source files*, *kind*, *degree*, *community*, and edge *relation/weight/confidence*. Weighted-modularity claims remain governed by `AVI-508`; nothing here depends on Leiden running on weighted edges.

---

## 1. Formal Definitions

### 1.1 Query response and size
A **query response** is a serialized payload `P` (`select_context` markdown or `query_graph` JSON) together with a non-negative integer `τ(P)` (its token estimate). For a query `Q` with normalized terms `terms(Q)` and a budget `B ∈ ℕ`, the candidate matched set is `M(Q) ⊆ V` (nodes with `rel(n) > 0` or otherwise admitted by the selected strategy); the rendered set is `R(Q,B) ⊆ M(Q)`; the omitted set is `O(Q,B) = M(Q) \ R(Q,B)`. The **output-size contract** is the hard assertion `τ(P) ≤ B` on the rendered response, subject only to the minimal-preservation rule that if `M(Q) ≠ ∅` then `R(Q,B) ≠ ∅` even when the best node alone exceeds `B`.

### 1.2 Canonicality score `κ`
Fix a node `n`. Define the **canonicality score** `κ(n) ∈ [0,1]` from graph structure only (no `nodeLabel` source text, no `nodeSignature` body), using three structural indicators and one demotion indicator:

- `defKind(n) ∈ {0,1}` — 1 when `nodeKind n` is a *definition kind* (function, class, type, interface, module, declaration) as reported by the extractor; 0 otherwise.
- `definedIn(n) ∈ {0,1}` — 1 when there exists at least one edge `e ∈ E` with `edgeTarget e = nodeId n` and `edgeRelation e = Contains` (equivalently, `n` is contained in/defined within another node); 0 otherwise.
- `docCodeClass(n) ∈ {0,1}` — 1 when `nodeFileType n ∈ {CodeFile, DocFile}` (a documented code artifact rather than generated/test-only artifact); 0 otherwise.
- `demote(n) ∈ [0,1]` — 1 when `n` is a test, wrapper, or generated artifact, detected structurally as: `nodeSourceFile n` whose path contains a test/wrapper/generated segment (`/test/`, `/tests/`, `*.test.*`, `*.spec.*`, `_gen`, `.generated.`) **or** `nodeKind n` indicating wrapper/generated; 0 otherwise.

Then
```
κ_raw(n) = (defKind(n) + definedIn(n) + docCodeClass(n)) / 3   ∈ [0,1]
κ(n)     = clamp( κ_raw(n) · (1 − demote(n)), 0, 1 )             ∈ [0,1]
```
`κ` is a pure function of `(nodeKind, nodeFileType, nodeSourceFile, adjacency)`. It never reads `nodeLabel` or `nodeSignature`, so it is compatible with the slim store GT-Q2 (§2) by construction.

*Remark.* The two factors are multiplicative so a canonical-definition node never collapses to 0 through demotion, and a test/wrapper can still carry a reduced score when it happens to also be a definition-kind node. `clamp(·,0,1)` keeps `κ ∈ [0,1]` always.

### 1.3 Store split `π`
Define the **slim view** of a node `n` as the tuple
```
π(n) = (nodeId n, nodeSourceFile n, nodeKind n, (nodeLineStart n, nodeLineEnd n),
         nodeCommunityId n, nodeDegree n,
         { (edgeRelation e, edgeWeight e, edgeConfidence e) : e ∈ E, edgeSource e = nodeId n or edgeTarget e = nodeId n })
```
i.e. identity, source path, kind, line span, community, degree, and the relation/weight/confidence of incident edges — **but not** `nodeLabel`, `nodeSignature`, `nodeExtra` source-text blobs, or `nodePresentBits` (which tracks field presence of the fat node). The slim graph is `G_st = π(G)` with `gNodes(G_st) = { (id, π(n)) }` and `gEdges(G_st) = gEdges(G)` (edges untouched, since they already carry only relation/weight/confidence).

### 1.4 Matched-first candidate ordering
Let `matched(n) ⇔ rel(n) > 0`. Let the **candidate ranking order** for a (query `Q`, graph `G`) be the total order `≺` on `M(Q)` defined by:
```
n ≺ m  ⇔  ( rel(n), κ(n), -lexNodeId(n) )  >_lex  ( rel(m), κ(m), -lexNodeId(m) )
```
equivalently:
1. higher `rel` first;
2. on `rel`-ties, higher `κ` first;
3. on ties of both, **least `NodeId` (lexicographic, ascending)** first.
Here `rel` is the existing relevance score (`Score.hs:166,190`); `κ` is §1.2. This order is the "rank-descending" order of GT-Q1 augmented by the κ secondary key demanded by JOINT-Q. It is a function of `(graph, terms)` only (see §4).

---

## 2. GT-Q2 — Topology / Content Store Split

**REQ GT-Q2 (statement).** Formalize the projection `π : G ↦ G_st` (§1.3) keeping exactly `NodeId, source_file, kind, line span, community, degree, relation, weight, confidence` and dropping source text (`nodeLabel`, `nodeSignature`, source-text in `nodeExtra`, and presence bits). The hot query path (`query_graph`, `select_context`, `formatContext*`) reads **only** `G_st`; the fat `Graph` is never touched on that path. The relation `(Relation, Double, Confidence Double)` carried by edges is preserved verbatim.

Formal obligations:
- **Obligation 1 (projection).** `π` is a pure function of `G`: `G_st = π(G)`; applying `π` twice is idempotent on the preserved columns (`π(π(G)) = π(G)` on all preserved fields).
- **Obligation 2 (size bound).** `|G_st| = O(N + E)` with a **bounded per-node constant**: the slim node carries no field whose size grows with `nodeLabel` length, so the serialized bytes per slim node are bounded by a fixed constant `c` (target `c ≤ 256 B +` the bytes of the truncated label preview). Formally, for every node `n`, `serializedBytes(π(n)) ≤ c` independently of `L_n = |nodeLabel n|`.
- **Obligation 3 (no source-length dependence).** No field of `π(n)` is a function of `L_n`: changing `nodeLabel` length while holding `NodeId, nodeSourceFile, nodeKind, nodeLineStart, nodeLineEnd, nodeCommunityId, nodeDegree` fixed leaves `π(n)` unchanged. (Property test: mutate `nodeLabel` length only; assert `π(n)` equal.)
- **Obligation 4 (hot-path restriction).** The `query_graph` / `select_context` / format pipeline is typed over `G_st`: it cannot dereference `nodeLabel` beyond the truncated preview already budget-checked. `nodeSignature` is never read on the hot path.

> **Concrete surfaces.** `openspec/specs/node-schema` (twelve canonical fields); `openspec/specs/graph-json-contract` (compact serialization); `Graphos.Domain.Types` (`Node.hs:120`, `Edge.hs:88`, `Graph/Core.hs:47`). Constrains openspec changes `jgf-graph-serialization` + `fix-runtime-ram-crash`.
>
> **Acceptance (GT-Q2).** (A) Complexity assertion: serialization of a list of `N` slim nodes is `O(N)` with per-node constant `≤ c`; memory on the hot path is `O(budget)`, not `O(graph size)`. (B) Property test: for a fixture of nodes with varying `nodeLabel` length, mutating only the label length leaves `π(n)` byte-identical. (C) Scenario: `select_context` against a 4.5 GB graph renders from `G_st` without materializing full `nodeLabel` bodies. Joint acceptance with CT (AVI-566) AC-2/AC-3 is the determinism property (JOINT-Q).
>
> **Spec grounding.** `graph-json-contract`: *"compact node serialization: `id`, `source_file`, `score`, `kind`, truncated `label`; SHALL NOT emit full source text in list responses"*; `node-schema`: *"Canonical Node field set"* (twelve fields). The projection `π` is the GT-side formalization of that compact-set obligation; AVI-566 supplies the categorical universal property (idempotent endofunctor / coreflection).

---

## 3. GT-Q3 — Canonicality Score

**REQ GT-Q3 (statement).** Define `κ : Node → [0,1]` as in §1.2, promoting canonical definitions (`defKind`, `definedIn`, `docCodeClass`) and demoting tests/wrappers/generated code (`demote`). It satisfies the three axioms:

- **Axiom (i) — definition-over-test.** When exactly **one** definition node `d` (i.e. `defKind(d) = 1` and `demote(d) = 0`) matches the query terms (`rel(d) > 0`), then for every test/wrapper node `w` with `demote(w) = 1` that also matches the same terms (`rel(w) > 0`), the composite ranking places `d` **strictly above** `w`: `d ≺ w` does not hold and `w ≺ d` holds (equivalently `rel(d) > rel(w)` or (`rel(d) = rel(w)` and `κ(d) > κ(w)`)). In particular, with equal relevance, `κ(d) > κ(w)`.
- **Axiom (ii) — structure-only.** `κ` reads only `(nodeKind, nodeFileType, nodeSourceFile, adjacency)`; it never reads `nodeLabel` or `nodeSignature`. Therefore `κ` is well-defined on `G_st` (§2) and survives the projection unchanged: `κ(n) = κ(π(n))`.
- **Axiom (iii) — deterministic tie-break.** `κ` itself has no ties broken by insertion order: every term (`defKind`, `definedIn`, `docCodeClass`, `demote`) is a pure function of the node and its incident edges; the aggregate `κ_raw` is arithmetic over `{0,1}` values; the final ranking uses the `NodeId`-ascending tie-break (§1.4), which is independent of `Map` order.

**Final ranking — exact combination function.** The final per-node ranking key is the composite tuple (§1.4):
```
rankKey(n) = ( rel(n)  [descending],   κ(n)  [descending],   nodeId(n)  [ascending] )
```
The combined (scalar) equivalent, only for tooling that requires a single number, is
```
F(n) = rel(n) + κ(n) / 1000
```
**but** the authoritative order is the lexicographic tuple `rankKey` above, not the scalar: the scalar is guaranteed monotone with respect to `rankKey` only because `0 ≤ κ ≤ 1` and `1/1000` is below the smallest resolution of `rel`'s rational difference (`1/q ≥ 1/|terms|`; for `q ≤ 999` the step dominates). When `q ≥ 1000` the scalar MUST NOT be used as a proxy; tooling must sort on `rankKey`. (Or simply: sort on the tuple. This is the recommended implementation.)

> **Concrete surfaces.** `openspec/specs/query-relevance-scoring` (refinement — the `normalizeScore + fullLabelBoostForTerms` relevance score of §0 is retained; `κ` is added, not substituted); `openspec/specs/query-cli-contract` (`--budget`/ranking contract); `Graphos.Domain.Query` (`qrespNodes` assembly at `Query.hs:149,178,221`). F3 has no active OpenSpec surface today; this requirement is the seed.
>
> **Acceptance (GT-Q3).** Property test on the fixture graph: for the query `structuredLogger`, the canonical logger definition node (definition kind, defined-in relation, code file class, `demote = 0`) ranks strictly above **every** test-wrapper node (`demote = 1`) that matches the same terms — i.e. its `rankKey` is lexicographically greater (§1.4) than each wrapper's, even when both share `rel = 1.0`. Additional invariants: `κ(n) ∈ [0,1]` for all `n`; `κ(n)` is unchanged when `nodeLabel` length is mutated (Axiom ii, inherits GT-Q2 obligation 3); determinism: permuting insertion order leaves the `structuredLogger` top hit and all `rankKey` values unchanged.

---

## 4. JOINT-Q — Determinism of the Budget-Cut Order (must appear verbatim)

**JOINT-Q — determinism of the budget-cut order.** The final output order (GT-Q1 rank × GT-Q3 κ × deterministic tie-break) is a **total order that is a function of the graph as a mathematical object**, independent of Haskell `Map` iteration/insertion order. GT defines the order; CT proves it is well-defined (representation-independent). *Acceptance:* determinism property — the same graph built under two insertion orders ⇒ byte-identical slim output.

> **GT role (this document defines the order).** The GT-side definition of the order is `rankKey` (§1.4, GT-Q3): first `rel` descending, then `κ` descending, then `NodeId` ascending. Each component is a pure function of `(node, query, graph-structure)`:
> - `rel` is a pure function of `(terms, nodeLabel-as-token, index position)` per `query-relevance-scoring` / `Score.hs:166,190`.
> - `κ` is a pure function of `(nodeKind, nodeFileType, nodeSourceFile, adjacency)` per GT-Q3 axiom (ii).
> - the `NodeId`-ascending tie-break is the canonical least-key choice, independent of `Map` ordering.
>
> **CT role (AVI-566 proves well-definedness).** The CT expert proves that this composite order is **representation-independent**: for two graphs `G₁, G₂` that are isomorphic as weighted graphs (same `V, E, src, tgt, rel, w, nodeKind, nodeFileType, nodeCommunityId, nodeDegree, nodeSourceFile, nodeLineStart, nodeLineEnd`, possibly built under different `Data.Map` insertion orders), the order is preserved under the canonical iso, and — because `π` (GT-Q2) preserves exactly the columns `rankKey` reads — `rankKey` is a function of `π(G)`, i.e. of the slim graph. Hence `order_{π(G₁)} = order_{π(G₂)}`. Combined with GT-Q1's drop rule, the rendered payload `P` (slim) is byte-identical under reordered insertion.
>
> **Surface.** `openspec/changes/enforce-query-token-budget/specs/query-response-budget/spec.md` ("deterministically rank and cut results"; "highest-scoring nodes retained"); JOINT-Q pairs with AVI-566, whose combinatorial/projection grounding supplies the well-definedness proof.
>
> **Acceptance (JOINT-Q).** Build the same source-extraction graph under two distinct `Map`/insertion orders; run `query_graph` / `select_context` with identical `(terms, budget)` on `π(G)`; assert byte-identical slim output, identical `qrespNodes` node-id sequences, and identical `τ(P)`. Property name: **insertion-order invariance of slim budgeted output** (AC-4 of both halves).

---

## 5. GT-Q1 — Output-Size Contract

**REQ GT-Q1 (statement).** For every query `Q`, budget `B`, and graph `G` (read via its slim view `π(G)`), the serialized response `P = format(Q, B, π(G))` MUST satisfy:

- **(a) Hard cap.** `τ(P) ≤ B`. `τ` is `countContextTokens` (`FormatContext.hs`); raw character length is **not** an acceptable proxy. The cap is a property of the rendered payload, not a traversal hint.
- **(b) Deterministic cut.** Candidates in `M(Q)` are sorted by `rankKey` §1.4 (i.e. `rel` desc, then `κ` desc, then `NodeId` asc). Truncation removes the **lowest-ranked** candidates first; the drop set is the unique minimal suffix of this sorted list such that the emitted prefix (plus header and, when it fits, footer) satisfies (a). Because `rankKey` is a total order on `M(Q)` (JOINT-Q), the cut is unique.
- **(c) Conservation.** For nodes: `|R_nodes(Q,B)| + |O_nodes(Q,B)| = |M_nodes(Q)|`. For edges: `|R_edges(Q,B)| + |O_edges(Q,B)| = |M_edges(Q)|`. The footer reports exactly `|O_nodes|` and `|O_edges|` (plus dropped bridges/hubs in the dedicated counters); `omitted` counts account for every dropped candidate, with no double-counting and no silent drops. `rendered + omitted = matched` holds for each class independently.
- **(d) Top-node preservation (minimal preservation).** If `M(Q) ≠ ∅`, then the highest-ranked node under `rankKey` is always emitted, even when `τ({that node}) > B`; the budget test is applied to the *accumulator after* inclusion, not before. The footer reports the minimal overrun.
- **(e) No unconditional hub inclusion.** `selectCommunityAware` / `selectRelevanceWeighted` / `selectPathBased` MUST NOT seed `scGodNodes` from `analysisGodNodes` unconditionally: a hub node is admitted only if it is query-relevant (`matchScore n terms > 0`) or reached by BFS within depth; the `### Hub Nodes` section is omitted entirely when no hub is admitted.
- **(f) Bounded, relevance-filtered hints.** `formatExpansionHintsBudgeted` caps suggested communities at the top `N` (default `N = 8`) by relevance; omits any community whose member count exceeds `--max-hint-community-size` (default `50`); omits the chat-history community (`chatCommunityId`); omits the whole section when nothing passes.
- **(g) Footer budget-checked.** The footer `- _omitted: <k> nodes, <e> edges_` is emitted iff `totalOmitted > 0`, and only when it itself fits in `B`; otherwise it is dropped (so (a) is never violated by the footer).

> **Concrete surfaces.** `openspec/specs/query-noise-control` (semantic edge filtering, label elision, verdict/hash/ranked nodes); `openspec/specs/query-serving` (single query invocation, budget consumption); `openspec/specs/query-cli-contract` (`--budget` honored uniformly); `UseCase/Query.hs` (`qrespNodes` assembly at `:149,178,221`); refines openspec change `enforce-query-token-budget`.
>
> **Acceptance (GT-Q1, property test).** On the fixture graph, for all `Q, B`: (i) the output parses (JSON or markdown as requested); (ii) `τ(P) ≤ B`; (iii) conservation holds for nodes and edges; (iv) top-node preservation holds (minimal budget still yields the top node); (v) determinism: two runs with the same `Q, B` yield byte-identical `P`; (vi) no hub from an unrelated community is rendered under a focused query. *Scenario tests:* over-budget, minimal-budget (B smaller than best node), normal budget, and empty `M(Q)`.

---

## 6. Complexity Bounds

Let `q = |terms|`, `L_n = |nodeLabel n|`, `D = max degree`, `C = # candidate nodes`, `H = # communities`.

- **Relevance scoring.** `rel(n)` for one node is `O(q · L_n)` worst-case over the label scan (`computeScore` + `fullLabelBoostForTerms`, `Score.hs:173,190`); over the candidate set `O(Σ_{n∈C} q · L_n)`.
- **Canonicality `κ`.** `O(defKind + definedIn + docCodeClass + demote)` per node, where `definedIn` is one adjacency scan `O(deg(n))`; over all nodes `O(N + E)` (each edge inspected a constant number of times). No dependence on `L_n` (GT-Q3 axiom ii).
- **Projection `π`.** `O(N + E)` total; per-node `O(1)` plus per-incident-edge `O(1)`. Serialized bytes per slim node `≤ c` (target `≤ 256 B + truncated label`), independent of `L_n`. Memory on the hot path is `O(budget)`, not `O(N)` — the property that makes sub-linear-in-graph cost possible.
- **Path filter.** `pathGlobFilter` is `O(|C| · |pattern|)` per query (`Index.hs:170`); applied before scoring, so the budget is spent only on in-scope nodes (GT-Q1 conservation uses the scoped `M(Q)`).
- **Budgeted format.** `formatContextForLLMBudgeted` folds the sorted candidate list once, each step one `countContextTokens` (`O(segment length)`); total `O(Σ segment lengths) = O(total rendered size)`, i.e. **linear in the (bounded) output, never in the unbounded input graph**. The sorted order is `rankKey` (§1.4); sorting `C` candidates costs `O(C log C)`.
- **Hint cap.** `formatExpansionHintsBudgeted` sorts communities by relevance `O(H log H)` then caps at `maxHints` (default 8).
- **GT-Q2 size-savings bound.** Let `F = Σ_{n∈M(Q)} c(S(n))` be the full-source cost and `H_id = Σ_{n∈M(Q)} c(I(n))` the identity cost. GT-Q2 guarantees `τ(P) ≤ H_id + O(1)` independent of `F`, so for dense graphs where source snippets dominate, the asymptotic savings are `Θ(F) → Θ(H_id)`. This is what resolves F1 (output explosion) and F2 (reload cost): reload now reads `G_st = O(N+E)` with bounded per-node constant instead of the fat graph.

---

## 7. Determinism Lens (GT-side)

All three surfaces are deterministic functions of `(graph, terms, budget, config)`:

- `rel` is pure on `(terms, nodeLabel-token, index position)` with stable index tie-break; no `IO`, no RNG (`Query.hs:149,178,221`).
- `κ` is pure on `(nodeKind, nodeFileType, nodeSourceFile, adjacency)`; no source text; idempotent under `π` (GT-Q3 axiom ii).
- `rankKey` (§1.4) is a total function of the above; therefore the sorted candidate list, the drop suffix, and the rendered payload are all deterministic.
- `pathGlobFilter` and the hint cap are pure string/arithmetic operations.
- Truncation order uses the total order induced by `rankKey`, so identical inputs yield byte-identical payloads — subject to CT proof (JOINT-Q) that this total order is representation-independent with respect to `Data.Map` insertion/iteration. The GT claim is the combinatorial totality and functionality; the CT claim is the representation independence (AVI-566).

> **Property tests.** (D1) Same `(graph, terms, budget)` run twice ⇒ byte-identical payload (determinism). (D2) Two `Map`-insertion permutations of the same source graph ⇒ byte-identical slim output (JOINT-Q AC-4). (D3) Nodes with equal `rel` and equal `κ` always resolve by ascending `NodeId` (tie-break).

---

## 8. Acceptance-Criteria Mapping (issue checklist)

- **AC-1 (output-size contract).** `τ(P) ≤ B` with lowest-rank-first truncation, top-node preservation, omitted footer, conservation — GT-Q1 (a)–(g). *Check:* scenario tests for over-budget, minimal-budget, normal, and empty-candidate cases; property test over all `Q, B` on the fixture graph.
- **AC-2 (canonicality score).** `κ ∈ [0,1]`; definition node ranks above test/wrapper under equal relevance; structure-only; deterministic tie-break — GT-Q3 axioms + property test `structuredLogger`. *Check:* property tests on bounds + ranking + label-length mutation invariance.
- **AC-3 (store split / scoping).** Budgeted listing carries identity only (`π`); `--path` restricts candidates before traversal; round-trip lossless; `|G_st| = O(N+E)` with bounded per-node constant; no slim field depends on source length — GT-Q2. *Check:* path-filter scenario + identity-only listing assertion + complexity assertion.
- **AC-4 (determinism).** Identical graph + query ⇒ identical payload; insertion-order invariance of slim output — GT-Q1/JOINT-Q. *Check:* permutation-invariance property test (JOINT-Q AC-4, shared with AVI-566).
- **AC-5 (complexity).** Budgeted format linear in rendered size; `κ` linear in `N+E`; path filter before traversal — §6. *Check:* complexity assertions on `formatContextForLLMBudgeted` and `pathGlobFilter`.

---

## 9. Spec-vs-Implementation Gap Notes (for Graphos Dev)

| # | Note | Surface | Action |
|---|---|---|---|
| G1 | `fullLabelBoost` vs `fullLabelBoostForTerms` — two overlapping functions; query path uses the multi-term one. Confirm collapse. | `Score.hs:181,190` | dedup + assert equal behavior (existing AVI-565 G1) |
| G2 | REQ GT-Q1(e): verify `scGodNodes` in `SelectContext` filters by relevance/depth rather than taking `analysisGodNodes` wholesale. | `SelectContext.hs:174,223` | confirm filter present (existing G2) |
| G3 | REQ GT-Q2 round-trip: confirm the truncated preview is a *projection* of the full snippet and a fetch path reconstructs the node byte-exact; `nodeSignature` classification as content is the arbitrable judgment of AVI-566 M-4. | `Report.hs` / format layer | add round-trip test (existing G3) |
| G4 | `countContextTokens` heuristic (`~1.33 × word-count`); contract `τ(P) ≤ B` holds on the *estimate*, not true token count. Document estimator error bound. | `FormatContext.hs` | document (existing G4) |
| G5 (new) | **Canonicality score `κ` is not yet implemented.** There is no `canonicality`/`κ` function today; `rel` (`Score.hs`) is the only scorer. Insert `κ` into `Domain.Graph.Score` (or a sibling module) reading only structure columns, and feed `rankKey` into `Query.qrespNodes`. Definition-kind / defined-in / doc-code / test-wrapper detection rules need extractor agreement on what counts as a "definition kind" and a "wrapper". | `Score.hs`, `Query.hs:149,178,221`, `Domain.Graph.Index` | new function + unit tests (AC-2) |
| G6 (new) | **Test/wrapper/generated detection** today is implicit (file-path heuristics in queries). Make it a named, deterministic predicate `isTestOrWrapper :: Node -> Bool` reading only `nodeSourceFile`/`nodeKind`, so `κ` and the demotion are single-sourced. | `Node.hs` predicates | new named predicate |
| G7 (new) | **Slim store `G_st` is not yet a distinct type.** Today `Graph` carries `nodeLabel`/`nodeSignature` everywhere. Either introduce a `SlimNode`/`SlimGraph` newtype (preferred for hot-path typing per GT-Q2 obligation 4) or document that hot-path reads a typed projection view. Without the type, obligation 4 cannot be enforced by the compiler. | `Domain.Types.Node`, `Domain.Types.Graph` | type or view |

---

## 10. Proven / Assumed / Open

- **Proven (GT-side, within this document).** Definitions §1 (π, κ, rankKey); conservation relation of GT-Q1(c); complexity bounds §6 (scoring, κ, projection, format); axiom (ii) / axiom (iii) implication that κ survives π; (d) top-node preservation argument (accumulator semantics).
- **Assumed (🔧 Dev).** G2 hub-filter presence under REQ GT-Q1(e); G3 round-trip (projection vs replacement); G5/G6 new `κ` implementation and `isTestOrWrapper`; G7 slim type introduction. Each needs Graphos Dev confirmation before the corresponding requirement is finalized as behavior (per consolidated §3 arbitration discipline).
- **Open (needs external decision).** (O1) The exact classifier for "definition kind" and for "test/wrapper/generated" (G5/G6) — the Head of R&D / Head of Graphos arbitrates the predicate vocabulary, since it touches extractor output. (O2) Whether the scalar `F = rel + κ/1000` shortcut is permitted at all, or tooling sorts strictly on the `rankKey` tuple — recommend tuple only. (O3) The `κ` (canonicality) vs `rel` (relevance) primary-vs-secondary ordering under Axiom (i): this document fixes `rel` primary, `κ` secondary (§1.4); any reversal requires re-arbitration because it changes the `structuredLogger` acceptance test's expected top hit. If a judgment above confidence is needed, comment on this issue for head-rnd before blocking (per execution contract).

---

## 11. Surface Grounding (line references)

| Topic | Surface | Location |
|---|---|---|
| Token estimator / budgeted format / footer | `countContextTokens`, `formatContextForLLMBudgeted`, `formatExpansionHintsBudgeted` | `src/Graphos/UseCase/FormatContext.hs:12,50-129,223` |
| Relevance score assembly | `normalizeScore`, `fullLabelBoostForTerms`, `computeScore` | `src/Graphos/Domain/Graph/Score.hs:166,173,181,190` |
| Score used in query | `scoredNodesSorted`, `qrespNodes` | `src/Graphos/UseCase/Query.hs:149,178,221` |
| Strategy relevance + hub seeding | `matchScore`, `scGodNodes` | `src/Graphos/UseCase/SelectContext.hs:138,174,192,223,241` |
| Path filter | `pathGlobFilter` | `src/Graphos/Domain/Graph/Index.hs:170` |
| Node identity tuple (12 fields) | `Node` record | `src/Graphos/Domain/Types/Node.hs:120` |
| Edge identity (relation/weight/confidence) | `Edge` record | `src/Graphos/Domain/Types/Edge.hs:33,77,88` |
| Graph container | `Graph` record | `src/Graphos/Domain/Types/Graph/Core.hs:47` |
| Canonicality score (new, not yet implemented) | (G5) | *to be added* to `Score.hs` / new module |
| Slim store type (new, not yet implemented) | (G7) | *to be added* to `Domain/Types/Node.hs` |

**Specs grounded:** `openspec/specs/query-noise-control/spec.md`, `openspec/specs/query-serving/spec.md`, `openspec/specs/query-relevance-scoring/spec.md`, `openspec/specs/query-scoping/spec.md`, `openspec/specs/query-cli-contract/spec.md`, `openspec/specs/node-schema/spec.md`, `openspec/specs/graph-json-contract/spec.md`, and the change proposals `enforce-query-token-budget`, `jgf-graph-serialization`, `fix-runtime-ram-crash`.

**Sibling document (CT half):** `docs/math-requirements/AVI-563-query-ergonomics-category.md` (AVI-566) — `P` as idempotent endofunctor/coreflection, colimit preservation `P(merge(V_i)) ≅ merge(P(V_i))`, no-loss over the 12-field column set, and the representation-independence proof of JOINT-Q. The GT half (this doc) fixes the *definition of the order*; the CT half proves it is well-defined.

---

## 12. Next Actions (Graphos-Dev + GT follow-up)

1. **Implement property tests** AC-1..AC-5 (§8) against the fixture graph; names: `prop_output_size_hard_cap`, `prop_output_conservation`, `prop_top_node_preserved`, `prop_canonicality_structured_logger`, `prop_slim_store_no_label_dependence`, `prop_insertion_order_invariance`.
2. **Resolve G5/G6** with the extractor owner: fix the `defKind` and `isTestOrWrapper` predicate vocabulary, then implement `κ`.
3. **Resolve G7**: introduce the `SlimNode`/`G_st` type so obligation 4 (hot-path restriction) becomes a compile-time guarantee rather than a convention.
4. **Loop JOINT-Q with AVI-566**: CT proves `rankKey` is representation-independent once GT defines it; confirm the byte-identical-slim-output determinism test holds end to end.
5. **Escalate** (O1)/(O2)/(O3) (§10) to head-rnd if either expert or Dev needs arbitration before finalizing.

---

## 13. Definition of Done

- [x] Three requirements (GT-Q1 §5, GT-Q2 §2, GT-Q3 §3), each with statement + concrete surfaces + checkable acceptance criterion.
- [x] JOINT-Q stated verbatim (§4), paired with the CT expert (AVI-566).
- [x] Notation reused from the consolidated glossary §0 (§0); no invented names for existing objects (only GT-local helpers `π`, `κ`, `rankKey`, `τ`, §1).
- [x] Complexity bounds concrete and data-structure-grounded (§6).
- [x] Every requirement mapped to a concrete Graphos surface (§5/§2/§3/§11).
- [x] Proven/assumed/open tagged (§10); gaps flagged for Dev (§9).
- [ ] Property tests AC-1..AC-5 implemented by Graphos-Dev (child follow-up).
- [ ] G5–G7 (new surfaces) landed by Graphos-Dev.
- [ ] (O1)–(O3) resolved (head-rnd arbitration) where a judgment exceeds GT confidence.
