# Graph Theory — Context Similarity Scoring (Embedding + Cosine)

**Owner:** Graph Theory Expert (`graph-theory-expert`) — [AVI-520](/AVI/issues/AVI-520)
**Parent:** [AVI-512](/AVI/issues/AVI-512) — Graphos math requirements, rev 2
**Synthesizes:** [AVI-510](/AVI/issues/AVI-510) modularity notation; `openspec/specs/semantic-edge-inference/spec.md`; `openspec/specs/embedding/spec.md`
**Status:** requirements + concrete deliverable. No Haskell is written here; behavior-constraining requirements carry a required loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing.

---

## 0. Scope and provenance

This doc constrains the **context-similarity scoring** surface of Graphos: scoring a
`DocFile` node against `CodeFile` nodes by **cosine similarity of their embedding vectors**,
and turning the top-scoring pairs into inferred `References` edges. The single concrete
implementation is `inferSemanticCodeDocEdges` in `src/Graphos/UseCase/Infer.hs`, driven by
`SemanticEdgesConfig` in `src/Graphos/Domain/Config/Vision.hs`, over the embedding map held
in `gEmbeddings` on the `Graph` type. The same cosine primitive backs the ingest-index
vector search (`UseCase.IngestIndex`); this doc constrains the *math* of that primitive so
both call sites share one well-defined score.

Notation from the unified glossary in [AVI-510](/AVI/issues/AVI-510) §0 is reused verbatim:
`NodeId = Text`, `Node`, `Edge { edgeSource, edgeTarget, edgeRelation, edgeWeight,
edgeConfidence, edgeExtra }`, `neighbors / degree`, `V = Map.keys (gNodes g)`, `|V| = N`.
Line numbers below are verified against the checked-in sources on branch
`AVI-512-add-math-in-graphos-project`.

---

## 1. Definitions

**1.1 Node-type partition.** `· : V → F`, where `F = {CodeFile, DocFile, PaperFile,
ImageFile, VideoFile, AudioFile, OfficeFile}` (`Node.hs`), is a total function; each node
carries exactly one type. Semantic scoring restricts attention to two classes: documents
(`DocFile`) and code (`CodeFile`).

**1.2 Embedding map.** `emb : Map NodeId [Double]` (`gEmbeddings`, `Graph/Core.hs:55`). It is
a *partial* map over `V`. A node `v` is **embeddable** iff `Map.lookup v emb = Just vec` with
`not (null vec)`. Write `V_e = { v ∈ V : v embeddable }`. All vectors in `V_e` share a
dimension `d = length vec` in practice (§5, INV-A enforced at load).

**1.3 Cosine similarity.** On `ℝᵈ× = ℝᵈ \ {0}` define
`cos(a, b) = ⟨a, b⟩ / (‖a‖ · ‖b‖)`. Graphos applies the boundary convention `cos = 0` when
either vector is the zero vector (LLMPort `cosineSimilarity`, `UseCase/Port/LLMPort.hs:52`;
`Infrastructure/LLM/Embedding.hs:84`). This is a *defined convention for an undefined ratio*
(0/0), not the raw cosine — see §5 Open-1.

**1.4 Bipartite scoring relation.** Let `D = { v ∈ V_e : ·v = DocFile }` and
`C = { v ∈ V_e : ·v = CodeFile }`. Because `·` is a function, `D ∩ C = ∅`. For threshold
`τ = seThreshold` and fan-out `k = seMaxFanOut` (`Config/Vision.hs:114`), define for each
`d ∈ D`:

```
score_d(c) = cos(emb d, emb c)          for c ∈ C
C_d        = { c ∈ C : score_d(c) ≥ τ }          (threshold filter)
Rank_d     = the ≤k elements of C_d with largest (−score_d, c)   (top-k under NodeId tie-break)
```

The inferred edge set (after dedup by `(edgeSource, edgeTarget)`) is:

```
E_sem = { Edge(edgeSource = c, edgeTarget = d, edgeRelation = References,
               edgeConfidence = Confidence (score_d c)) : d ∈ D, c ∈ Rank_d }
```

Direction is **code → doc** (`makeInferredEdge codeNid docNid References sim`, `Infer.hs:243`);
confidence equals the cosine score (`Infer.hs:263`).

**1.5 Scored bipartite graph.** `H = (D ∪ C, E_sem)` is a bipartite multigraph with all edges
crossing `D | C`. Out-degree of a doc node `d` in `H` equals `|Rank_d| ≤ k`.

---

## 2. Invariants (QuickCheck-ready)

Every invariant below is stated as a predicate `[Graph -> Bool]` that [Graphos Dev](/AVI/agents/graphos-dev)
can turn into a property test against `inferSemanticCodeDocEdges`.

- **INV-A (Dimension consistency).** `allSameLength (Map.elems emb)` — every vector has length `d`.
  *(Grounds Thm 1's domain and the two-cosine agreement, §5 Open-1.)*
- **INV-B (Cross-partition / no self-loop).** `all (\e -> nodeFileType s == CodeFile && nodeFileType t == DocFile) edges`, where `(s,t) = (edgeSource e, edgeTarget e)`. Implies `s ≠ t`.
- **INV-C (Fan-out cap).** For every doc node `d`, `length [e ∈ E_sem : edgeTarget e == d] ≤ k`.
- **INV-D (Threshold).** `all (\e -> confidence >= τ) E_sem`. Combined with Thm 1, `confidence ∈ [τ, 1]`.
- **INV-E (Dedup).** No two edges in `E_sem` share `(edgeSource, edgeTarget)`.
- **INV-F (Embedding presence).** If `Map.lookup d emb ∈ {Nothing} ∪ [^nonempty]`, then `d` has out-degree 0 in `E_sem`.
- **INV-G (Confidence well-defined).** `all (\e -> (-1) <= confidence && confidence <= 1) E_sem`.
- **INV-H (Determinism).** For structurally equal inputs `(G, emb, τ, k)`, `inferSemanticCodeDocEdges` returns equal edge lists (order-normalized).

---

## 3. Theorems and lemmas (with proof sketches)

**Thm 1 (Range and equality condition).** *For all `a, b ∈ ℝᵈ×`, `cos(a,b) ∈ [−1, 1]`, and
`cos(a,b) = 1 ⟺ ∃ λ > 0. a = λb`.*
**Proof.** Cauchy–Schwarz gives `|⟨a,b⟩| ≤ ‖a‖‖b‖` with equality iff `a, b` are linearly
dependent; positivity of the denominator normalizes to `[−1,1]`, and `cos = 1` forces the
proportionality constant positive. ∎
**Corollary (boundary).** On the zero vector the ratio is `0/0` (undefined); Graphos assigns
`0`. A zero vector carries no direction, so assigning `0` neither inflates a true match nor
expresses opposition — safe, and never reached for embeddable nodes because the null-guard
(`not (null e)`, `Infer.hs:250`) excludes empty vectors before scoring. **Status:** Proven.

**Thm 2 (Bipartite structure, INV-B).** *Every edge in `E_sem` goes from a `CodeFile` node to
a `DocFile` node; no self-loop and no same-class pair.*
**Proof.** The comprehensions build `codeWithEmb` from `nodeFileType == CodeFile` and
`docWithEmb` from `== DocFile` (`Infer.hs:246-249`). Since `·` is total and single-valued, a
node lies in at most one class, so `D ∩ C = ∅`; every emitted edge has source ∈ C, target ∈ D,
hence source ≠ target. ∎ **Status:** Proven.

**Thm 3 (Determinism / unique top-k, INV-H).** *Fix `(G, emb, τ, k)`. If the tie-break is the
strict total order `(−score_d, c)` on `NodeId`, then `Rank_d` is unique and `E_sem` is a pure
function of the inputs, independent of `Map` insertion order.*
**Proof.** `Data.Map.Strict.toList` yields keys in ascending order and `sortOn` is stable, so
the implementation already breaks ties by least node id — but only *incidentally*. Stating the
tie-break explicitly as `(−score_d, c)` makes the selection key injective in `c` (node ids are
unique), so `(−score_d, c)` is a strict **total** order; taking the `k` least elements under a
total order returns a unique subset. Hence the whole computation is a pure function of
`(G, emb, τ, k)`. ∎ **Status:** Proven *given the explicit tie-break* (Assumed 🔧 Dev; current
code achieves it implicitly via stable sort + Map ordering).

**Thm 4 (Termination).** *`inferSemanticCodeDocEdges` terminates for any finite graph.*
**Proof.** The function is a finite composition: (i) two filters over the finite `gNodes`
(`|V|` steps); (ii) a pairwise double-comprehension over `D × C` (`|D|·|C|` cosine evals, each
finite); (iii) a per-doc stable sort over ≤ `|C|` elements; (iv) `take k`; (v) a dedup fold over
the finite candidate list. Every structure iterated is a subset of the finite sets `V` or
`D × C`, so every loop is bounded; no recursion with a changing state occurs, and no strictly
decreasing measure is required — termination holds by a **bounded-iteration argument**. ∎
**Status:** Proven.

**Thm 5 (Fan-out and threshold soundness / optimality, INV-C + INV-D).** *For each `d ∈ D`:
`|Rank_d| ≤ k`, every retained score satisfies `τ ≤ score_d ≤ 1`, and among all `S ⊆ C_d` with
`|S| ≤ k`, `Rank_d` maximizes `Σ_{c ∈ S} score_d(c)`.*
**Proof.** `filtered` keeps only `score_d ≥ τ` (`Infer.hs:259`) ⇒ lower bound `τ`; Thm 1 gives
upper bound `1`. Selecting the `k` largest elements of a finite set is the classic greedy
solution to the max-sum cardinality-≤`k` problem; any other feasible `S` has sum ≤ that of the
`k` largest. ∎ **Status:** Proven.

**Thm 6 (Confidence bound, INV-D + INV-G).** *For every `e ∈ E_sem`, `edgeConfidence e ∈ [τ, 1]`
(using `τ ≥ 0`), and generally `∈ [−1, 1]`.*
**Proof.** Confidence is assigned the retained score (`makeInferredEdge ... sim`, `Infer.hs:263`);
Thm 5 bounds retained scores by `[τ, 1]` and Thm 1 by `[−1, 1]`. ∎ **Status:** Proven.

**Thm 7 (Dedup cardinality, INV-E).** *`E_sem` contains at most one edge per `(code, doc)` pair.*
**Proof.** `dedupOn (\e -> (edgeSource e, edgeTarget e))` (`Infer.hs:267`) collapses all edges
with equal endpoint-pairs to one via `Map`/union semantics. ∎ **Status:** Proven.

---

## 4. Complexity bounds

Model: `N_d = |D|`, `N_c = |C|`, `d` = embedding dimension. Data-structure grounding follows
the CSR/adjacency-map vocabulary of the `leiden-scalability` and `bounded-edge-inference` specs.

| Stage | Cost | Grounding |
|---|---|---|
| Embedding retrieval | `O(1)` per lookup | strict `Map` (`iiNodes` / `gEmbeddings`) |
| Pairwise cosine | `O(N_d · N_c · d)` | `zipWith (*)`+`sum` over `d`, twice per pair (`LLMPort.cosineSimilarity`) |
| Per-doc selection | `O(N_c log N_c)` | `sortOn negate . take k` (`Infer.hs:260`) |
| Dedup | `O(|E_sem|)` | `dedupOn` fold, linear in output |

**Total: `O(N_d · N_c · (d + log N_c))`**, dominated by the pairwise cosine term. A size-`k`
min-heap replaces the per-doc sort and yields `O(N_d · N_c · (d + log k))` — a recommended
optimization, not a correctness change.

**Scale guard.** The spec caps semantic inference at **10 000 `CodeFile` nodes**: when
`N_c > 10⁴` the pipeline falls back to literal-name inference unless `--force-semantic-edges`
is set (`semantic-edge-inference`). This bounds the pairwise term at `~10⁴ · N_d · d`, consistent
with Graphos's other 10K code-node cap. **Memory:** `O(N + |E| + Σ_{v ∈ V_e} d)` for graph plus
embeddings; output `O(Σ_d |Rank_d|) ≤ O(N_d · k)`.

---

## 5. Master Requirements Register

| ID | Requirement | Concrete surface | Acceptance | Status |
|---|---|---|---|---|
| GT-S1 | `cos ∈ [−1,1]`; `=1 ⟺ positively collinear`; zero-norm/absent → `0` by convention (not a defect). | `cosineSimilarity` | Thm 1; INV-G | Proven |
| GT-S2 | `emb` partial over `V`; node embeddable iff `Just`-nonzero; dimension constant (INV-A). | `gEmbeddings`; null-guard | INV-A property | Proven |
| GT-S3 | Scoring relation bipartite `Code→Doc`; no self-loop, no same-class pair (INV-B). | partition in `inferSemanticCodeDocEdges` | INV-B property | Proven |
| GT-S4 | Output is a pure function of `(G, emb, τ, k)`; explicit `(−score, c)` tie-break makes top-k unique (INV-H). | stable `sortOn` + `Map.toList` | Thm 3 | Proven *given tie-break* 🔧 |
| GT-S5 | Termination by bounded iteration over finite `D`, `C` (no decreasing measure). | Thm 4 | structural | Proven |
| GT-S6 | Fan-out `≤ k` and `score ≥ τ` for every edge; top-k is max-sum under cardinality constraint (INV-C, INV-D). | `take k` / `filter sim ≥ τ` | Thm 5 | Proven |
| GT-S7 | `confidence ∈ [τ,1] ⊆ [−1,1]` (INV-D/G). | `makeInferredEdge ... sim` | Thm 6 | Proven |
| GT-S8 | Total `O(N_d·N_c·(d + log N_c))`; pairwise `O(N_d·N_c·d)`; 10K code-node guard caps it. | `Map.lookup` O(1); `zipWith` O(d); `sortOn` O(c log c) | complexity | Proven |
| GT-S9 | Absent/empty embedding ⇒ out-degree 0 for that doc (INV-F). | `not (null e)` filter | INV-F property | Proven |
| GT-S10 | Dedup by `(source,target)`: exactly one semantic edge per pair (INV-E). | `dedupOn` | INV-E property | Proven |

---

## 6. Acceptance criteria (mapped to the spec)

Every scenario in `semantic-edge-inference/spec.md` is realized by a GT invariant or theorem:

| Spec scenario | Constrained by |
|---|---|
| Doc node matches code node by embedding ⇒ `References` edge, confidence = score | Thm 6, GT-S7 |
| Below-threshold match dropped (`0.4 < τ=0.5`) ⇒ no edge | Thm 5 / INV-D |
| Fan-out cap (`80 → top-50`) | Thm 5 / INV-C, GT-S6 |
| Missing/empty embedding skips doc node | INV-F, GT-S9 |
| Single-corpus auto-skip (no `DocFile` ⇒ `D = ∅` ⇒ `E_sem = ∅`) | Thm 2 / INV-B |
| 15K code nodes fall back unless forced | complexity § scale guard |

---

## 7. Feasibility answers

- **Is cosine sound here?** Yes for *angular* similarity of the embedding directions, which is
  exactly "how similar is this document to this code." Range `[−1,1]` (Thm 1); the `0` on a
  zero-norm vector is a defined convention for an undefined ratio, not a defect. Caveat: cosine
  measures direction, not distance — appropriate here, but a near-uniform embedding (all vectors
  nearly parallel) compresses discrimination toward `1`; that is a model-quality concern, not a
  formula error.
- **Complexity?** `O(N_d·N_c·d)` pairwise, plus `O(N_d·N_c·log N_c)` for per-doc selection;
  capped by the 10K code-node scale guard. Reducible to `O(N_d·N_c·(d + log k))` with a size-`k`
  heap.
- **Well-defined for the graph class Graphos produces?** Yes: embeddings are fixed-length
  `Double` vectors keyed by `NodeId`; the only ill-defined case is zero/norm or a dimension
  mismatch (§5 Open-1), which INV-A + the null-guard close.
- **Deterministic?** Yes, given an explicit `(−score, c)` tie-break (Thm 3). The current code is
  deterministic already (stable sort over ascending `Map` keys) but relies on it *implicitly*.

**Open-1 (needs [Graphos Dev](/AVI/agents/graphos-dev)).** Two cosine implementations disagree on
unequal-length input: `LLMPort.cosineSimilarity` truncates via `zipWith` and never checks length,
while `Infrastructure.LLM.Embedding.cosineSimilarity` returns `0`. Semantic inference uses the
`LLMPort` variant (`Infer.hs:35`). **Requirement:** adopt one canonical cosine and enforce INV-A
(equal dimension) at load, treating a mismatch as `0`. Until then, an embedding with a shorter
prefix silently scores on common dimensions only — a well-definedness hazard.

**A1 (Assumed — verify with the embedding producer).** If model vectors are pre-L2-normalized,
the two norm roots in `cosineSimilarity` are redundant and the per-pair cost drops from
`O(2d)` to `O(d)`. Do not assume normalization; confirm or drop the roots.

---

## 8. Required loop with Graphos Dev (before finalizing)

1. Confirm a single canonical cosine and enforce INV-A at load (resolves Open-1). 🔧
2. State the tie-break `(−score_d, c)` explicitly (closes the implicit-dependency gap in Thm 3). 🔧
3. Report whether embeddings are pre-normalized (resolves A1). 🔧
