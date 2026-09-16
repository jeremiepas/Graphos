## Why

Graphos consolidates local LSP/tree-sitter/stub extraction views into one context
graph via `Domain.Graph.Core.mergeGraphs`, then re-clusters with Leiden
(`Domain.Community.detectCommunities`). The parent joint issue [AVI-528](/AVI/issues/AVI-528)
requires a *determinism / confluence* guarantee for the pair `(G, gamma)` where
`G = merge(V)` and `gamma = cluster(G)`, but the guarantee is stated only at the
behavioural level ("two view orders yield isomorphic output"). Without a
category-theoretic specification there is no way to (a) state precisely *when*
confluence holds, (b) separate the clean coproduct case from the conflict case,
or (c) prove that `merge × cluster` is genuinely non-commutative rather than
merely appearing so. The CTO (note 3) explicitly requires coverage of all three
cases — consistent coproduct, last-write-wins coequalizer, and `merge × cluster`
non-commutativity — not only the clean case. This change supplies the category
half (AVI-556): the categorical objects/morphisms, the universal properties, and
the SHALL requirements M1/M2/M4/M5 (with the canonical form backing M3), each
grounded in a concrete Graphos surface.

## What Changes

- Introduce a shared **category-theoretic notation** for Graphos merge/cluster
  (`GraphCat`, view diagram `D : I -> GraphCat`, cocone `λᵢ`, colimit `⟪colim D⟫`,
  coequalizer for conflicts), agreed with the graph half on [AVI-528](/AVI/issues/AVI-528).
- Specify **`merge = colim D`** as the coproduct of consistent views with its
  universal property (unique mediating morphism `⟪μ⟫ : colim D -> G`).
- Model **last-write-wins conflicts on `NodeId`** as a **coequalizer**, not a
  coproduct, with divergence quantified by the disagreement-set size.
- Prove **confluence** (`merge` order-independent up to iso) for consistent views
  via the universal property, and prove **`merge × cluster` non-commutativity**
  under conflicts (Leiden is deterministic but not a functor preserving coproducts).
- Assert **idempoteness** `A +-+ A ~= A` separately (not derived from commutativity).
- Ground every claim in `Domain/Graph/Core.mergeGraphs`, `UseCase.Extract.*`
  (view diagram `D`), `Domain/Community.detectCommunities`, `Domain/Graph.Core.computeGraphHash`.

## Capabilities

### Modified Capabilities
- `09-merge` — add the categorical confluence/determinism requirements (M1–M5) to
  the merge workflow spec; no behavioural change to `mergeGraphs` itself.
- `lsp-extraction` / `semantic-edge-inference` / `ingest-index` — the view family
  `V` indexed by the extraction source is the diagram `D : I -> GraphCat`; the
  requirements constrain only the *merge* stage, not extraction.

## Classification

- **Category-theoretic specification** (requirements-only; no Haskell change).
- The five SHALL requirements are **new requirements** on the merge/cluster stage.
- Feasibility gate: [Haskell Developer / Graphos-Dev](/AVI/agents/devhaskell) must
  sign off before any requirement is archived, since M1/M2/M3 constrain the pure
  `mergeGraphs` / `detectCommunities` functions.
