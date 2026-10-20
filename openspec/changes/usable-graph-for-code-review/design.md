## Context

The 2026-10-08 review of solario-core PR #1357 (vault bug report
`note/personal/graphos/bug-report-2026-10-08-solario-core-pr1357.md`) exercised the read path of
Graphos on a 129,839-node graph and found it unusable for its purpose. Every symptom traces to a
small number of code sites, all confirmed by reading the source and by a control run on
`example/ts-lsp-test` (111 nodes):

| Symptom (bug report) | Root cause | Layer / site |
|---|---|---|
| B1 ids and labels embed source text; module id = whole file; graph.json 2.7 GB | `makeNode` uses `tsNodeUntruncatedLabel` (the node's full normalized text) as both label and id input; `makeNodeId` concatenates it; `contains` edge ids concatenate two such ids | Infrastructure — `Extract/TreeSitter/Convert.hs` |
| B2 `symbols dbGetById` → "No symbol found" (also on the 111-node corpus) | `symbolLookup` is an exact match on the label index, and labels are declaration texts | Domain `Graph/Index.hs`, UseCase `Query.hs` |
| B5 `explain`/`path` answer on an unrelated node | `explainNodeWithIndex` / `pathQueryWithIndex` use `findBestNodeWithIndex` (tokenized best score) instead of `resolveNodeArg` | UseCase `Query.hs`, `app/Main.hs` |
| B4 no path across files; `imports` edges end on stems | Import target id = `makeNodeId targetPath targetName` (a placeholder `External` node) while the target file's module id embeds its whole text; the two never coincide | Infrastructure `Convert.hs` |
| `0_external_## Examples by kind …` nodes | `extractSpecifier` re-parses `tsnText` with `breakOnEnd "from "`, so any text containing `from ` (template literals) becomes an import | Infrastructure `Convert.hs` |
| B3 cypher `WHERE` finds nothing on the big graph but works on the small one | `evaluate` applies `takeCap budget` to the raw bindings before the `WHERE` filter (2,000 default) | Domain `Query/Cypher/Eval.hs` |
| B3 `MATCH p = …` parse error at `p` | grammar has no named path; error is positional, not named | Domain `Query/Cypher/Parser.hs` |
| B6 `granularity: file` logged, symbol-level nodes emitted | `Wiring.hs:197` calls `tsNodesToExtraction GranularityFunction …`, ignoring the level resolved by `granularityForFile` | Infrastructure `Wiring.hs` |
| B7 `traces/` written despite `--no-observability` | `app/Main.hs` computes `debugDir` from config regardless of the flag and `newDebugTraceEnv (logLevel <= LogDebug)` enables it whenever `debug: true` | `app/Main.hs`, Infrastructure `Observability/SDK.hs` |
| B8 29.6 MB report, 18,894 articulation rows | `bridgeNodesSection` renders every articulation point with its raw id | UseCase `Report.hs` |
| B10 151,689 → 136,340 edges, unexplained | `buildGraph` drops unknown-endpoint edges and the `(source,target)`-keyed edge map collapses duplicates, silently | Domain `Graph/Core.hs`, UseCase build |

Constraints: Domain stays IO-free; `Domain.Types.Node` is not changed (`nodeLabel` and
`nodeSignature` already exist, PRD §4.1); the extraction cache key already factors the extraction
configuration (change `wire-incremental-update`), so invalidation is a fingerprint bump, not a
migration; the active change `cypher-eval-graphindex` plans index-anchored candidate lookup for
cypher — this change stays compatible by fixing filter order only and anchoring `id` equality,
which that change can extend.

## Goals / Non-Goals

**Goals:**
- A reviewer can copy an id from `graph.json` or type a declared name and get the right node from
  `symbols`, `explain`, `path` and `neighbors`, or an explicit *not found* / *ambiguous*.
- `imports` edges connect module nodes across files so `path` answers reachability questions.
- `graph.json` and `GRAPH_REPORT.md` no longer duplicate source text; a 130k-node graph loads in
  seconds, not 40 s.
- `cypher` property filters work at any graph size; named paths fail with an actionable message.
- `--no-observability` and `granularity: file` do what they say.

**Non-Goals:**
- A persistent query index or daemon (read commands still load `graph.json`; `graphos serve`
  remains the answer for repeated queries).
- `length(p)` / named path support in cypher.
- Changing Leiden, inference, HTML viewer behaviour or the Neo4j/Memgraph push modes (they
  consume ids opaquely and re-hash on push).
- LSP-based extraction (tree-sitter is the default and the only path exercised by the report).

## Decisions

### D1: Identity = (path, kind, identifier, collision rank); label = identifier; text → signature

Id layout: `<dirHash>_<stem>_<kind>:<identifier>[#L<line>]`. Keeps the existing directory hash
and stem (so `makeNodeId` callers and the markdown extractor's `<hash>_h<level>_<title>` scheme
stay in the same family), adds the kind to separate a type from a function of the same name, and
suffixes the start line only when a collision exists within the file, so the common case is
line-independent and survives edits above the declaration. The identifier is the first named
`identifier`-class child of the declaration (`identifier`, `type_identifier`,
`property_identifier`, `field_identifier`, `name`), resolved once in `Convert.hs`; import and
re-export nodes use the resolved specifier; module nodes use the file name. Wrapper nodes without
an identifier (`export_statement`, `decorated_definition`) are not emitted; their inner
declaration is, with an `exported` marker in `nodeExtra`. The normalized declaration text goes to
`nodeSignature`, bounded by the existing named truncation budget.

| Alternative | Why not |
|---|---|
| Keep text-derived ids, add a separate `identifier` field | Leaves the 2.7 GB graph and the unusable CLI arguments in place; adds a Domain field (spec change on `domain-types`) |
| Always suffix the line number | Every edit above a declaration changes its id; breaks `--update` reuse and cross-run diffs |
| Hash the declaration text into a short id | Opaque, still changes on every body edit, and gives agents nothing to type |
| Resolve identifiers via LSP `documentSymbol` | Reintroduces the LSP dependency the project moved away from; tree-sitter children already carry the name |

Layers: Infrastructure only (`Extract/TreeSitter/Convert.hs`); `Domain.Graph.Index` needs no change
because the label index becomes an identifier index by construction.

### D2: Import edges target the module node computed from the resolved path

With D1, the module node id of a file is a pure function of its path, so `Convert.hs` computes the
target id from the resolved specifier path and emits no placeholder for relative imports that
resolve to an extracted file; the placeholder stays only for package/builtin specifiers (one
canonical external node per package, as the spec already requires). The specifier comes from the
declaration's string-literal child, which the tree-sitter tree exposes, replacing the
`breakOnEnd "from "` scan. The build stage counts unknown-endpoint and duplicate-key drops and logs
them once.

| Alternative | Why not |
|---|---|
| Post-build pass that merges `External` placeholders into module nodes by `source_file` | Works, but keeps two ids for one file in extraction outputs and caches; D1 makes the merge unnecessary |
| Keep re-parsing the label but filter on node type | Still fragile for multi-line and aliased imports; the spec already forbids label re-parsing |

Layers: Infrastructure (`Convert.hs`), UseCase build logging (`Extract/Core.hs` / pipeline build
step), Domain unchanged.

### D3: `explain` and `path` adopt `resolveNodeArg`; fuzzy stays in `query`

`explainNodeWithIndex` and `pathQueryWithIndex` are replaced by resolution through
`resolveNodeArg` followed by the existing explain/path computations; the CLI dispatcher reuses the
`neighbors` rendering of `NotFound` (suggestions, non-zero exit) and `Ambiguous` (candidate list).
`symbolLookup` is unchanged in shape; it works once labels are identifiers.

| Alternative | Why not |
|---|---|
| Keep fuzzy fallback behind a `--fuzzy` flag | More surface; `query` already is the fuzzy command, and a review tool that guesses silently is the defect being fixed |
| Resolve by `source_file` path too (e.g. `explain ./src/db.ts`) | Useful, but the path index would need disambiguation rules; the file name identifier of module nodes covers the review case |

Layers: UseCase (`Query.hs`), CLI dispatch (`app/Main.hs`), Domain unchanged.

### D4: Cypher filters lazily before the cap; `id` equality is answered from the node map

`evaluate` becomes: enumerate bindings lazily → filter by `WHERE` → `takeCap budget`. A `WHERE`
that is an equality on `id` (or an `id` property constraint in the pattern) anchors the candidate
list on `Map.lookup`, mirroring what `mergeNode` already does for mutations. Named path variables
get a dedicated parse error. Index anchoring for `label` and `source_file` is left to
`cypher-eval-graphindex`, which this change does not conflict with.

| Alternative | Why not |
|---|---|
| Raise the default budget | Hides the ordering bug and makes broad matches slower |
| Implement named paths and `length()` now | Scope; `graphos path` covers the reviewer's need |

Layers: Domain (`Query/Cypher/Eval.hs`, `Parser.hs`).

### D5: `--no-observability` disables debug tracing; trace dir resolves under the output dir

`app/Main.hs` sets the debug trace enable flag to false when `--no-observability` is given, and
resolves a relative `debugTraceDir` against `cfgOutputDir`; the default moves from
`<out>/traces` to `<out>/debug` to match the spec. `SDK.hs` keeps its lazy directory creation.

| Alternative | Why not |
|---|---|
| Only move the directory, keep tracing on | The flag's documentation ("Disable all observability") would stay false |
| Delete the global config default | Operator choice; the flag must win regardless of config |

Layers: `app/Main.hs` (composition), Infrastructure `Observability/SDK.hs` unchanged in behaviour.

### D6: Bridge table bounded and labelled

`bridgeNodesSection` takes the top `bridgeTableCap` (named constant, 50) articulation points by
degree after dropping degree ≤ 1, renders label / kind / source file / degree, and appends a
footer with total and omitted counts. Articulation computation itself is unchanged.

| Alternative | Why not |
|---|---|
| Compute articulation points on a file-level projection | Better signal, but changes `Domain.Analysis` semantics and the report-consistency totals; a follow-up |

Layers: UseCase (`Report.hs`).

### D7: Pass the resolved granularity through the FFI wiring

`Wiring.hs` threads the `Granularity` argument of `extractViaTreeSitterFFI` into
`tsNodesToExtraction` instead of the constant. The extraction fingerprint already includes
granularity, so cached entries built with the wrong level miss once the fingerprint also carries
the new id scheme version.

| Alternative | Why not |
|---|---|
| Treat as a documentation gap | The spec and the log line both promise the behaviour; the run shows it is not honoured |

Layers: Infrastructure (`Wiring.hs`).

### D8: Invalidate caches by fingerprint, not by migration

The extraction-config fingerprint gains an `idScheme=2` component. Old cache entries and
checkpoints miss; `graphos . --fresh` rebuilds. No reader of `graph.json` depends on the id
layout (ids are opaque `Text`, `graph-json-contract` schema version unchanged).

### D9: Lean 4 verification instrument before the Haskell mirrors

`lean/ReviewGraph.lean` (Lean 4.29.1 core, no Mathlib) models each decision as a pure function
over structural data and proves the guarantee once; the Haskell code is then written to mirror
the model and the Hspec suite checks parity on the Lean toy fixtures.

| Decision | Lean objects | Theorems (kernel-checked) |
|---|---|---|
| D1 identity | `NodeKey`, `Decl`, `countBefore`, `keyOf`, `assignKeys` | `keyOf_text_free`, `rank_zero_of_unique`, `countBefore_unrelated`, `rank_ge`, `assignKeys_distinct` |
| D2 imports | `moduleKey`, `importTarget`, `placeholderKey`, `builtEdges`, `Reach`, `ImportChain` | `importTarget_eq_moduleKey`, `placeholder_ne_moduleKey`, `resolved_import_kept`, `chain_lifts` |
| D3 resolution | `exactId`/`exactLabel`/`ciLabel`, `outcome`, `firstNonEmpty`, `resolve`, `Matches` | `resolve_single_sound`, `resolve_notFound_sound`, `resolve_ambiguous_sound`, `resolve_exact_id`, `byKey_unique` |
| D4 cypher | `filt`, `tk`, `evalOld`, `evalNew` | `evalOld_subset_evalNew`, `evalNew_complete`, decided counter-example |
| D5 observability | `traceEnabledOld/New`, `resolveTraceDir`, `isPrefixSeg` | `noObservability_disables`, `old_ignores_flag`, `relative_under_output` |
| D6 report | `bridgeRows` (sort as a membership-preserving parameter) | `bridgeRows_length_le`, `bridgeRows_degree` |
| D7 granularity | `resolveGran`, `convertedLevelOld/New` | `cli_wins`, `perExt_wins`, `global_applies`, `wiring_threads_level`, `wiring_bug_refuted` |

The id render function and the case-normalisation are section parameters, so the resolution
theorems hold for any injective rendering and any normaliser; the sort is a parameter constrained
only by membership preservation. Strings appear only as opaque data; every primitive the kernel
must reduce (`filt`, `tk`, `concatMap`, `Distinct`, `isPrefixSeg`) is a structural helper, per
`lean-proof-methodology`.

| Alternative | Why not |
|---|---|
| QuickCheck properties only | Properties sample; the id-distinctness and chain-lifting claims are universally quantified and cheap to prove once |
| Mathlib (`List.Nodup`, `List.Perm`, `Relation.ReflTransGen`) | Breaks the core-only toolchain pin shared by every change artifact |
| No formal artifact | The bug report is the result of claims (spec `import-resolution`, "canonical target identity") that were never checked against a model |

Layers: the model is layer-free by construction; it constrains Infrastructure (`Convert.hs`,
`Wiring.hs`), UseCase (`Query.hs`, `Report.hs`) and Domain (`Cypher/Eval.hs`) mirrors alike.

## Risks / Trade-offs

- [Identifier collisions across kinds that share a name, e.g. a `type User` and a `const User`] →
  the kind is part of the id; `symbols User` returns both as `Ambiguous` with kinds, which is the
  desired behaviour.
- [Grammars whose declarations have no identifier child (anonymous functions, arrow functions
  assigned to `const`)] → the enclosing declaration (`lexical_declaration` / `variable_declarator`)
  carries the name; anonymous nodes inside bodies are already excluded at `function` granularity.
- [Embedding and labeling caches are keyed on text; label shrink changes every embedded text] →
  one-time re-embedding on the next `--embed` run; documented in the change notes.
- [Neo4j/Memgraph graphs pushed before the change keep old ids] → next `push` re-hashes; the
  neo4j-integration spec already tolerates re-pushes.
- [Downstream tooling that greps ids in `graph.json` (solario bench Neo4j push skill)] → ids are
  shorter and still contain the file stem; the skill reads ids opaquely.
- [`contains` edge ids shrink but keep the `->…:contains` layout] → unchanged readers.
- [Filtering before the cap makes a non-selective `WHERE` scan the whole graph] → this is the
  documented semantics of a budget on results; broad matches without `WHERE` are unchanged.

## Migration Plan

1. Land D7, D5, D6 and D4 first (independent, no id change) — each verifiable with `cabal test`.
2. Land D1 + D2 + D8 together with the delta specs; run `graphos . --fresh` on
   `example/ts-lsp-test` and on this repository; record node/edge counts and `graph.json` size
   in the change notes.
3. Land D3 and the `identifier-resolution` CLI outcomes; regenerate the agent skill
   (`install-skill`) so its examples use identifiers and ids of the new layout.
4. Rollback: revert the Convert/Wiring commits and run `--fresh`; the fingerprint component
   returns to its previous value so old caches hit again.

Verification: `cd lean && lake clean && lake build` with the pinned Lean 4.29.1 toolchain
(`lean/VERIFICATION.md`) exits 0 with zero errors, zero warnings and zero `sorry` before any
Haskell mirror is written; `cabal build` (with `-Wall -Werror` under `--flag dev`) and
`cabal test` for every step; the integration scenarios on `example/ts-lsp-test` (path across imports, one node per file at
`file` level, no placeholder targets) are Hspec tests; a manual run on a ≥100k-node repository
measures `graph.json` size and the wall time of `graphos symbols` before and after.

## Open Questions

- Should module nodes keep the `Module` kind name in the id (`Module:db.ts`) or use the bare file
  name? Proposed: keep the kind for uniformity; `symbols db.ts` resolves by identifier either way.
- Does any consumer rely on `External` placeholder nodes for relative imports (HTML viewer
  layering in `UseCase/Subgraph.hs` maps `external` tiers)? Proposed: the tier mapping keys on
  package nodes only; confirm during task 2.
