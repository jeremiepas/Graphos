## Why

A real code review (solario-core PR #1357, 2026-10-08, documented in the vault bug report
`note/personal/graphos/bug-report-2026-10-08-solario-core-pr1357.md`) built a 129,839-node graph in
2.5 minutes and then could not answer the one question the review needed — *does the worker entry
reach `shared-run-command.ts`?* — because every read path is defeated by how nodes are identified:
a node's id and label are its **entire normalized source text** (`Convert.hs makeNodeId` /
`makeNode`: a module node's id is the whole file, a function node's id includes its body, and a
`contains` edge id concatenates both). That single choice makes ids unusable as CLI arguments,
makes `symbols`/`explain` miss any function asked for by name, inflates `graph.json` to 2.7 GB
(40 s and several GB of RAM per read command), and leaves `imports` edges pointing at placeholder
nodes instead of the imported file's module node. Around it, `cypher` applies `WHERE` only after
the 2,000-binding cap, `--no-observability` still writes `traces/` into the working directory, and
`GRAPH_REPORT.md` lists all 18,894 articulation points. The reviewer fell back to
`tsc --listFilesOnly`. Graphos must be usable for the workflow it was built for (PRD §13 query
commands, §3.3 build-stage connectivity) before any further feature work.

## What Changes

- **Identifier-based node identity and labels** (PRD §3.2 extract stage). A tree-sitter node's
  id becomes `<dirHash>_<stem>_<kind>:<identifier>` (a `#L<line>` suffix only when two
  declarations in one file share kind and identifier); its label becomes the identifier (the file
  name for module nodes); the normalized declaration text, bounded by the named truncation budget,
  moves to `nodeSignature`. **BREAKING**: every id in `graph.json` changes; checkpoints and the
  extraction cache are invalidated through the existing config fingerprint; a `--fresh` rebuild is
  required once.
- **Import edges reach the imported file's module node** (PRD §3.3 build stage). Module node ids
  are derived from the path alone, so the target id computed for a resolved relative specifier
  equals the id of the module node emitted for that file. The specifier is taken from the
  structured import node, not recovered by searching the declaration text for `from ` (which today
  fabricates `0_external_## Examples by kind …` nodes out of template literals). The build stage
  logs the number of edges it drops for unknown endpoints or duplicate endpoints.
- **Exact resolution for node-argument commands** (PRD §13). `symbols` matches the identifier
  (exact, then case-insensitive) and the identifier index covers every code node. `explain` and
  `path` resolve their arguments through the same `resolveNodeArg` contract as `neighbors`
  (exact id → exact identifier → case-insensitive identifier) and report *not found* or
  *ambiguous* with candidates; silent fuzzy best-match is removed from them and remains the
  domain of `query`.
- **Cypher filters before it caps** (PRD §13). `WHERE` is evaluated on the lazily enumerated
  bindings and the result budget is applied to the *filtered* rows, so an equality on `id`,
  `label` or `source_file` finds its rows on a 130k-node graph. A named path variable
  (`MATCH p = …`) is rejected with an error naming the construct and pointing to `graphos path`.
- **`--no-observability` really disables everything** (PRD §10.1). The flag turns the debug
  trace environment off; a relative `observability.debugTraceDir` resolves under the output
  directory, and the default location is `graphos-out/debug/`, never the working directory.
- **Bounded, readable bridge table** (PRD §12 export formats). The report lists the top
  articulation points by degree, excludes degree-1 leaves, prints label and source file instead of
  the raw id, and states the total.
- **Global `granularity: file` honoured for code files** (PRD §14 configuration). A project
  `graphos.yaml` setting `granularity: file` yields one node per code file for every extension
  without an explicit per-extension override, which the 2026-10-08 run (logged `Granularity: file`,
  emitted 115 nodes for one file) shows is not the case today.

- **Lean 4 formal model** (`lean/ReviewGraph.lean`, Lean 4 core only, no Mathlib) with
  kernel-checked theorem gates for every decision: node keys never contain declaration text and
  are pairwise distinct within a file (`keyOf_text_free`, `assignKeys_distinct`), a unique name
  has rank 0 wherever it sits and unrelated declarations change no rank (`rank_zero_of_unique`,
  `countBefore_unrelated`); the import target of a resolved specifier *is* the module key of the
  target file and the old placeholder never was (`importTarget_eq_moduleKey`,
  `placeholder_ne_moduleKey`); a resolved import survives the build and any file-level import
  chain lifts to a path in the built graph (`resolved_import_kept`, `chain_lifts`, AVI-518
  INV-9); `resolve` never substitutes a node, `notFound` means no node matches by any criterion,
  an exact id always wins (`resolve_single_sound`, `resolve_notFound_sound`,
  `resolve_exact_id`); filtering before the cap contains the old result and is complete within
  budget, with a decided counter-example for the old order (`evalOld_subset_evalNew`,
  `evalNew_complete`); `--no-observability` disables tracing and a relative trace dir stays under
  the output dir (`noObservability_disables`, `relative_under_output`); the bridge table is
  bounded and degree-filtered (`bridgeRows_length_le`, `bridgeRows_degree`); the wiring threads
  the resolved granularity and the constant wiring is refuted on `file`
  (`wiring_threads_level`, `wiring_bug_refuted`). The methodology follows
  `lean-proof-methodology` (goal-guided invocation, structural list primitives that reduce by
  `rfl`/`decide`, no `by decide` over non-reducible statements); `lean/VERIFICATION.md` records
  the pinned toolchain (`nix run nixpkgs#lean4` → Lean 4.29.1; `nixpkgs#lean` is Lean 3 and must
  not be used) and the exact `lake clean && lake build` command, which exits 0 with zero errors,
  zero warnings and zero `sorry`.

**Formal grounding.** The id scheme instantiates AVI-563 §1.3 (short stable ids as a
well-defined function of `(source_file, kind, line span, symbol name)`; M-6: deterministic
function + injectivity). Cross-file reachability relies on AVI-518 §1.1 and INV-8/INV-9
(reachability is the reflexive-transitive closure of the built edge relation; `shortestPath`
decides it), which `chain_lifts` makes usable by guaranteeing the edges exist. The cypher budget
follows AVI-565 §2.1–2.2 (a size-bounded payload with deterministic truncation of the *result*,
never of the candidates).

Out of scope, recorded for follow-up: a persistent index or daemon so read commands do not reload
the whole `graph.json` (today `graphos serve` already answers `/api/*` from one loaded graph and
the generated skill should say so), and `length(p)` over named paths.

## Capabilities

### New Capabilities
- `identifier-resolution`: One resolution contract for node arguments across `symbols`, `explain`,
  `path` and `neighbors` — identifier index built from node labels, exact-then-case-insensitive
  matching, explicit *not found* / *ambiguous* outcomes, no fuzzy fallback (PRD §13, workflows
  04–06).

### Modified Capabilities
- `extraction-quality`: "Node identity is derived from normalized declaration text" becomes
  identity derived from file path, kind and identifier; labels are identifiers; declaration text
  lives in `nodeSignature` (PRD §3.2).
- `import-resolution`: "Canonical import target identity for path-based module systems" gains the
  guarantee that the module node id is path-derived, that the specifier never comes from re-parsing
  the label, and that dropped edges are counted at build (PRD §3.3).
- `04-query`, `05-path`, `06-explain`: node arguments resolve exactly via the shared contract; no
  silent fuzzy match; *not found* and *ambiguous* outcomes are defined (PRD §13, workflows 04–06).
- `cypher-query`: the budget caps filtered rows, not raw bindings; named path variables produce a
  named error (PRD §13).
- `14-observability`: `--no-observability` disables debug tracing; trace directory resolution
  (PRD §10.1).
- `report-consistency`: bounded bridge-node table and edge-drop accounting (PRD §12).
- `extraction-granularity`: the global level applies to every extension without an explicit
  per-extension override, verified on a real repository (PRD §14).

## Impact

- **Infrastructure**: `Extract/TreeSitter/Convert.hs` (`makeNodeId`, `makeNode`, `tsNodeLabel`,
  `extractSpecifier`, import target node), `Observability/SDK.hs` and `app/Main.hs` (debug trace
  gating and directory), `Config.hs` / `Domain/Config/Core.hs` merge if the granularity bypass is
  there.
- **UseCase**: `Query.hs` (`explainNodeWithIndex`, `pathQueryWithIndex` → `resolveNodeArg`;
  identifier lookups), `Report.hs` (`bridgeNodesSection`), `Extract/Core.hs` (granularity per
  file, edge-drop logging at build).
- **Domain**: `Query/Cypher/Eval.hs` (filter before `takeCap`), `Query/Cypher/Parser.hs` (named
  path error), `Graph/Index.hs` (label index now indexes identifiers; path index reused for
  `source_file` anchoring). No change to `Domain.Types.Node` — `nodeLabel` and `nodeSignature`
  already exist.
- **Outputs and consumers**: `graph.json` ids change (opaque `Text`, schema version unchanged);
  Neo4j/Memgraph pushes re-hash ids on the next push; `cypher --write` ids written against an old
  graph do not carry over; `graph.json` and `GRAPH_REPORT.md` shrink by the duplicated source text.
- **Formal**: `lean/ReviewGraph.lean` mirrors the Infrastructure id/import logic, the UseCase
  resolution contract and the Domain cypher evaluation order; the Haskell implementation must
  match the Lean model's verdicts on the toy fixtures (`example/ts-lsp-test` chain
  `index.ts → db.ts → types.ts`, `resolve "dbGetById"` single / `"dbGetByIdentifier"` not found,
  the `evalOld`/`evalNew` counter-example) — the same checker-parity discipline as
  `deterministic-doc-code-edges`. The change is complete only when `lake clean && lake build` is
  green with zero `sorry`.
- **Tests**: `tests/Graphos/Infrastructure/Extract/*` (convert), `tests/Graphos/UseCase/QuerySpec.hs`,
  `tests/Graphos/UseCase/ReportSpec.hs`, `tests/Graphos/Domain/Query/Cypher/*`,
  `tests/Graphos/Domain/ConfigSpec.hs`, `tests/Graphos/CLI/ParserSpec.hs`, plus an integration
  scenario on the `example/ts-lsp-test` corpus.
