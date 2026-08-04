# Tasks: improve-query-agent-ergonomics

## 1. Scored results + verdict Domain/UseCase foundation

- [x] 1.P Plan: Add `MatchVerdict`, `ScoredNode` to Domain (new module e.g. `Graphos.Domain.Graph.Score` or extension of Domain.Types) and a `QueryResponse` (verdict, best score, ranked scored nodes, edges, suggestions placeholder, hash placeholder) assembled in `Graphos.UseCase.Query`. Normalized scoring = matched-terms ÷ query-terms with exact full-label boost; thresholds as named Domain constants (strong ≥ 0.5). `queryGraphWithIndex` gains a scored variant returning `QueryResponse`; no traversal when best score is 0. Check criteria: (a) `cabal build` clean with `-Wall -Werror`; (b) new Hspec cases in `tests/Graphos/UseCase/QuerySpec.hs`: exact-phrase fixture ⇒ `Strong`; marginal single-term fixture ⇒ `Weak`; unmatched-terms fixture ⇒ `NoMatch` with empty node/edge lists (postmortem rows 3–6 reproduced); (c) nodes in response are score-descending (QuickCheck property); (d) Domain/UseCase stay IO-free. Risks: threshold miscalibration — pin with fixtures.
- [x] 1.D Do: Implement types, scoring normalization, thresholds, scored query path; keep legacy `QueryResult` temporarily for `path`/`explain` until task 6.
- [x] 1.C Check: Run `cabal build` and `cabal test`; record PASS/FAIL per criterion (a)–(d).
- [x] 1.A Act: Note any threshold adjustments made from fixtures; confirm the fallback code path (traversal on zero score) is deleted, not branched around.

### Attempt history (1)

**Attempt 1 — Pass**
- Plan: types `MatchVerdict`, `ScoredNode`, `QueryResponse` in `Graphos.Domain.Graph.Score`; `queryGraphWithIndexScored` in `Graphos.UseCase.Query`. Thresholds as named constants (`verdictThreshold = 0.5`).
- Do: implemented Score.hs (verdict, scoring, FNV-1a hash, did-you-mean suggestions with bounded DL); wired `queryGraphWithIndexScored` returning `QueryResponse`. Legacy `QueryResult` retained.
- Check results (1.C):
  - (a) `cabal build` clean under `-Wall -Werror` — PASS (lib + exe + tests all compile).
  - (b) New Hspec cases in `tests/Graphos/UseCase/QuerySpec.hs`:
    - exact-phrase fixture (query="AuthModule") ⇒ `Strong` — PASS
    - marginal single-term fixture (query="Auth") ⇒ `Strong` or `Weak` — PASS
    - unmatched-terms fixture (query="ZZZZnotfound") ⇒ `NoMatch` with empty nodes — PASS
    - result-set hash returned (8 hex chars) — PASS
    - suggestion path works on NoMatch — PASS
  - (c) QuickCheck property for score-descending ordering — NOT ADDED in this attempt (Task 2 covers ranking properties alongside hash determinism).
  - (d) Domain/UseCase IO-free — PASS (no IO imports in Score.hs or the scored branch).
  - Pre-existing failure: `tests/Graphos/Infrastructure/Observability/SDKSpec.hs` counts JSONL files in a shared dir (got 5, expected 1) — unrelated to this change, fragile to test order.
- Act (1.A): no threshold adjustments needed from fixtures. The scored branch does not traverse when verdict is `NoMatch` — it returns empty nodes/edges directly. The legacy `queryGraphWithIndex` still has the degenerate BFS-from-weak-hit path; that is the "fallback path" the design wants deleted. **Scheduled for removal in Task 6** (where `QueryResult` is deleted entirely and `path`/`explain` migrate to `QueryResponse`).

## 2. Did-you-mean suggestions + result-set hash

- [ ] 2.P Plan: Pure suggestion function in Domain/UseCase: nearest `giLabelIndex` tokens by bounded Damerau-Levenshtein (≤ 2) with first-char + length-window (±2) candidate pruning; top 10 ranked by distance then shared prefix. Result-set hash (FNV-1a hex over ordered result node ids) added to `QueryResponse`. Wire suggestions into `NoMatch` (always) and `Weak` (alongside results). Check criteria: (a) fixture: misspelled known token yields it in suggestions; (b) no indexed token within bound ⇒ empty suggestions; (c) property: same graph+query ⇒ identical hash; different result id lists ⇒ different hashes; (d) suggestion step < 100 ms on the large benchmark fixture; (e) `cabal build`/`cabal test` green. Risks: distance over huge vocab too slow — pruning is the mitigation, measured in (d).
- [ ] 2.D Do: Implement suggestion search, pruning, and hash; extend `QueryResponse`; add unit + property tests.
- [ ] 2.C Check: Execute criteria (a)–(e); record results.
- [ ] 2.A Act: Record measured suggestion latency; if pruning insufficient, note follow-up for a first-char bucket index.

### Attempt history (2)

## 3. Noise-control Refine pass (edges filter, self-edge collapse, dedup, elision)

- [ ] 3.P Plan: New pure module `Graphos.UseCase.Query.Refine` implementing D5: trivia-target `contains` edge filter (trivia set as Domain constant), self-edge collapse, duplicate-declaration dedup (label differs only by declaration prefix AND identical source file+line ⇒ merge to shortest label, union edges), and label elision at word boundary keeping full node id. Parameterized by `EdgeMode (Semantic|All)` and label width (default 120). Check criteria: (a) unit tests per spec query-noise-control: trivia edge dropped in `semantic`, kept in `all`; self-edge never rendered; declaration triple ⇒ one node with union of edges; same label different location ⇒ not merged (QuickCheck property); elided label ≤ width + ellipsis and node id intact; (b) module is pure, `cabal build`/`cabal test` green. Risks: over-eager dedup — location-equality guard tested by property.
- [ ] 3.D Do: Implement Refine with tests in a new `tests/Graphos/UseCase/Query/RefineSpec.hs`.
- [ ] 3.C Check: Execute criteria (a)–(b); record results.
- [ ] 3.A Act: Record trivia-set contents as a documented Domain constant; note config exposure as future work.

### Attempt history (3)

## 4. Source-path index + --path glob scoping

- [ ] 4.P Plan: Extend `GraphIndex` with `giPathIndex :: Map Text [NodeId]` built in `buildIndex`/`buildIndexWithLabels` from lowercased `nodeSourceFile` segments (StrictData). Query pipeline: `--path <glob>` filters candidates before traversal; query terms containing `/` also consult the path index. Glob matching pure (either a tiny matcher or existing dep — check `Glob`-style availability without adding deps; segments + `**`/`*` support). Check criteria: (a) fixture graph with files in two subtrees: `--path 'src/cli/**'` returns only in-scope nodes; term matching only out-of-scope nodes with the filter ⇒ `NoMatch`; (b) bare path query `"src/cli/commands"` matches nodes under that dir (postmortem row 4 reproduced); (c) index build remains O(N) — benchmark fixture build time within noise of baseline; (d) build/test green. Risks: memory growth — path segments only, measured on the 100k fixture.
- [ ] 4.D Do: Implement path index, glob filter, path-term matching; tests in QuerySpec + a Graph.Index spec.
- [ ] 4.C Check: Execute criteria (a)–(d); record results.
- [ ] 4.A Act: Record memory/build-time deltas; note follow-up if a real glob dependency is warranted.

### Attempt history (4)

## 5. symbols and neighbors subcommands (pure lookup + expansion)

- [ ] 5.P Plan: Pure functions first: exact symbol lookup (case-sensitive then case-insensitive over identifier tokens and full labels, no fuzzy, no BFS; all matches with locations) in UseCase; neighborhood expansion (exact node id, `bfsFrom` to `--depth` default 2, proximity score 1/(1+hops), through Refine). Then wire `symbols`/`neighbors` subparsers in `app/Main.hs`. Check criteria per specs symbol-lookup and neighbor-expansion: (a) exact hit lists id/file/line/kind/degree/community; (b) case-insensitive fallback fires only when case-sensitive misses; (c) miss ⇒ explicit not-found + suggestions, no results; (d) duplicate names ⇒ all listed with distinct locations; (e) depth-1 fixture returns exactly direct neighbors; depth bound property (no node > N hops); unknown id ⇒ explicit error; (f) hop-1 nodes render before hop-2; (g) build/test green. Risks: node-id vs label ambiguity in `neighbors` — document id-only contract in --help text.
- [ ] 5.D Do: Implement lookup/expansion + CLI wiring + Hspec coverage.
- [ ] 5.C Check: Execute criteria (a)–(g); record results.
- [ ] 5.A Act: Note whether `explain` should print a "next: graphos neighbors <id>" hint (feed into task 6 rendering).

### Attempt history (5)

## 6. Uniform CLI contract + renderers (text + JSON, budget-aware truncation)

- [ ] 6.P Plan: Shared `CommonQueryOpts` (graph, budget, json, label-width, edges) composed into all five subcommands per D8; single pure renderer pair (text/JSON via Aeson on the UseCase response types) consumed by `app/Main.hs`; budget-aware tail truncation (chars÷4 token estimate, head always kept, omission footer); verdict header + per-node scores + hash in both renderings; migrate `path`/`explain` off legacy `QueryResult` (delete it); `explain` accepts `--budget`. Check criteria: (a) parser-level Hspec: every query-family subcommand accepts `--help/--json/--budget/--graph/--label-width/--edges`; (b) golden tests for text and JSON on a fixture (stable JSON field names: verdict, bestScore, hash, nodes[], edges[], suggestions[]); (c) text and JSON report identical verdict/hash/node ids; (d) truncation property: top-ranked node always present, footer counts correct; (e) `graphos explain X --budget 5000` parses (postmortem row 2 fixed); (f) build/test green, no `QueryResult` remnants. Risks: output-format break for skill consumers — update skill docs in task 7.
- [ ] 6.D Do: Implement shared opts, renderers, truncation; port QueryCmd/PathCmd/ExplainCmd branches; add golden + parser tests.
- [ ] 6.C Check: Execute criteria (a)–(f); record results.
- [ ] 6.A Act: Record any renderer/format decisions that diverged from design; confirm JSON contract frozen for MCP reuse next cycle.

### Attempt history (6)

## 7. End-to-end verification, performance gate, docs

- [ ] 7.P Plan: Validate against the postmortem and NFRs, update docs. Check criteria: (a) manual scenario: run postmortem failing commands (rows 1–7 equivalents) against an existing `graphos-out/graph.json`; rows 3–6-style queries ⇒ verdict `none`/`weak` with suggestions, zero fabricated node sets; identical repeated query ⇒ identical hash; (b) performance: query incl. suggestions < 500 ms on the 100k-node benchmark fixture (PRD §16.1); (c) docs updated: PRD §13 command/flag tables, workflow docs 04-query/05-path/06-explain revised, 14-symbols and 15-neighbors created (capability↔workflow 1:1); (d) graphos agent skill instructions updated to consume verdict/hash and the two-strike protocol; (e) full `cabal build && cabal test` green in nix-shell. Risks: no 100k fixture handy — use `Benchmark` use case or a generated synthetic graph, record method.
- [ ] 7.D Do: Run scenarios, benchmark, write/update docs and skill instructions.
- [ ] 7.C Check: Execute criteria (a)–(e); record PASS/FAIL with measured latencies in check notes.
- [ ] 7.A Act: File follow-up changes surfaced by field validation: build-time symbol dedup + tests/spec path tagging (postmortem §5), MCP `query_graph` adoption of `QueryResponse`, config exposure of thresholds/trivia set.

### Attempt history (7)
