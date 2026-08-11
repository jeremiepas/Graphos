## Why

Two query-family CLI ergonomics gaps were found while testing `graphos query` against a built
graph (PRD §13.1 commands, §13.2 flags):

1. **`graphos neighbors` rejects display names.** The spec
   (`neighbor-expansion`) and the parser metavar say `NODE_ID`, and the
   `graphos-query` skill tells agents to "expand around a node". In practice, agents call
   `neighbors` with the human-readable label they just saw in `explain`/`symbols` output
   (e.g. `Graphos.UseCase.QuerySpec`) and get `Node not found`. Only the internal
   `mod_*`/numeric id works. This forces a round-trip through `explain` just to discover the
   internal id, wasting a query per expansion.
2. **`graphos query` rejects `--json`.** The `query-cli-contract` spec already requires
   every query-family subcommand — including `query` — to accept `--json` and emit a single
   JSON document. The `symbols` and `neighbors` parsers already wire `--json` via
   `CommonQueryOpts`; `query` (and `path`/`explain`) do not. The `graphos-query` skill
   documents `--json` as available, so agents hit an `Invalid option` error.

Both are conformance gaps against already-shipped specs; this change closes them so the CLI
matches its own contract.

## What Changes

- **`graphos neighbors <id-or-name>`**: resolve the argument as a node id first; if no exact
  id matches, fall back to `symbolLookup`-style exact/case-insensitive label match. When the
  fallback resolves to a single node, expand from it; when it resolves to multiple or zero,
  report the candidates explicitly (no silent fuzzy traversal).
- **`graphos query <q> --json`**: accept `--json` (and the rest of the `CommonQueryOpts`
  family: `--label-width`, `--edges`) and emit a single JSON document with the existing
  `QueryResponse` fields (verdict, best score, hash, nodes, edges, suggestions). No
  interleaved log lines on stdout.
- **`graphos path <from> <to> --json`** and **`graphos explain <node> --json`**: gain the
  same `--json` flag for uniformity, rendering their existing results as JSON.
- Update `renderCommandReference` so the embedded command reference agents read matches the
  new flag surface.
- **BREAKING**: none. Existing invocations (passing real `NODE_ID` to `neighbors`, plain text
  to `query`) keep working.

## Capabilities

### New Capabilities
<!-- None — this change only closes conformance gaps against existing specs. -->

### Modified Capabilities
- `neighbor-expansion`: the argument is no longer strictly a node id; a display-name
  fallback is added, with explicit multi/zero-match reporting and no fuzzy traversal.
- `query-cli-contract`: `query`, `path`, and `explain` now honor `--json` (and the uniform
  `--label-width`/`--edges` flags) as already required by the spec's "Uniform flag
  acceptance" and "JSON output mode" requirements.

## Impact

- **Code**:
  - `src/Graphos/CLI/Parser.hs` — add `CommonQueryOpts` to `queryOpts`, `pathOpts`,
    `explainCmd`; widen `neighborsOpts` metavar and resolution.
  - `src/Graphos/UseCase/Query.hs` — add a `resolveNodeArg` helper (id-first, then label
    exact, then label case-insensitive); expose JSON renderers for `path`/`explain`
    alongside the existing `QueryResponse`/`SymbolResult`/`NeighborsResult` `ToJSON`
    instances.
  - `app/Main.hs` — dispatch the new flags to the UseCase layer and route stdout to JSON or
    text rendering.
- **APIs**: CLI surface only; no library/MCP protocol change.
- **Dependencies**: none new.
- **Specs/Skills**: `query-cli-contract`, `neighbor-expansion` delta specs; the
  `graphos-query`/`graphos` skill command reference continues to describe `--json` and is
  now accurate.

## PDCA Cycle

- **Plan**: Close the two conformance gaps discovered by direct CLI testing so the
  query-family commands accept what their own specs and skills advertise. Success is
  measured by (a) `graphos neighbors Graphos.UseCase.QuerySpec --depth 1` returning the same
  neighborhood as `mod_Graphos.UseCase.QuerySpec` does today, (b) `graphos query "Graph"
  --json` emitting valid JSON with `verdict`/`hash` fields, and (c) no regression in existing
  text output (PRD §16.1 query latency budget < 500ms).
- **Do**: Implement the parser/UseCase changes in `tasks.md`, keeping all IO in
  Infrastructure and all resolution logic pure in UseCase (architecture-purity).
- **Check**: Hspec + QuickCheck tests in `tests/Graphos/UseCase/QuerySpec.hs` and
  `tests/Graphos/CLI/ParserSpec.hs` cover id-first vs label-fallback resolution, multi-match
  reporting, and JSON/text field agreement; `cabal test` is green; manual smoke test
  against `graphos-out/graph.json` confirms both fixed invocations.
- **Act**: If JSON/text agreement holds and the fallback preserves the < 500ms latency
  budget, standardize the `resolveNodeArg` helper as the single node-argument resolver for
  future query-family commands; if the label fallback adds latency on large graphs, feed
  that into the next query-performance iteration.