## Context

Two query-family CLI conformance gaps were found by direct testing against
`graphos-out/graph.json` (PRD §13.1):

1. `graphos neighbors <id-or-name>` only accepts the internal node id (`mod_*`/numeric).
   Passing the display name an agent just saw in `explain`/`symbols` output returns
   `Node not found`.
2. `graphos query <q> --json` (and `path`/`explain` `--json`) are rejected with
   `Invalid option '--json'`, even though `query-cli-contract` already requires every
   query-family subcommand to accept `--json`, and the `graphos-query` skill documents
   `--json` as available.

Current state (read before designing):

| Layer | Module | Current state |
|-------|--------|---------------|
| Infrastructure | `Graphos.CLI.Parser` | `queryOpts` parses `QUESTION + --dfs + --budget + --graph` only; `pathOpts` parses `FROM TO + --graph`; `explainCmd` parses `NODE + --graph`; `symbolsOpts`/`neighborsOpts` already use `CommonQueryOpts` (`--graph --budget --json --label-width --edges`). |
| UseCase | `Graphos.UseCase.Query` | `queryGraphWithIndexScored` returns `QueryResponse`; `symbolLookup` already does exact-then-case-insensitive label matching; `neighborhoodExpansion` takes a `NodeId` and returns `NeighborsResult`. No node-argument resolver. |
| UseCase | `Graphos.UseCase.Query.Render` | `renderQueryResponseJSON`, `renderPathResultJSON`, `renderExplainResultJSON`, `renderNeighborsResultJSON`, `renderSymbolResultJSON` all already exist. |
| UseCase | `Graphos.UseCase.Query.Refine` | `refineResponse` applies edge filtering, self-edge collapse, declaration dedup, label elision — all pure. |
| Infrastructure | `app/Main.hs` | `QueryCmd`/`PathCmd`/`ExplainCmd` only call the text renderers; `SymbolsCmd`/`NeighborsCmd` already branch on `cqoJson`. |

Constraints:

- Clean architecture: Domain has zero IO; UseCase has zero IO implementation; all side
  effects in Infrastructure (`architecture-purity`).
- Haskell conventions: explicit exports, type signatures on all top-level definitions,
  `StrictData`, pure functions preferred.
- Build: `cabal build` with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror --flag dev`;
  tests via `cabal test` (Hspec + QuickCheck).
- Performance: PRD §16.1 query latency budget < 500ms (MCP path; CLI must not regress).
- No new external dependencies; no orphan instances; no partial functions.

## Goals / Non-Goals

**Goals:**
- Make `graphos neighbors <display-name>` expand the same neighborhood as the internal id.
- Make `graphos query|path|explain --json` emit a single JSON document with no log noise.
- Keep every existing text-mode invocation byte-identical (no breaking changes).
- Keep all resolution logic pure in UseCase; CLI parser/dispatcher stays in Infrastructure.
- Ship tests proving id-first resolution, multi-match reporting, and JSON/text agreement.

**Non-Goals:**
- Fuzzy matching for `neighbors` (explicitly rejected by the spec; resolution is
  exact-id → exact-label → case-insensitive label only).
- Changing the `QueryResponse`/`NeighborsResult`/`SymbolResult` JSON schema (already
  shipped and consumed by the MCP server).
- Adding `--json` to non-query-family commands (`push`, `merge`, `ingest`, `serve`).
- Rewriting the dispatcher to a registry pattern (out of scope; small targeted edit only).

## Decisions

### Decision 1: Add a `resolveNodeArg` helper in `Graphos.UseCase.Query`

A new pure function resolves a user-supplied argument to a node id using the
existing `GraphIndex` and `symbolLookup` machinery:

```
resolveNodeArg :: Text -> Graph -> GraphIndex -> NodeResolution
data NodeResolution
  = ResolvedSingle NodeId
  | Ambiguous [ScoredNode]
  | NotFound
```

Resolution order: (1) exact id (`Map.lookup arg (gNodes g)`), (2) exact label via
`giLabelIndex`, (3) case-insensitive label fallback. This mirrors `symbolLookup` so the
two commands agree on what counts as a "name hit".

Layer: UseCase (pure). The CLI dispatcher in `app/Main.hs` calls `resolveNodeArg` and
branches on the result before invoking `neighborhoodExpansion`.

| Alternatives | Why rejected |
|--------------|-------------|
| Resolve inside `neighborhoodExpansion` | Couples expansion to resolution and changes the existing pure function's signature; harder to reuse for future query-family commands. |
| Resolve in the CLI parser (`Parser.hs`) | Parser is pure option-parsing only; it has no `Graph`/`GraphIndex`. Pushing resolution there violates layering. |
| Fuzzy resolution | Spec explicitly forbids fuzzy traversal; agents that want fuzzy should use `query`. |

### Decision 2: Thread `CommonQueryOpts` through `queryOpts`, `pathOpts`, `explainCmd`

`queryOpts` gains `--json`, `--label-width`, `--edges` (plus the existing `--dfs`,
`--budget`, `--graph`). `pathOpts` and `explainCmd` gain `--json` (and the rest of
`CommonQueryOpts` for uniformity, even if `--budget` is a no-op for `path`).

`Command` constructors change from positional tuples to carrying `CommonQueryOpts`:

- `QueryCmd :: Text -> Text -> CommonQueryOpts` (question, mode, opts)
- `PathCmd :: Text -> Text -> CommonQueryOpts` (from, to, opts)
- `ExplainCmd :: Text -> CommonQueryOpts` (node, opts)

This is a pure data-shape change inside one module; no public library API depends on these
constructors.

| Alternatives | Why rejected |
|--------------|-------------|
| Add only `--json` to each command | Violates `query-cli-contract` "Uniform flag acceptance" which requires `--label-width` and `--edges` on all query-family commands too. |
| Keep tuple shape and add a `Bool` json flag | Diverges from `symbolsOpts`/`neighborsOpts` which already use `CommonQueryOpts`; creates two patterns. |

### Decision 3: Dispatch JSON vs text in `app/Main.hs` using existing renderers

`QueryCmd`/`PathCmd`/`ExplainCmd` branches gain the same `if cqoJson opts then ... else ...`
shape already used by `SymbolsCmd`/`NeighborsCmd`. The JSON branch calls the
already-existing `renderQueryResponseJSON`/`renderPathResultJSON`/`renderExplainResultJSON`.

To honor the "no interleaved log lines" requirement, the `logInfo`/`logDebug` calls in the
`QueryCmd`/`PathCmd` branches SHALL be routed to **stderr** (or skipped) when `cqoJson` is
true. The default `LogEnv` writes to stderr already, so this is mostly a check; any
`putStrLn`-based log that currently goes to stdout must be moved to the log env.

Layer: Infrastructure (IO dispatch only). No new IO in UseCase.

| Alternatives | Why rejected |
|--------------|-------------|
| Add a `--format text|json` flag instead of `--json` | `CommonQueryOpts` already uses `cqoJson :: Bool` and `symbols`/`neighbors` already accept `--json`; a new `--format` would diverge. |
| Write a new JSON renderer in Infrastructure | Renderers already exist in `Render.hs`; reuse keeps a single source of truth. |

### Decision 4: `resolveNodeArg` is the single node-argument resolver

`neighborhoodExpansion` keeps its `NodeId -> ...` signature. The dispatcher resolves
first, then calls `neighborhoodExpansion` with the resolved id. On `Ambiguous`/`NotFound`,
the dispatcher renders a candidate list / not-found message (text or JSON) without
calling expansion. This keeps the existing pure function unchanged and reusable.

| Alternatives | Why rejected |
|--------------|-------------|
| Change `neighborhoodExpansion` to take `Text` | Breaks existing callers (MCP server) and couples expansion to resolution. |

### Decision 5: Update `renderCommandReference` in the same change

`renderCommandReference` is the source for generated skill command tables. Update it in
the same commit so the `graphos-query`/`graphos` skill reference is accurate the next time
skills are regenerated.

## Risks / Trade-offs

- [Multi-match noise on short common names like `parse`, `main`, `spec`] → Mitigation:
  `Ambiguous` lists candidates with source file + line so the caller can disambiguate and
  re-run with the id; no traversal is performed so the cost is one index lookup.
- [Latency: case-insensitive label scan on 100k+ node graphs] → Mitigation: resolution
  uses the already-built `giLabelIndex` (lowercased keys), so it is O(log N) per lookup,
  not O(N). Verify in tests against the 100k fixture.
- [Log lines leaking into JSON stdout] → Mitigation: explicit stderr-routing in JSON mode
  + a test that asserts stdout parses as a single JSON document.
- [Breaking changes to `Command` constructor shape] → Mitigation: constructors are
  internal to `Graphos.CLI.Parser` + `app/Main.hs`; grep confirms no other module
  pattern-matches on them (to be verified in tasks).
- [`--budget` semantics for `path`/`explain`] → Mitigation: `path` and `explain` outputs
  are small; `--budget` is accepted for uniformity and applied via `truncateOutput` but is
  effectively a no-op in normal cases. Document this in the command reference.

## Verification Strategy (Check)

1. **Unit (UseCase, pure)**:
   - `tests/Graphos/UseCase/QuerySpec.hs` — add cases for `resolveNodeArg`: exact id,
     exact label, case-insensitive label, ambiguous, not-found.
   - `tests/Graphos/UseCase/QuerySpec.hs` — assert `neighborhoodExpansion` output is
     unchanged when given a resolved id (regression guard).
2. **Parser (Infrastructure)**:
   - `tests/Graphos/CLI/ParserSpec.hs` — assert `query`/`path`/`explain` accept `--json`,
     `--label-width`, `--edges`; assert `neighbors` metavar/help text reflects
     `<id-or-name>`.
3. **Renderer agreement (UseCase, pure)**:
   - `tests/Graphos/UseCase/QuerySpec.hs` — property: for any graph, `renderQueryResponseJSON`
     parses as JSON and its `verdict`/`hash`/node-id-set equal the text rendering's
     header/hash/set.
4. **Build gate**: `cabal build --flag dev` is green with `-Werror`.
5. **Test gate**: `cabal test` is green (Hspec + QuickCheck).
6. **Manual smoke (acceptance)** against `graphos-out/graph.json`:
   - `graphos neighbors Graphos.UseCase.QuerySpec --depth 1` returns the same neighborhood
     as `mod_Graphos.UseCase.QuerySpec --depth 1` (Plan success criterion).
   - `graphos query "Graph" --json | jq .verdict` returns `"strong"`.
   - `graphos path "Graphos.UseCase.QuerySpec" "Graphos.UseCase.SelectContextSpec" --json`
     parses as JSON.
   - Text-mode invocations unchanged (diff against pre-change output).

## Iteration & Rollback (Act)

- If `cabal test` fails: stop (stop-on-failure rule), report, propose fix, request
  approval — do not auto-fix.
- If JSON/text agreement test fails: the renderer dispatch in `Main.hs` is the likely
  culprit; revert the dispatch edit and re-run.
- Rollback: all changes are in `Parser.hs`, `Query.hs`, `Render.hs` (no), `Main.hs`,
  `renderCommandReference`, and tests. A single `git revert` of the change commit restores
  the previous behavior; no data migration is involved.
- Next PDCA cycle: if the `resolveNodeArg` pattern holds, extract it as the canonical
  node-argument resolver for any future query-family command that takes a node argument
  (`explain` could be migrated to it as a follow-up, but is out of scope here).