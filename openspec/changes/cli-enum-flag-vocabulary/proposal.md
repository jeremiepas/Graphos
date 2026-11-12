## Why

`graphos --edge-density sparse` is rejected:

```
option --edge-density: cannot parse value `sparse'
```

while the same binary's `--help` documents `Edge density: sparse|normal|dense|maximum`.
The option is built with `option auto`, so the accepted vocabulary is the derived
`Read` instance of `EdgeDensity` — the capitalised constructors `Sparse|Normal|Dense|Maximum`.
Help text, documentation and accepted tokens are three unrelated strings in the
source, and they have drifted. The run that surfaced this had to be re-issued
with `--edge-density Sparse` after the documented spelling failed.

This is not only a usability defect. `--neo4j-push-mode full` fails the same way,
and accepted spec `12-neo4j-push` requires `Override: --neo4j-push-mode full|subgraph|community`
— so the implementation is non-conforming against an already-approved requirement.
Six enum options are affected (PRD §13.1 commands, §13.2 flags):

| Option | Commands | `--help` says | `Read` actually accepts |
| --- | --- | --- | --- |
| `--edge-density` | run, merge | `sparse\|normal\|dense\|maximum` | `Sparse\|Normal\|Dense\|Maximum` |
| `--neo4j-push-mode` | run | `full\|subgraph\|community` | `FullPush\|SubgraphPush\|CommunityPush` |
| `--mode` | push | `FullPush\|SubgraphPush\|CommunityPush` | same (constructor-shaped help) |
| `--memgraph-push-mode` | run | `MemgraphFull\|MemgraphSubgraph\|MemgraphCommunity` | same (constructor-shaped help) |
| `--mode` | push-memgraph | `MemgraphFull\|…` | same (constructor-shaped help) |

Two further symptoms of the same root cause: no enum option declares a `metavar`,
so usage reads `--edge-density ARG`; and `auto` failures never name the accepted
values, so the error teaches nothing. Separately, ~30 numeric options also use
`option auto`, which accepts nonsense that only fails much later —
`--threads -1`, `--port 99999`, `--resolution 0` all parse today.

## What Changes

- Add a token-table reader: one `[(token, value)]` table per enum, from which the
  `ReadM`, the `metavar` and the help fragment are all derived. Help can no longer
  document a token the parser rejects, because both come from the same value.
- Switch the five enum options above from `option auto` to that reader, with the
  lowercase vocabulary (`full|subgraph|community`, `sparse|normal|dense|maximum`)
  as the documented primary tokens — the vocabulary already used by `PushMode`'s
  JSON instances, `graphos.yaml`, the workflow docs and spec `12-neo4j-push`.
- Keep constructor spellings (`Sparse`, `MemgraphSubgraph`, …) as silent aliases and
  match case-insensitively, so no invocation that works today stops working.
- Route the already-correct hand-written readers (`--granularity`, `--detect-mode`,
  `--edges`, `--cluster-algorithm`) through the same helper so one mechanism covers
  every enum option.
- Give every enum and numeric option a metavar, and replace numeric `option auto`
  with the existing `boundedIntReader` (plus a new `boundedDoubleReader`), so
  out-of-range values are rejected at parse time with the range named.
- Make unknown values report the accepted vocabulary instead of `cannot parse value`.
- Correct the docs that are wrong in the other direction: `docs/workflows/01-full-pipeline.md`
  and `docs/workflows/09-merge.md` document `--edge-density N | 0.0` as a float.
- Add table-driven `ParserSpec` coverage that walks the registry and asserts each
  option's help body and accepted tokens agree — the drift becomes a test failure.

## Capabilities

### New Capabilities
- `cli-enum-vocabulary`: the token vocabulary, metavar, help derivation, bounds and
  error shape of every enum-valued and numeric CLI option.

### Modified Capabilities
None. `12-neo4j-push` already requires `--neo4j-push-mode full|subgraph|community`
and `13-memgraph-push` already requires `--memgraph-push-mode`; this change makes
the implementation conform rather than changing those requirements.

## Impact

- `src/Graphos/CLI/Parser.hs` — all `option auto` sites; `granularityReader`,
  `detectModeReader`, `edgeModeReader` folded into the shared helper;
  `renderCommandReference` token lists.
- New `src/Graphos/CLI/Readers.hs` (reader combinators) and token tables beside the
  types in `src/Graphos/Domain/Types/Pipeline.hs` (pure, no IO — Domain rule holds).
- `graphos.cabal` — new exposed module.
- `tests/Graphos/CLI/ParserSpec.hs` — registry-driven help/parse agreement tests.
- `README.md`, `docs/workflows/01-full-pipeline.md`, `09-merge.md`,
  `12-neo4j-push.md`, `13-memgraph-push.md`.
- No change to pipeline behaviour, `graph.json`, or any default value.

### Adjacent finding, deliberately out of scope
`neo4jPushMode` / `mgPushMode` in `Domain/Config/Export.hs` are parsed from
`graphos.yaml` but never read by any consumer — `UseCase/Export.hs` uses the CLI
value only. The shipped `graphos.yaml` documents the key as `push_mode` while the
`FromJSON` instance reads `pushMode`. Wiring config→push mode is a behaviour change
and belongs in its own proposal; recorded here so it is not lost.
