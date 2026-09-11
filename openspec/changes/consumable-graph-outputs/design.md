# Design — Consumable Graph Outputs

## Context

Feedback-driven change (`note/feedback/graphos-feedback.md`): the pipeline and in-process
queries work at 87K-node scale, but every path that leaves graphos — sub-graph for a task,
branch diff, canvas, scripted cypher — required hand-written Python. Everything here is
CLI + UseCase + one pure Domain extension; the acceptance bar is that the user's three
workaround scripts are deletable. Layer placement: seed/scope/rollup logic in UseCase,
changed-node detection in Domain (pure map algebra), writers in Infrastructure, flags in
CLI.

## Decision 1 — Fix `subgraph` by adding seed modes, not replacing config mode

`--config` (named subsystems) stays; `--path`/`--seeds`/stdin become the ergonomic front
door with `--hops` expansion. The feedback's failed invocations are the design spec for
the new surface — what a user guesses first should work.

Alternatives considered: replacing config mode (breaks the taxonomy workflow it was built
for); a new separate command (two extraction commands with one meaning apart is exactly
how `subgraph` got lost in the first place).

## Decision 2 — Diff in-process on the existing Domain diff, extended for changed nodes

`graphDiff` already computes added/removed via `Map.difference`; changed symbols are the
id-intersection with unequal content (`Map.intersectionWith`), a pure extension. The CLI
loads both graphs in one process — the same memory model as the existing `merge` command,
which already loads two graphs.

Alternatives considered: streaming file-level comparison (what the Python workaround did)
— only needed if two-graph residency measurably fails, and the report format is designed
so the loading strategy can change underneath (the Act note in the spec); external diff
via Neo4j (heavyweight dependency for a local question).

## Decision 3 — One canvas writer

The vault exporter already writes `graph.canvas`; it gets extracted to
`Infrastructure/Export/Canvas.hs` and gains file-aggregation, category/status coloring
and a deterministic layout. `subgraph --canvas` and `diff --canvas` call the same writer.
Determinism (same input → byte-identical output) is a requirement because canvases live
in vaults under git.

Alternative considered: a second, focused canvas implementation next to the vault one —
rejected, that is the viewer-duplication mistake this repo already paid for once.

## Decision 4 — Wire the existing cypher JSON renderer; add `--format`

`renderCypherResultJSON` is implemented and unit-tested; the bug is a missing dispatch in
the success path. `--format json|csv|tsv` becomes the general surface with `--json` kept
as an alias, TSV stays the default. Output discipline follows `query-cli-contract`: one
document on stdout, errors on stderr.

Alternative considered: making JSON the default — rejected for backward compatibility
with existing scripts parsing TSV.

## Decision 5 — Helper regression test, not just the one-line fixes

Adding `<**> helper` to nine registrations fixes today; a parser-structure test that
fails when any registered subcommand lacks subcommand help prevents the same silent
regression on the next command added. Same philosophy as the studio's token lint:
make the drift structural, not reviewed.

## Deferred — binary/indexed graph format (feedback priority 3)

Not in this change. Rationale: (a) seed-driven `subgraph` turns the 4.3 GB problem into a
one-command small-JSON problem for the offline case; (b) the serving case is
`progressive-graph-interface`'s slice API; (c) an on-disk format is a contract-level
decision (`graph-json-contract`) deserving its own change with measurements, not a rider.
Revisit if multi-GB pain persists after (a) and (b) land.

## Verification strategy

- `cabal build` with the dev flag (warnings as errors).
- `cabal test` — Hspec: seed resolution (path/file/stdin parity, comment handling), hop
  expansion on a fixture graph, changed-node detection (label/kind/signature/span cases),
  scope filtering, per-file rollups, csv quoting (RFC 4180 cases), canvas golden files
  (determinism = byte equality on repeated runs), parser helper-coverage test.
- Scenario pass: every WHEN/THEN in the four delta specs plus the modified `cypher-query`
  requirement, run against the repo's own extraction (`graphos . --no-viz`) — including
  the feedback's original failing invocations, which must now either work or fail with
  the guidance error.
