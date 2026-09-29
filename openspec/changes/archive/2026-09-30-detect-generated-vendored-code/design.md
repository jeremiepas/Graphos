## Context

The Detect stage currently enumerates candidate files and hands them to Extract.
There is no notion of "this file is generated/vendored and adds noise." A single
generated bindings file (~350k lines) produced ~61k nodes that Leiden collapsed
into one 32k–43k-node community, skewing centrality and causing MCP timeouts.
The fix belongs at the earliest pure stage so all downstream stages benefit.

## Goals / Non-Goals

**Goals:**
- Classify files as Source / Generated / Vendored / Minified during Detect.
- Default to excluding non-Source files; offer a collapse alternative.
- Keep classification pure and testable in Domain/UseCase; keep only IO
  (reading leading bytes, path checks) in Infrastructure.

**Non-Goals:**
- Language-aware semantic analysis of generated code.
- Changing Leiden parameters (covered by a separate change).
- Replacing `.graphosignore` (complementary; user-driven ignores are separate).

## Decisions

- **Classification lives in Domain as a pure function** `classifyFile :: DetectionConfig -> FileMeta -> FileClass`.
  - *Alternative considered:* classify inside Infrastructure/Extract — rejected,
    violates the "Domain has zero IO / logic stays pure" rule and is harder to test.
- **Leading-content sniff limited to first 40 lines / 8 KB**, read once in
  Infrastructure/FileSystem and passed as `FileMeta`.
  - *Alternative considered:* full-file regex scan — rejected on performance for
    350k-line files.
- **Exclude is the default; collapse is opt-in** via `DetectionMode`.
  - *Alternative considered:* collapse by default — rejected because excluded
    generated code is rarely audited and collapse still adds a node/edges.
- **Vendored detection is path-segment based**, not content based, matching a
  configurable set defaulting to `node_modules`, `vendor`, `third_party`.
  - *Alternative considered:* rely on `.gitignore` — rejected, not always present
    and not semantically "vendored".

## Risks / Trade-offs

- [False positive excludes hand-written file resembling generated] → limit
  signatures to high-precision phrases; log every exclusion so it is auditable.
- [Collapse node loses internal structure] → acceptable; `childCount` preserves
  the signal, and full detail remains on disk.
- [Leading-sniff misses generator marker below line 40] → threshold configurable.

## Migration Plan

- Additive and behind defaults; existing graphs regenerate with fewer nodes.
- Rollback: set detection mode to `off` to restore prior behavior.
- Verify with `cabal test` (classifier unit tests) and a `cabal run graphos`
  smoke run confirming the mega-community disappears.
