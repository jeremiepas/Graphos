## Context

`Graphos.Infrastructure.FileSystem.Ignore` already defines pattern types
(`GlobPattern`, `PrefixPattern`, `SuffixPattern`, `WcDoubleStar`, etc.) and a
`loadGitignore` path, but user-supplied `.graphosignore` patterns did not exclude
files in practice. The likely causes are ambiguous base-directory resolution
(CWD vs scan root) and path normalization mismatches (`./source/...` prefix vs
scan-root-relative). There is also no logging to diagnose load/match behavior.

## Goals / Non-Goals

**Goals:**
- Deterministic, documented resolution of the ignore file location and match base.
- Correct gitignore-style semantics.
- Observability: log patterns loaded and files ignored.
- A CLI escape hatch (`--ignore`).

**Non-Goals:**
- Automatic detection of generated/vendored files (separate change).
- Nested per-directory ignore files (only scan-root `.graphosignore` for now).

## Decisions

- **Match against scan-root-relative normalized paths** (strip the `./` and the
  scan-root prefix before matching).
  - *Alternative considered:* match absolute paths — rejected, patterns would
    need machine-specific prefixes.
- **Read `.graphosignore` from the scan root (the `PATH` argument)**, not CWD.
  - *Alternative considered:* CWD — rejected, surprising when scanning a subdir.
- **Merge CLI `--ignore` patterns after file patterns**; both feed one matcher.
  - *Alternative considered:* CLI overrides file — rejected, additive is least
    surprising.
- **Keep matching pure in Infrastructure.FileSystem.Ignore**; Detect calls it.
  - *Alternative considered:* new module — rejected, reuse existing pattern types.

## Risks / Trade-offs

- [Path normalization edge cases on Windows separators] → normalize to `/` before
  matching; add tests for both separators.
- [Behavior change for anyone relying on the previous no-op] → documented in
  proposal; ignored files simply disappear from the graph.

## Migration Plan

- Additive plus a bug fix; regenerate graph to apply.
- Rollback: remove `.graphosignore` and omit `--ignore`.
- Verify with `cabal test` (Ignore matcher suite) and a `cabal run graphos`
  smoke run confirming a targeted file yields zero nodes.
