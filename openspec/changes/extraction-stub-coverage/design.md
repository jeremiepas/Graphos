## Context

See proposal.md - Why. The stub count for any extension is logged at
`logDebug` (`UseCase/Extract/Core.hs`, the `stubFiles` list from
`partitionByExtractor`), invisible without `--debug`, and `GRAPH_REPORT.md`
never reports extraction coverage at all. This change makes that visible,
independent of which language or config gap caused the stub fallout.

(This change originally also added C# tree-sitter support, motivated by the
Bano corpus where that gap combined with silent stub reporting to produce a
near-empty graph. The C# grammar — along with PHP and Perl, and Java import
resolution — shipped independently on `main` via PR #134 (`AVI-1716`/
`AVI-1780`) before this change's implementation started. That work is no
longer part of this change's scope; see proposal.md.)

## Goals / Non-Goals

**Goals:**
- Any extension whose files route to `ExtractStub` is surfaced at INFO with a
  per-extension breakdown, and the same counts appear in `GRAPH_REPORT.md`.
- A majority-stubbed corpus names itself as such in the warning.

**Non-Goals:**
- Any grammar, extension registration, or import-resolution work — that is
  `AVI-1716`'s territory (merged) and #116/#158's remaining follow-ups
  (Kotlin import-edge wiring, Scala/Elixir/Bash/HCL/Svelte/Vue grammars,
  `.c`/`.h` grammar linking).
- Detecting stubs produced by a *different* failure mode: a tree-sitter file
  whose grammar is missing or whose parse fails mid-file already falls back
  to `makeStubNode` inside `TreeSitter.hs`, and is already covered by the
  pre-existing aggregate `missingGrammars` WARN (`Core.hs:116-119`). This
  change's coverage counters only cover the `stubFiles` bucket from
  `partitionByExtractor` (files statically routed to `ExtractStub` because no
  `defaultExtractors`/config entry exists for their extension). Widening
  "stub" to a post-hoc inspection of every produced extraction (e.g. reusing
  or fixing `isStubExtraction`, which checks `nodeKind == Just "File"` and so
  does not even match real `makeStubNode` output where `nodeKind = Nothing`)
  is out of scope here.

## Decisions

- **Coverage counters are computed from the existing `stubFiles` partition,
  not a new accounting structure.** `partitionByExtractor` already produces
  exactly the file list this feature needs; a `StubCoverage` record
  (`scDetected`, `scStructural`, `scStubbedByExt`) is derived from its
  structural (tree-sitter ++ LSP) and stub output lists with one pure
  function (`stubCoverage`) in `UseCase/Extract/Core.hs`, reused by both the
  INFO warning (`stubCoverageWarning`) and the report section. Alternative
  considered: track per-file extraction outcomes (stub vs. structural) as
  each file is actually processed, which would also catch
  tree-sitter-attempted-but-failed files — rejected per the Non-Goals note
  above (would need touching the parallel/cached extraction loop's
  accounting, not just the pre-computed partition, for no scenario this
  change needs to cover).
- **`GRAPH_REPORT.md`'s coverage section recomputes the partition locally in
  `generateReport`, rather than threading new state through the pipeline.**
  `generateReport :: Graph -> Analysis -> PipelineConfig -> Detection -> ...`
  already receives `PipelineConfig` and `Detection` (both were previously
  underscore-prefixed/unused) — `partitionByExtractor config codeFiles` where
  `codeFiles = detectionFiles detection Map.! CodeFiles` reproduces the same
  three-way split `extractAll` computed, with no IO and no new field on
  `Extraction`/`AppEnv`/the pipeline's checkpoint state. Alternative
  considered: have `extractAll` return/store the `StubCoverage` value and
  thread it through to the export stage — rejected as unnecessary plumbing
  across pipeline stage boundaries for a value derivable purely from inputs
  already in scope at the call site, and consistent with `generateReport`'s
  existing "pure rendering, no whole-graph recomputation" contract (it already
  recomputes small summaries like this from its inputs rather than carrying
  precomputed state for everything).
- **Threshold is a top-level exported constant**, `stubDominanceThreshold ::
  Double` (default `0.5`) in `UseCase/Extract/Core.hs`, not a `PipelineConfig`
  field or CLI flag — tasks.md specifies "a named exported threshold", not a
  user-facing setting, and no proposal/spec scenario asks for configurability.

## Risks / Trade-offs

- [The coverage section and the INFO warning can disagree in a case this
  change doesn't produce today: a file assigned to tree-sitter whose grammar
  is missing counts as "structural" in `scStructural` (it was *routed* to
  tree-sitter) even though it ends up a `makeStubNode` fallback at runtime.]
  → Documented as a Non-Goal above; the pre-existing `missingGrammars` WARN
  already flags that case by grammar name, so it isn't silent — just counted
  under a different bucket than this change's new coverage line.
- [`stubCoverageWarning`'s dominance clause and `GRAPH_REPORT.md`'s coverage
  section both derive from `partitionByExtractor`, so a bug in one pure
  function affects both outputs identically rather than being caught by
  disagreement between two independent computations.]
  → Mitigated by unit-testing `stubCoverage`/`stubCoverageWarning` directly in
  addition to the rendered report string, so the shared logic is covered at
  the function level, not only through one integration path.

## Migration Plan

Additive only: new log line, new report section. No existing output is
removed or renumbered; `GRAPH_REPORT.md` consumers that parse it by section
order should tolerate a new section (matches the file's existing append-only
section history). No rollback beyond reverting the commit — no data
migration, no schema change.
