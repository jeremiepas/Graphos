## Why

Graphos used to silently discard C# source. On a real corpus — the `retro-doc`
Bano sample (a detached OutSystems application, 52 files: 42 `.cs`, 8 `.js`,
1 `.csproj`, 1 `.css`) — a full run produced **589 nodes, of which 577 came
from a single `NTR.model.js`**; only **4 of the 42 `.cs` files** yielded a
node, and every one of those four was a bare stub (`kind = null`, no line
range). The business logic of such an application lives in the generated C#
ScreenServices (`Action*_Model.cs`: validations, queries, `if`/`raise`/
`message`), so graphos mapped ~0 % of the thing a reader actually wants and
~98 % of its nodes were data-model plumbing from one JS file.

Two defects combined to produce that:

1. **No C# extractor** (GitHub #116, #158). This half is now **fixed on
   `main`**: PR #134 (`AVI-1716`/`AVI-1780`, merged 2026-11-01) vendors
   `tree-sitter-c-sharp` (plus PHP and Perl) directly into `graphos.cabal`
   (`vendor/tree-sitter-c-sharp/src/{parser.c,scanner.c}`, grammar name
   `csharp`) and registers `.cs` in `defaultExtractors`. That PR predates this
   change's implementation; the C# grammar is no longer this change's
   concern. (It shipped independently of, and a day after, the planning-only
   `AVI-1765` change that originally scoped this work.)

2. **Stub fallout is invisible.** Files that route to `ExtractStub` never
   reach the "No tree-sitter grammar binding for …" warning (that warning
   only covers tree-sitter-routed files with a missing grammar). The stub
   count is emitted at `logDebug`, so a default run gives the user no signal
   that the majority of the input was reduced to empty nodes. The
   `GRAPH_REPORT.md` summary reports only totals, never coverage. **This half
   is still unfixed on `main`** and is this change's sole remaining scope.
   It generalizes #158's "no warning storm" expectation from grammar-mismatch
   stubs to every stub source — including the languages #116/#158 still list
   as missing (Kotlin import-edge wiring, Scala, Elixir, Bash, HCL, Svelte,
   Vue, `.c`/`.h`).

The result, before this change, was a confident-looking graph that could be
near-empty of the corpus's actual content, with nothing in the normal output
telling the user so. That is a correctness-of-signal problem independent of
which specific language is missing a grammar on any given day.

## What Changes

- **Surface stub coverage.** The extract stage SHALL emit a visible (INFO,
  not DEBUG) warning whenever any detected code file routes to `ExtractStub`,
  naming the count and the extensions affected (e.g. `3 code files produced
  stub nodes only (no structure extracted): .scala (3)`). The
  `GRAPH_REPORT.md` summary SHALL gain an "Extraction Coverage" section:
  files detected, files that produced structural (non-stub) nodes, and the
  stubbed count by extension. This makes a near-empty extraction self-evident
  regardless of which language is responsible.
- **Treat a dominated/near-empty extraction as a reportable condition.** When
  the share of code files reduced to stubs exceeds a named threshold, the
  warning SHALL state that the graph likely under-represents the corpus and
  name the dominant stubbed extension.

## Capabilities

### Modified Capabilities
- `extraction-quality`: a new requirement makes stub extraction a visible,
  reported outcome (INFO warning + coverage section in `GRAPH_REPORT.md`), so
  silent near-empty extractions cannot pass unnoticed.

## Impact

- **UseCase**: `Extract/Core.hs` (promote stub reporting from `logDebug` to a
  counted INFO warning via new `StubCoverage`/`stubCoverage`/
  `stubCoverageWarning`/`stubDominanceThreshold` exports); `Report.hs` (new
  "Extraction Coverage" section, computed by recomputing
  `partitionByExtractor` from the `PipelineConfig`/`Detection` already passed
  into `generateReport`, not by threading new state through the pipeline).
- **Tests**: pure unit tests for `stubCoverage`/`stubCoverageWarning`
  (`ExtractSpec`) covering the count/breakdown, the silent case, and the
  dominance-threshold trip/no-trip; a `GRAPH_REPORT.md` rendering test
  (`ReportSpec`) for the coverage section's three lines.
- **No breaking changes**: existing graphs and consumers are unaffected; the
  new output is additive (one warning line, one report section).
- **Scope boundary**: this change does *not* touch any grammar, extension
  registration, or import-resolution code — that is `AVI-1716`'s territory
  (already merged) and #116/#158's remaining follow-ups (Kotlin import
  wiring, the other missing languages). This change is purely about making
  *any* stub fallout visible, whatever causes it.
- **Issue tracking**: this does not close #116 or #158 (neither issue is
  about report visibility) — it is cross-referenced from both as a related,
  independent fix. See the corrective comments posted on both issues noting
  that #134 already resolved the C#/PHP/Perl/Java-imports portions those
  issues originally tracked.
