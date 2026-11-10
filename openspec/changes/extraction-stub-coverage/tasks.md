## 1. C# grammar binding and default extractor — superseded

Dropped from this change: PR #134 (`AVI-1716`/`AVI-1780`, merged
2026-11-01) already shipped C#/PHP/Perl tree-sitter grammars (vendored
`tree-sitter-c-sharp`/`tree-sitter-perl`, Hackage `tree-sitter-php`) and Java
import resolution, done independently of and before this change's
implementation. See proposal.md "Why" for the full account, including a
dead-end investigation (chased here first, before discovering #134) into a
real packaging bug in the Hackage `tree-sitter-c-sharp` 0.1.0.1 release
(ships `parser.c` but omits `scanner.c`) — moot now since `main`'s version
vendors the grammar fully and never depends on that Hackage package at all.

## 2. Visible stub coverage (UseCase + Report)

- [x] 2.1 In `UseCase/Extract/Core.hs`, replaced the `logDebug "  stub: N files"` line with a counted INFO warning (`logInfo`) emitted whenever `stubFiles` is non-empty: `[extract] WARNING: N code files produced stub nodes only (no structure extracted): .scala (K), .xml (M)` — grouped/counted by extension via the new `StubCoverage`/`stubCoverage`/`stubCoverageWarning` pure functions. Unit tests cover the pure functions directly rather than a fake logger (`ExtractSpec`, "stubCoverage / stubCoverageWarning"): a 3-stubbed corpus names `.scala (3)`; an all-structural corpus is silent. Verified with `cabal test`.
- [x] 2.2 `GRAPH_REPORT.md` gains an "Extraction Coverage" section in `UseCase/Report.hs`: `Files detected: N`, `Files with structure: M`, `Stubbed: K (.scala K1, …)`. Implemented by recomputing `partitionByExtractor` from the `PipelineConfig`/`Detection` already passed into `generateReport` (both previously unused) rather than threading new state through the pipeline — see `design.md`. Unit test: 8 structural / 34 stubbed (`.scala`) renders all three lines with the right numbers (`ReportSpec`). Verified with `cabal test`.
- [x] 2.3 Added the dominated-extraction warning: when `stubbedCodeFiles / detectedCodeFiles` exceeds the exported `stubDominanceThreshold` (default `0.5`), `stubCoverageWarning` appends `-- the graph likely under-represents this corpus; dominant stubbed type: .scala`. Unit tests: 34/42 stubbed trips it naming `.scala`; 1/42 does not (`ExtractSpec`). Verified with `cabal test`.

## 3. End-to-end validation

- [x] 3.1 `ReportSpec` renders a corpus of 8 structural (`.ts`) + 34 stubbed (`.scala`, genuinely unsupported per #158) files and asserts `Files detected: 42`, `Files with structure: 8`, `Stubbed: 34 (.scala 34)`; a second case with every file structural asserts `Stubbed: 0`. `ExtractSpec` covers the same `stubCoverage`/`stubCoverageWarning` logic directly at the function level (count/breakdown, silent case, dominance trip/no-trip). Verified with `cabal test` (full suite green; see commit message for the exact count at commit time).
- [ ] 3.2 Manual acceptance: run graphos on a real corpus with a genuinely unsupported language dominating it (the original Bano/C# scenario no longer reproduces this now that #134 shipped C#) and confirm the INFO warning and `GRAPH_REPORT.md` coverage section appear as expected. Not run in this environment — no such corpus was available; the unit/rendering tests above are the verification for this change.
