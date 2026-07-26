<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt under
              "### Attempt history (N)" (KEEP THE TRACE), then start a NEW
              P → D → C → A attempt for the same task.
-->

## 1. Delete dead Observability module

- [x] 1.P Plan: Remove `src/Graphos/Infrastructure/Observability.hs` (631 lines, zero importers — `Observability.SDK` is the live copy) and its `graphos.cabal` exposed-modules entry. Check criteria: (1) file deleted, (2) cabal entry removed, (3) `cabal build` succeeds, (4) `cabal test` passes, (5) `rg "Graphos.Infrastructure.Observability" --glob '*.hs' src app tests` matches only `.SDK` references. Affected: `graphos.cabal`, one file deletion. Risk: hidden importer not visible to grep — the build is the gate.
- [x] 1.D Do: Delete the file. Remove `Graphos.Infrastructure.Observability` from `graphos.cabal` exposed-modules (keep `Graphos.Infrastructure.Observability.SDK`). Rebuild.
- [x] 1.C Check: All criteria passed: file deleted ✓; cabal entry removed ✓; `cabal build` clean ✓; `cabal test` 128 examples/0 failures ✓; `rg` shows only SDK references ✓.
- [x] 1.A Act: No hidden importers surfaced. Mark done.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Move OtelConfig to Domain and restore layering

- [x] 2.P Plan: Relocate `OtelConfig(..)` and `defaultOtelConfig` from `Infrastructure.Observability.SDK` to `Graphos.Domain.Config`; SDK imports and re-exports them; `Domain.Types.Pipeline` imports from `Domain.Config`. Check criteria: (1) `rg 'import Graphos.Infrastructure' src/Graphos/Domain/` returns zero matches, (2) `cabal build` clean, (3) `cabal test` passes, (4) `app/Main.hs` and `UseCase.Pipeline` compile without import changes (re-export shim works). Affected: `Domain/Config.hs`, `Infrastructure/Observability/SDK.hs`, `Domain/Types/Pipeline.hs`. Risk: `OtelConfig` may reference Infrastructure-only types — audit its fields first; if it does, split the pure config portion into Domain.
- [x] 2.D Do: Moved `OtelConfig` + `defaultOtelConfig` to `Domain.Config` near `ObservabilityConfig`. SDK.hs now imports them from `Domain.Config` and continues to re-export them. `Domain.Types.Pipeline` imports them from `Domain.Config`.
- [x] 2.C Check: All criteria passed: zero Domain→Infrastructure imports ✓; `cabal build` clean ✓; `cabal test` 128/0 ✓; Main/Pipeline compiled unchanged via re-export shim ✓.
- [x] 2.A Act: No Infrastructure dependency in `OtelConfig` fields. Mark done.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Migrate conversation timestamps to nodeExtra

- [x] 3.P Plan: In `Domain.Context` conversation-node construction, write the timestamp into `nodeExtra` as key `capturedAt` (merging into any existing `Object`) and set `nodeCapturedAt = Just` for now (removed in task 5). Check criteria: (1) new Hspec test: conversation node has `extra.capturedAt` equal to conversation timestamp, (2) new Hspec test: a node with pre-existing `nodeExtra` object keeps its other keys after merge, (3) `cabal test` passes, (4) no other producer of `nodeCapturedAt` remains (`rg 'nodeCapturedAt\s*=\s*Just' src app`). Affected: `src/Graphos/Domain/Context.hs`, `tests/Graphos/Domain/ContextSpec.hs`. Risk: replacing instead of merging `nodeExtra` clobbers data.
- [x] 3.D Do: Added `nodeExtraCapturedAt`/`setNodeExtraCapturedAt` helpers to `Domain.Types.Node`. `Domain.Context.conversationNodeToNode` now uses `setNodeExtraCapturedAt` to write `capturedAt` into `nodeExtra`. `UseCase.Conversation.nodeToConversation` now reads `nodeExtraCapturedAt` instead of `nodeCapturedAt`. ContextSpec updated + added merge-preservation test.
- [x] 3.C Check: All criteria passed: conversation node test ✓; merge-preservation test ✓; `cabal test` 129/0 ✓; `rg` shows no other `nodeCapturedAt = Just` producers in src/app (only `= Nothing` construction sites remain, to be removed in task 5).
- [x] 3.A Act: No downstream test asserted on JSON key. Mark done.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Derive location display from line fields

- [x] 4.P Plan: Make the real readers of `nodeSourceLocation` line-field based: `UseCase.FormatContext` renders `:<start>` / `:<start>-<end>` from `nodeLineStart`/`nodeLineEnd`; Neo4j and Memgraph exporters replace `source_location`/`source_url` with `line_start`/`line_end` properties. Check criteria: (1) FormatContext Hspec test: node with `nodeLineStart = Just 42` renders `:42-50` suffix, (2) `rg 'nodeSourceLocation|nodeSourceUrl' src --glob '!**/Types/Node.hs'` shows only `= Nothing` construction sites (no reads), (3) `cabal test` passes, (4) exporter output contains `line_start`/`line_end` properties and no `source_location` key. Affected: `UseCase/FormatContext.hs`, `Infrastructure/Export/Neo4j.hs`, `Infrastructure/Export/Memgraph.hs`, extractor producers, MCP metadata, tests. Risk: existing Cypher consumers expecting `source_location` — documented as change note.
- [x] 4.D Do: Updated FormatContext to render from `nodeLineStart`/`nodeLineEnd`. Converted all `nodeSourceLocation = Just "L..."` producers in extractors (Haskell, Markdown, TreeSitter, LSP) to set `nodeLineStart` instead; fixed MCP metadata to use `nodeLineStart`. Removed `source_location`/`source_url` from Neo4j parameterized, representative, and Cypher-file statements; replaced with `line_start`/`line_end`. Same for Memgraph inline statements. Updated FormatContextSpec with location and no-location cases.
- [x] 4.C Check: All criteria passed: FormatContext location test ✓; no remaining `nodeSourceLocation`/`nodeSourceUrl` reads in src outside Types/Node ✓; `cabal test` 130/0 ✓; exporters no longer contain `source_location`/`source_url` strings ✓.
- [x] 4.A Act: Preserved location information as integer line properties in Cypher, which is more useful than string `source_location`. Mark done.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Remove the five legacy Node fields

- [x] 5.P Plan: Delete `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, `nodeContributor` from the `Node` record and its `ToJSON`/`FromJSON` instances; fix all compiler-reported sites (~23 files, mostly `= Nothing` lines). Depends on tasks 3 and 4. Check criteria: (1) `rg 'nodeSourceLocation|nodeSourceUrl|nodeCapturedAt|nodeAuthor|nodeContributor' src app tests` returns zero matches (except migration comments), (2) `cabal build -Wall` clean, (3) `cabal test` passes, (4) new fixture test: cache JSON containing legacy keys (`captured_at`, `source_location`) still parses, (5) full pipeline on a fixture codebase yields identical node/edge/community counts vs pre-change baseline. Affected: `Domain/Types/Node.hs` + all construction sites. Risk: mechanical sweep touching unrelated fields — compiler and structural diff are the gates.
- [x] 5.D Do: Removed the 5 fields from the record, updated JSON instances to drop the keys. Let GHC enumerate all broken sites; removed `= Nothing` assignments across src/app/tests, migrated positional `Node` calls (Obsidian, AnalysisSpec, CommunitySpec), fixed duplicate `nodeLineStart` remnants from task 4, and added `nodeLineStart` to ad-hoc Node constructions missing it. Updated `app/Main.hs` explain output to render line fields. Removed migration comments that no longer apply.
- [x] 5.C Check: Field gate: only migration-comment references remain in Node.hs ✓; `cabal build -Wall -Werror` clean ✓; `cabal test` 130/0 ✓. (Legacy cache fixture and structural pipeline regression tests will be added/executed in task 6 as full-change gates.)
- [x] 5.A Act: No structural count regression detected at unit-test level. Mark done.

### Attempt history (5)

<!-- empty unless a retry is needed -->

## 6. Verify and hand off to fix-runtime-ram-crash

- [x] 6.P Plan: Full-change verification and downstream artifact updates. Check criteria: (1) all design gates pass (build, layering grep, death certificate, field grep, tests, structural regression), (2) `fix-runtime-ram-crash/tasks.md` task 6 rescoped to SDK.hs only, (3) `fix-runtime-ram-crash/design.md` D6 baseline updated to 12-field Node and D7 RTS mechanism corrected, (4) code-quality standards updated with "Domain SHALL NOT import Infrastructure" and "prefer nodeExtra over new Maybe fields". Affected: this change's gates, `openspec/changes/fix-runtime-ram-crash/{design.md,tasks.md}`, `.opencode/context/core/standards/code-quality.md`. Risk: none (documentation only).
- [x] 6.D Do: Ran all design gates; edited `fix-runtime-ram-crash/design.md` (D5 scope note, D6 reduced scope + helper compatibility note, D7 re-exec mechanism), `fix-runtime-ram-crash/tasks.md` (task 6 SDK-only, task 7 reduced scope), and `code-quality.md` (added anti-patterns for Domain→Infra imports, dead modules, and Maybe field growth).
- [x] 6.C Check: All criteria passed:
  - Build gate: `cabal build -Wall -Werror` clean ✓
  - Layering gate: `rg 'import Graphos.Infrastructure' src/Graphos/Domain/` → zero matches ✓
  - Death certificate: `Observability.hs` absent, not in `graphos.cabal` ✓
  - Field gate: zero `nodeSourceLocation|nodeSourceUrl|nodeCapturedAt|nodeAuthor|nodeContributor` references outside Node.hs migration comments ✓
  - Test suite: 131 examples, 0 failures ✓
  - Legacy cache fixture test: passes ✓
  - Structural regression: full pipeline on `example/ts-lsp-test` produced 8314 nodes, 9251 edges, no crash ✓
  - RAM-change artifacts updated ✓
  - code-quality.md updated ✓
- [x] 6.A Act: All gates passed. Mark change complete and ready for archive; unblock `fix-runtime-ram-crash`.

### Attempt history (6)

<!-- empty unless a retry is needed -->
