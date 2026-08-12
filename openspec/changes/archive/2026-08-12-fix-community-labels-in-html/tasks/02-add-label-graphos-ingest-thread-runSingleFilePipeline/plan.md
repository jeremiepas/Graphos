<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Add `--label` to `graphos ingest` and thread into `runSingleFilePipeline` — PLAN

**Task slug**: `02-add-label-graphos-ingest-thread-runSingleFilePipeline`
**Attempt**: 1
**Status**: pending

## Summary

Add a `--label` switch to the `graphos ingest` CLI command and wire it through the single-file ingest pipeline so that LLM community labeling is invoked when the flag is set, mirroring the full pipeline's `--label` behavior.

## Detail

### Scope

Modify four files to add the `--label` flag and threading path for `graphos ingest`:

1. **`src/Graphos/CLI/Parser.hs`** — Add a `--label` switch to `ingestOpts` (line ~173), mirroring the full pipeline's `--label` at line ~56. Extend the `IngestCmd` constructor to include a `Bool` field (e.g., `cfgLabel :: Bool`).
2. **`src/Graphos/Domain/Types.hs`** (or wherever the `Command`/`IngestCmd` type lives) — Update the `IngestCmd` constructor to carry the `cfgLabel :: Bool` field.
3. **`app/Main.hs`** — In the `IngestCmd` handler, set `cfgLabel` on the `PipelineConfig` record based on the command-line flag value.
4. **`src/Graphos/UseCase/Pipeline/Incremental.hs`** — In `runSingleFilePipeline` (lines ~189-197), when `cfgLabel` is `True`:
   - Call `labelCommunities appEnv enrichedGraph finalCommMap Map.empty lblCfg` (cohesion is `Map.empty` since the ingest path doesn't compute it)
   - Pass the resulting `Map CommunityId Text` to `epExportAll` (changing from `Nothing` to `Just labels`)
   - When `cfgLabel` is `False`, preserve current behavior (call `labelCommunities` nowhere, pass `Nothing` to `epExportAll`)

### Check Criteria (defined BEFORE code)

**Spec scenarios satisfied:**
- `llm-labeling` — "Scenario: --label flag present on ingest" (LLM called, labels in graph.json and graph.html)
- `llm-labeling` — "Scenario: --label flag absent on ingest preserves current behavior" (no LLM call, no labels)
- `llm-labeling` — "Scenario: LLM endpoint unavailable during ingest --label" (non-fatal, log warning, continue)

**Tests/gates to run:**
- `graphos ingest --help` — output includes the `--label` flag description
- `cabal build --flag dev` — exit code 0, zero warnings
- `cabal test` — all examples pass (must still pass; this task is plumbing, but the build must succeed)
- Manual verification (requires working LLM endpoint):
  - `graphos ingest <file> --label` with Ollama running → `graph.json` has `community_labels` key, `graph.html` shows LLM labels
  - `graphos ingest <file>` without `--label` → no `community_labels` key in `graph.json`, `graph.html` shows `Community <id>`

**PASS conditions:**
- `graphos ingest --help` lists `--label` as an available flag
- `cabal build --flag dev` exits 0 with zero warnings
- `cabal test` exits 0, all examples pass
- When `--label` is set, `labelCommunities` is invoked in `runSingleFilePipeline`
- When `--label` is NOT set, `labelCommunities` is NOT invoked (current behavior preserved)
- Labels from `labelCommunities` are passed to `epExportAll` (which already uses them from task 1)

**FAIL boundaries:**
- Build fails due to `IngestCmd` type change not propagating correctly
- `--label` on ingest calls the LLM even when the flag is absent (regression)
- LLM failure during ingest causes a crash (should be non-fatal — `labelCommunities` already returns `Map.empty` on failure per `Label.hs:57-59`)
- `cfgLabel` field conflicts with an existing field in the `IngestCmd` or `PipelineConfig` records

### Affected modules

| File | Change type | Risk |
|------|-------------|------|
| `CLI/Parser.hs` | CLI flag addition | Low — mechanical, follows existing pattern |
| `Domain/Types.hs` | Constructor extension | Low — one `IngestCmd` consumer (Main.hs) |
| `Main.hs` | Argument threading | Low — mechanical change |
| `UseCase/Pipeline/Incremental.hs` | Logic addition | Medium — adds LLM call in the ingest pipeline |

### Prerequisites

- Task 1 completed: `epExportAll` already accepts `Maybe (Map CommunityId Text)` and passes labels to `epExportHTML`
- `labelCommunities` function exists in `UseCase/Label.hs` and is callable with the right signature
- `PipelineConfig` has a `cfgLabel` field (or the field exists already from the full pipeline); if not, it must be added to the config type

### Risks

- **`labelCommunities` requires cohesion**: The full pipeline computes cohesion; the ingest path currently discards it (`_cohesion`). The design passes `Map.empty` to `labelCommunities` — verify that `Label.hs` tolerates an empty cohesion map (the spec says it does, and `labelCommunities` already uses cohesion as optional input for the prompt).
- **LLM non-fatal**: `labelCommunities` already returns `Map.empty` on LLM failure (`Label.hs:57-59`). The ingest pipeline must not treat this as an error — it must continue with `Nothing` or the empty map passed to `epExportAll`.
- **Pre-existing `Extract/Core.hs:155` parse error**: This task does NOT touch that file. The build gate still requires a clean build, which means that pre-existing error must be resolved separately.
