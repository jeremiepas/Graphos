<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — End-to-end manual acceptance + regression pass — PLAN

**Task slug**: `04-e2e-manual-acceptance-regression`
**Attempt**: 1
**Status**: pending

## Summary

Run the full manual acceptance from the `design.md` Verification Strategy on a real codebase (e.g., `example/` or a small fixture) with and without `--label`, for both the full pipeline and `graphos ingest`, to confirm that LLM labels appear in `graph.html` and `graph.json`.

## Detail

### Scope

No code changes. This task is purely verification: execute the manual acceptance scenarios from the design document and the spec scenarios, document results, and confirm no regressions.

**Verification steps:**

1. **Build gate**: Run `cabal build --flag dev` and confirm zero warnings.
2. **Unit tests**: Run `cabal test` and confirm all examples pass (target: 347+ examples including the new tests from task 3).
3. **Full pipeline with `--label`**: Run `cabal run graphos -- example/ --label` (requires a working LLM endpoint — e.g., local Ollama). Open `graphos-out/graph.html` and verify:
   - Sidebar community list shows LLM labels (e.g., `"Authentication Module"`) instead of `"Community <id>"`
   - Overview dot tooltips include LLM labels
   - `graphos-out/graph.json` contains a `community_labels` key with non-empty map
4. **Ingest with `--label`**: Run `cabal run graphos -- ingest <file> --label`. Open `graphos-out/graph.html` and `graphos-out/graph.json` — same verification as step 3.
5. **Full pipeline without `--label` (regression)**: Run `cabal run graphos -- example/`. Open `graphos-out/graph.html` — community list shows `"Community <id>"` fallback, no crash.
6. **Ingest without `--label` (regression)**: Run `cabal run graphos -- ingest <file>`. Verify `graph.json` has no `community_labels` key and `graph.html` shows `"Community <id>"` fallback.

### Check Criteria (defined BEFORE code)

**Spec scenarios satisfied:**
- `llm-labeling` — "Scenario: --label flag present on ingest" (step 4)
- `llm-labeling` — "Scenario: --label flag absent on ingest preserves current behavior" (step 6)
- `llm-labeling` — "Scenario: LLM endpoint unavailable during ingest --label" (if LLM unavailable during steps 3-4)
- `html-lod-viewer` — "Aggregate fields populated with LLM label" (steps 3-4, visual verification)
- `html-lod-viewer` — "Fallback when no labels provided" (steps 5-6, visual verification)
- `html-lod-viewer` — "HTML viewer shows label in sidebar" (steps 3-4, browser verification)

**Tests/gates to run:**
- `cabal build --flag dev` — exit code 0, zero warnings
- `cabal test` — exit code 0, all examples pass
- Manual browser verification (requires LLM endpoint):
  - Steps 3-4: Open `graph.html`, verify LLM labels visible
  - Steps 5-6: Open `graph.html`, verify `"Community <id>"` fallback visible
- JSON inspection:
  - Steps 3-4: `jq '.community_labels' graphos-out/graph.json` shows non-empty map
  - Step 6: `jq '.community_labels' graphos-out/graph.json` shows `null` or key absent

**PASS conditions:**
- `cabal build` exits 0 with zero warnings
- `cabal test` exits 0 with all examples passing
- With `--label`: `graph.html` shows LLM labels in sidebar and overview dot
- With `--label`: `graph.json` has non-empty `community_labels` key
- Without `--label`: `graph.html` shows `"Community <id>"` fallback (no regression)
- Without `--label`: `graph.json` has no `community_labels` key (no regression)

**FAIL boundaries:**
- Build fails (would indicate regression in earlier tasks — re-check tasks 1-3)
- `cabal test` fails (would indicate broken tests from task 3)
- `graph.html` with `--label` still shows `"Community <id>"` instead of LLM labels (threading broken)
- `graph.html` without `--label` crashes or shows unexpected output (regression)
- `graph.json` missing `community_labels` key when `--label` is set (export path broken)
- LLM endpoint unreachable with `--label` causes crash instead of non-fatal fallback (task 2 bug)

### Affected modules

No code changes. This task reads and inspects outputs from:
- `graphos-out/graph.html` — manual browser verification
- `graphos-out/graph.json` — `jq` inspection
- Console output — warning messages for LLM failures

### Prerequisites

- Tasks 1-3 completed and passing (build gate + unit tests green)
- A working LLM endpoint available (local Ollama at `localhost:11434` with `llama3.2` model, or equivalent OpenAI-compatible endpoint)
- An example fixture available (e.g., `example/` directory or a small test file)

### Risks

- **No LLM endpoint in CI/environment**: If no LLM endpoint is available, manual steps 3-4 are deferred. The unit tests (tasks 1-3) provide automated coverage. Document the deferral.
- **Stale `graphos-out/` from previous runs**: Clear `graphos-out/` before each test to avoid confusion between old and new output files.
- **Partial labeling edge case**: If only some communities are labeled by the LLM, the sidebar should show a mix of LLM labels and `"Community <id>"` fallbacks. This is expected behavior — not a fail.
- **Browser rendering differences**: The sidebar community list rendering is a JavaScript concern; the verification checks that the embedded `_communityAggregatesData` has the correct JSON structure. The JS viewer behavior (sidebar, tooltips) is a best-effort visual check — the data correctness is the primary concern.
