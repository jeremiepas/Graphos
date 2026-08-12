<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Documentation — PLAN

**Task slug**: `05-documentation`
**Attempt**: 1
**Status**: pending

## Summary

Create `docs/embedding-models.md` with a trade-offs table comparing embedding models, covering local vs hosted options, dimensions, quality characteristics, latency, and cost. This helps users pick the right model for mixed code+docs corpora. Covers subtask 5.1.

## Detail

### Scope

- **New file**: `docs/embedding-models.md`
- **Content**:
  - Trade-offs comparison table with columns: model name, local/hosted, dimension, code-prose quality, latency, cost
  - Models to document:
    - `nomic-embed-text` — local, 768-dim, default model
    - `all-minilm` — local, 384-dim, faster, lower quality
    - `bge-m3` — local, 1024-dim, multilingual, better code+prose
    - `voyage-code-2` — hosted, 1536-dim, code-specialized
    - `text-embedding-3-small` — OpenAI, 1536-dim, hosted
  - Guidance note: for semantic code↔doc edges, a model that embeds code identifiers AND prose into a shared space is required — `nomic-embed-text` works but `bge-m3` or `voyage-code-2` recommended for mixed corpora
  - How to set via `embedding.model` in `graphos.yaml`
  - Brief section on model selection for different use cases (mixed corpus vs code-only vs docs-only)

### Check Criteria

**Tests to run**:
- No automated tests for documentation — verification is manual
- Confirm file exists at `docs/embedding-models.md`
- Confirm all 5 models are documented with accurate information
- Confirm trade-offs table is readable and well-formatted
- Confirm `embedding.model` configuration syntax is correct (matches existing config schema)

**Spec scenarios satisfied**:
- No spec scenarios — this is documentation only
- Supports user decision-making for model selection (implicit from proposal)

**PASS conditions**:
- File exists at `docs/embedding-models.md`
- All 5 models are listed: `nomic-embed-text`, `all-minilm`, `bge-m3`, `voyage-code-2`, `text-embedding-3-small`
- Each model has: local/hosted status, dimension, code-prose quality assessment, latency characteristics, cost
- Recommendation note is present: `bge-m3` or `voyage-code-2` recommended for mixed corpora
- Configuration syntax section shows how to set `embedding.model` in `graphos.yaml`
- No broken links or incorrect technical claims

**FAIL boundaries**:
- If any model is missing from the table, the test fails
- If the configuration syntax is incorrect (doesn't match actual `graphos.yaml` schema), the test fails
- If the file contains placeholder text (e.g., "TBD", "insert details here"), the test fails

### Affected modules

- **New**: `docs/embedding-models.md` — markdown documentation file
- **No code changes** — purely documentation

### Prerequisites

- Knowledge of the embedding models is available from the project context (proposal.md and design.md already specify the models)
- `embedding.model` configuration field exists in the config schema

### Risks

- **Low**: Documentation is factual and derived from existing design docs — no code to break
- **Low**: No impact on build, tests, or runtime behavior
- **Medium**: Information accuracy — model specifications (dimensions, latency, cost) could become outdated. Add a note about verifying model specs before publishing
