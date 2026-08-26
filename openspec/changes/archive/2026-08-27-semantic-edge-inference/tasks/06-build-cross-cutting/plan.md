# Task 6 — Build + cross-cutting — PLAN

**Task slug**: `06-build-cross-cutting`
**Attempt**: 1
**Status**: pending

## Summary

Final verification: legacy graph compatibility, `-Wall -Werror` clean build, all tests green (existing + new), and manual end-to-end validation against a mixed corpus. This is the integration gate that validates all prior tasks work together. Covers subtasks 6.1 (legacy compatibility), 6.2 (build + warnings), and 6.3 (manual verification).

## Detail

### Scope

- **Legacy graph compatibility** (Task 6.1):
  - Verify `graph.json` without `embeddings_path` loads without crash
  - Verify `gEmbeddings = Nothing` for legacy graphs
  - Verify all existing query-family commands work on legacy graphs (no regression)
  - Test fixture: a minimal `graph.json` from `graphos-out/` or existing test data, without `embeddings_path`

- **Build verification** (Task 6.2):
  - `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings
  - `cabal test` — green (all existing tests + all new Hspec cases from Tasks 1-5)
  - No regressions in existing test output

- **Manual mixed-corpus verification** (Task 6.3):
  - Build a mixed corpus: this repo (code) + `docs/` (markdown) with `--embed`
  - Confirm: `embeddings.json` sidecar exists in output directory
  - Confirm: `graph.json` has `embeddings_path: "embeddings.json"`
  - Confirm: semantic `References` edges appear in the graph between docs and code with different names (e.g., `docs/auth.md` ↔ `login.ts`)
  - Confirm: `--no-semantic-edges` reproduces today's clustering (literal-name only, no semantic edges)
  - Confirm: single-corpus (code-only) run skips semantic pass automatically (log message + zero semantic edges)

### Check Criteria

**Tests to run**:
- `cabal build --flag dev` — exits 0, no warnings
- `cabal test` — exits 0, all tests pass (exit code 0, 100% pass rate)
- Manual: load a legacy `graph.json` (without `embeddings_path`) — no crash, `gEmbeddings = Nothing`
- Manual: run `graphos query` commands on legacy `graph.json` — all commands succeed without error
- Manual: run pipeline on mixed corpus with `--embed` — `embeddings.json` exists, `graph.json` has pointer
- Manual: inspect `graph.json` edges — semantic `References` edges between doc and code nodes visible
- Manual: run pipeline with `--no-semantic-edges` — no semantic edges in output
- Manual: run pipeline on code-only corpus — skip log message present, zero semantic edges

**Spec scenarios satisfied**:
- `Scenario: Legacy graph loads without embeddings` (spec `semantic-edge-inference` § "Legacy graph loads without embeddings", spec `embedding` § "Legacy graph loads without embeddings")
- `Scenario: Config disabled skips semantic pass` (spec `semantic-edge-inference` — via `--no-semantic-edges` flag)
- `Scenario: Pure-code graph skips semantic inference` (spec `semantic-edge-inference` — code-only corpus)
- All scenarios from Tasks 1-5 (final gate before completion)

**PASS conditions**:
- `cabal build --flag dev` completes with exit code 0 and zero warnings
- `cabal test` completes with exit code 0 and 100% test pass rate (no regressions, all new tests pass)
- Legacy `graph.json` loads successfully with `gEmbeddings = Nothing`
- All existing query commands work on legacy graphs without errors
- Mixed corpus pipeline: `embeddings.json` sidecar exists, `graph.json` has `embeddings_path` pointer
- Mixed corpus: semantic `References` edges connect doc and code nodes with different names
- Mixed corpus with `--no-semantic-edges`: no semantic edges in output, literal-name edges only
- Code-only corpus: log message "single-corpus graph detected, skipping semantic edge inference" present, zero semantic edges

**FAIL boundaries**:
- If `cabal build` produces any warnings, the test fails (strict build policy)
- If any existing test regresses, the test fails (no acceptable regressions)
- If legacy `graph.json` crashes or produces malformed output, the test fails
- If `embeddings.json` is not written when `--embed` is passed, the test fails
- If no semantic `References` edges appear in the mixed corpus output, the test fails
- If `--no-semantic-edges` does NOT suppress semantic edges, the test fails
- If code-only corpus does NOT log skip message, the test fails

### Affected modules

- No code changes in this task — verification only
- All modules from Tasks 1-5 are implicitly tested
- Legacy graph fixture: a `graph.json` without `embeddings_path` field (test fixture from existing test data)
- Mixed corpus test: this repo (`src/`, `test/`) + `docs/` directory as input

### Prerequisites

- All Tasks 1-5 must be implemented and passing
- A mixed corpus input available: this repo code + docs directory
- Access to a legacy `graph.json` (without `embeddings_path`) for compatibility testing
- A code-only corpus test fixture for single-corpus verification

### Risks

- **Medium**: Performance on mixed corpus — if the cosine similarity pass is slow on the test corpus, manual verification may take a long time. The 10K code node cap should prevent this, but the mixed corpus may still be large.
- **Medium**: Edge quality on real data — semantic edges may or may not produce meaningful connections on the test corpus. If they don't, the question is whether the model is wrong, the threshold is too high, or the corpus is too small. This is not a build failure but requires human judgment.
- **Low**: Legacy graph compatibility — tested via explicit fixture, low risk of unexpected breakage
- **Low**: Build verification — standard gate, straightforward to fix if there are issues
