# Task 2 — Verify the `devenv-shell` spec delta captures the invariant — DO

**Task slug**: `02-verify-devenv-shell-spec-delta`
**Attempt**: 1
**Status**: in-progress

## Summary

Verified the ADDED requirement in `specs/devenv-shell/spec.md` (within the `fix-ci-missing-hackage-package-list` change) correctly codifies the "CI build task refreshes the Hackage package index before configure" invariant.

## Detail

### What was done

- Reviewed `openspec/changes/fix-ci-missing-hackage-package-list/specs/devenv-shell/spec.md`:
  - `## MODIFIED Requirements` header present (baseline requirement preserved)
  - `## ADDED Requirements` header present at line 25
  - Requirement body uses SHALL/MUST language correctly
  - All 4 scenarios use `#### Scenario:` (4 hashtags)
  - Each scenario has WHEN/THEN clauses
  - Requirement mentions both "cabal update" and "cabal configure" explicitly

- Ran `openspec validate --changes --json`:
  - `fix-ci-missing-hackage-package-list` returned `"valid": true`
  - No schema errors in this change's spec

### Key decisions

- No spec text changes were needed — the delta file was already correctly structured.
- The MODIFIED section includes the baseline requirement (unchanged content) because the spec is being extended by the ADDED requirement.
