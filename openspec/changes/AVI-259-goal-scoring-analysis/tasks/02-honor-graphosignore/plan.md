# Task: Honor .graphosignore Patterns

## Goal

Implement `.graphosignore` file parsing and application to exclude files/directories from ingestion.

## Score: 13.44 (P0) — Second highest priority

## Acceptance Criteria

- [ ] `.graphosignore` file parsed from project root
- [ ] Gitignore-compatible pattern matching (glob, regex)
- [ ] Excluded files skipped during ingestion
- [ ] Config override supported (graphos.yaml ignorePatterns)
- [ ] Tests for pattern matching edge cases

## Dependencies

- None (build first)

## Blocks

- All ingestion features (8 features)
- detect-generated-vendored-code
- fix-runtime-ram-crash (extraction sub-spec)

## Implementation Plan

1. Read existing `gitignore-parsing` spec for patterns
2. Implement `.graphosignore` parser (reuse gitignore logic)
3. Wire into ingestion pipeline (skip excluded paths)
4. Add config override support
5. Add tests for pattern matching

## Verification

- Create test project with `.graphosignore` containing various patterns
- Run graphos ingest and verify excluded files are skipped
- Test config override
