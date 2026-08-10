# Task 4 — Add Infrastructure writer for global install with idempotency — PLAN

**Task slug**: `04-global-infrastructure-writer`
**Attempt**: 1
**Status**: pending

## Summary

Implement the `runInstallSkill` handler in Infrastructure: resolve the user-level `~/.agents/skills/` root, create directories, write planned files only if absent, skip existing files with a message, and exit successfully.

## Detail

### Scope
- Implement `runInstallSkill :: InstallSkillTarget -> IO ()` (or equivalent) in Infrastructure.
- Resolve the output root to `getHomeDirectory </> ".agents" </> "skills"` in production.
- Accept an injectable root directory for tests (e.g., via a function argument or environment).
- Reuse existing idempotent write/skip logic from `init` scaffolding.
- Generate both global skill files for `OpencodeTarget` using the UseCase planner.

### Check Criteria (defined before code)
- C1: Writing to a temporary directory creates both `graphos/SKILL.md` and `graphos-query/SKILL.md`.
- C2: A second write to the same directory skips both files and exits 0.
- C3: If one file exists and the other does not, only the missing one is created; the existing one is skipped.
- C4: No existing file is overwritten (byte-identical after a skipped write).
- C5: Production code uses `getHomeDirectory` and does not touch test directories.
- C6: `cabal test` passes for the new writer tests.

### Affected Modules
- `src/Graphos/Infrastructure/Scaffold/Writer.hs` or equivalent
- Possibly `src/Graphos/Program.hs` if wiring is needed

### Prerequisites
- Task 2 (planner) completed.
- Task 1 (parser routing) completed.

### Risks
- Real home directory could be modified during tests if root injection fails. Mitigation: tests use a passed-in temporary path; production code path is separate.
