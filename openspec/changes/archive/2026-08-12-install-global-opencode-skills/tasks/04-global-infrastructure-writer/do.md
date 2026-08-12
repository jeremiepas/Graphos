# Task 4 — Add Infrastructure writer for global install with idempotency — DO

**Task slug**: `04-global-infrastructure-writer`
**Attempt**: 1
**Status**: in-progress

## Summary

Implement the `runInstallSkill` handler in Infrastructure: resolve the global skills root, create parent directories, write files idempotently, and report skipped files.

## Detail

### What will be implemented
- Add `runInstallSkill :: InstallSkillTarget -> IO ()` in Infrastructure that:
  1. Calls `getHomeDirectory` to obtain `~/.agents/skills/`.
  2. Runs `installSkillPlan (InstallSkillRequest target) commandReference` to get file plans.
  3. For each plan, creates parent directories.
  4. Writes the file only if it does not already exist; otherwise prints a skip message.
  5. Exits 0 unless a real filesystem error occurs.
- Expose an internal helper `runInstallSkillWithRoot :: FilePath -> InstallSkillTarget -> CommandReference -> IO ()` for tests that accepts a root directory.

### Key decisions
- Keep all IO in Infrastructure; the UseCase planner remains pure.
- Reuse existing directory-creation and idempotent-write logic from the `init` scaffolding writer to maintain the same non-destructive contract.

### Concrete changes
- `src/Graphos/Infrastructure/Scaffold/Writer.hs` (or equivalent): add `runInstallSkill` and `runInstallSkillWithRoot`.
- `src/Graphos/Program.hs`: call `runInstallSkill target` for the `InstallSkill` branch.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
