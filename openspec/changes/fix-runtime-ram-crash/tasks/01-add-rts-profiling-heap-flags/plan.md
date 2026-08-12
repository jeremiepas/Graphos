<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

# Task 1 — Add RTS profiling and heap limit CLI flags — PLAN

**Task slug**: `01-add-rts-profiling-heap-flags`
**Attempt**: 1
**Status**: pending

## Summary

Add first-class CLI flags `--rts-profile` and `--max-heap SIZE` to `app/Main.hs` that enable GHC RTS profiling and heap size limiting, making memory debugging discoverable without requiring users to know `+RTS` syntax.

## Detail

### Scope

This task modifies `app/Main.hs` to parse two new CLI flags:
- `--rts-profile` — causes the executable to re-exec itself with `+RTS -s -hT`, producing GC statistics on stderr and a heap profile file
- `--max-heap SIZE` — re-execs with `+RTS -M <size>`, causing a clear error exit when heap limit is exceeded

The implementation uses the CLI re-exec pattern described in Design Decision D7: GHC freezes RTS options at process start, so the executable must `getExecutablePath` + `execFile` with `+RTS` flags appended after `--`.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `pipeline/scen:rtss-profiling-gc-stats` | `specs/pipeline/spec.md` | `graphos . --rts-profile` prints GC stats to stderr and generates `graphos.hp` |
| `pipeline/scen:max-heap-limits-memory` | `specs/pipeline/spec.md` | `graphos . --max-heap 4G` exits with clear error when heap exceeded |
| `pipeline/scen:flags-combined` | `specs/pipeline/spec.md` | `--rts-profile` and `--max-heap 8G` work together |

**Specific tests/gates:**

1. **Unit test**: Add a test module `tests/MainSpec.hs` or inline tests that verify `--help` output contains both `--rts-profile` and `--max-heap SIZE`.
2. **Integration test**: Run `graphos . --rts-profile` on a small test directory — verify GC stats appear on stderr and `.hp` file is generated.
3. **Integration test**: Run `graphos . --max-heap 1G` on a codebase that exceeds 1GB — verify graceful error exit with message suggesting increasing `--max-heap`.
4. **Integration test**: Run `graphos . --rts-profile --max-heap 4G` — verify both flags function together.
5. **Build gate**: `cabal test` passes.

**PASS conditions:**
- `--help` mentions both flags with descriptions
- `graphos . --rts-profile` produces stderr output containing "Total time" and "GC" (GHC runtime stats)
- `graphos . --max-heap 1G` exits with code > 0 and clear error message (not a GHC runtime crash)
- Combined flags work without error
- `cabal test` returns exit code 0

**FAIL boundaries:**
- `setRTSOpts` does not exist in GHC API — the only viable approach is re-exec. If re-exec fails (e.g., `getExecutablePath` returns unexpected path), the flag silently does nothing (graceful degradation).
- If GHC is not built with `-rtsopts`, `+RTS -s` produces no output — this is a build configuration issue, not a code issue.
- `--max-heap` error message must NOT show raw GHC runtime diagnostics; it must be a formatted message like "Heap limit exceeded: 1GB limit reached. Increase --max-heap or reduce codebase size."

### Affected Modules

- `app/Main.hs` — CLI argument parsing, re-exec logic, error handling

### Prerequisites

- Project built with `--flag dev` (enables `-Wall -Werror` for development)
- GHC binary must be built with `-rtsopts` flag (standard for GHC binaries)
- Existing `app/Main.hs` has optparse-applicative based CLI parsing

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| `getExecutablePath` may fail on edge cases | Flag silently disabled | Check return value, continue normally if path unavailable |
| `--max-heap` may cause premature OOM on legitimate workloads | False positive exits | Error message includes recommendation to increase `--max-heap` |
| Users may confuse `--max-heap` with RTS `-M` | UX confusion | Help text documents that `--max-heap 4G` is equivalent to `+RTS -M 4G` |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK → "FAIL — see attempt-2/" and start a new P→D→C→A. -->
