## Context

`graphos-out/` is the single source of truth (`graph.json`, checkpoint, report,
etc.). Outputs are streamed/written incrementally. An interrupted labeling run
left `graph.json` truncated (`Unterminated string`), and a rebuild that deleted
outputs left the file missing, breaking the MCP server. There is no atomicity or
staging guarantee and no startup validation.

## Goals / Non-Goals

**Goals:**
- The final artifact path is always either the last good version or the complete
  new version — never partial.
- A failed rebuild never destroys a previously good graph.
- Corrupt graphs are detected early with an actionable message.

**Non-Goals:**
- Changing artifact formats or schema.
- Multi-writer coordination/locking across concurrent processes (future work).

## Decisions

- **Temp-file-plus-rename per artifact** within the same directory (rename is
  atomic on the same filesystem); `fsync` file and directory before rename.
  - *Alternative considered:* write-in-place — rejected, the observed failure mode.
- **Staging directory for full rebuilds**, swapped via directory rename on success.
  - *Alternative considered:* delete-then-write — rejected, this is exactly what
    left the graph missing mid-session.
- **Validate graph.json on load** (parse + minimal shape check) and error clearly.
  - *Alternative considered:* lazy failure at first query — rejected, produces
    confusing downstream errors.

## Risks / Trade-offs

- [Extra disk usage during staging] → transient; cleaned on success/failure.
- [Rename across filesystems not atomic] → keep temp/staging on the same
  filesystem as the target; detect and warn otherwise.
- [fsync cost] → negligible relative to extraction/clustering time.

## Migration Plan

- Additive; no format change. Existing valid graphs continue to load.
- Rollback: revert to direct writes (not recommended).
- Verify with `cabal test` (atomic-write + validation units) and an
  interrupt-during-write smoke test confirming the prior graph survives.
