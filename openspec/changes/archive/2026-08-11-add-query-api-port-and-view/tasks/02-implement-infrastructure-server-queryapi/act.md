<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement Infrastructure.Server.QueryAPI — ACT

**Task slug**: `02-implement-infrastructure-server-queryapi`
**Attempt**: 1
**Status**: complete

## Summary

Task 2 is complete. All plan criteria verified, implementation committed (not yet pushed — waiting for full change completion).

## What was committed

The following files were changed as part of this task:

| File | Action |
| --- | --- |
| `src/Graphos/Infrastructure/Server/QueryAPI.hs` | Created — HTTP API app with 5 endpoints |
| `tests/Graphos/Infrastructure/Server/QueryAPISpec.hs` | Created — parity + CORS + method tests |
| `src/Graphos/CLI/Parser.hs` | Modified — added `Show, Eq` to `Command`; updated `renderCommandReference` |
| `tests/Graphos/CLI/ParserSpec.hs` | Modified — added serve flag parser tests |
| `tests/fixtures/scaffold/graphos-global-skill.md` | Modified — updated golden fixture |
| `tests/fixtures/scaffold/graphos-query-global-skill.md` | Modified — updated golden fixture |
| `graphos.cabal` | Modified — added test module + dependencies |

## Test results

```
cabal test --flag dev --test-show-details=streaming
  363 examples, 0 failures
  Execution time/hours:   0.00s in 0.00s
```

## Next

Proceed to Task 3: extend `Static.hs` to compose static file serving with the query API (single Warp server serving both static HTML and `/api/*` routes).
