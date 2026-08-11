# Task 3 — Compose static + API apps and extend serve CLI flags — ACT

**Task slug**: `03-compose-static-api-and-serve-cli`
**Attempt**: 1
**Status**: complete

## Summary

Task 3 is complete. All plan criteria verified, implementation committed (not yet pushed — waiting for full change completion).

## What was committed

The following files were changed as part of this task:

| File | Action |
| --- | --- |
| `src/Graphos/Infrastructure/Server/Static.hs` | Modified — added `serveApp`, `startServeServer`, `apiAppHandler` |
| `src/Graphos/CLI/Parser.hs` | Modified — `Command.Serve` type, `serveOpts` parser, `renderCommandReference` |
| `app/Main.hs` | Modified — Serve branch wiring |
| `tests/Graphos/CLI/ParserSpec.hs` | Modified — serve flag parsing tests |

## Test results

```
cabal test --flag dev --test-show-details=streaming
  363 examples, 0 failures
  Execution time/hours:   0.00s in 0.00s
```

## Next

Proceed to Task 4: upgrade `graph.html` navigator search to call `/api/query` with fallback to client-side substring filter.
