# Task 5 — End-to-end pipeline validation — PLAN

**Task slug**: `05-end-to-end-pipeline-validation`
**Attempt**: 1
**Status**: pending

## Summary

Run the full pipeline `cabal run graphos -- .` on the Graphos repository itself and validate that all pipeline stages complete successfully with production-quality output: ≥100 nodes, ≥30 edges, ≥5 communities, proportional edge types (Calls/References/Imports present), and clean shutdown with exit code 0.

## Detail

### Scope

- **No code changes** — this is a validation task that runs the pipeline and measures output quality
- **Verification point**: After Tasks 1-4 are implemented, run `cabal run graphos -- .` on the Graphos repo (`.`)
- **Output directory**: `graphos-out/` — validate `graph.json`, `graph.html`, `GRAPH_REPORT.md`, `community_graph.json`

### Check Criteria

**What tests/gates will be run:**
- `cabal build` — zero warnings (with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`)
- `cabal test` — all tests pass (90/90)
- `cabal run graphos -- .` — pipeline completes, exit code 0
- Post-run validation: parse `graphos-out/graph.json` for node/edge/community counts
- Post-run validation: `grep -r 'EdgeId ""' src/` — zero matches
- Post-run validation: check edge types contain `Calls`, `References`, `Imports`

**What spec scenarios this task must satisfy:**
- `pipeline-shutdown` spec — **"Pipeline completes without MVar deadlock"**: `cabal run graphos -- .` exits cleanly with exit code 0.
- `design.md` verification strategy items:
  - graph.json has proportional edges: nodes:edges ratio ≤ 10:1
  - ≥5 communities detected
  - ≥100 nodes
  - HTML renders (graphos-out/graph.html exists and is valid HTML)

**What the exact PASS conditions are:**
1. `cabal build` — exit 0, zero warnings
2. `cabal test` — exit 0, 90/90 pass
3. `cabal run graphos -- .` — exit 0, no "thread blocked indefinitely" in output
4. `graphos-out/graph.json` exists, parses as valid JSON with ≥100 nodes
5. `graphos-out/graph.json` has ≥30 edges (goal: ≥1000 edges based on production baseline of 67289)
6. `graphos-out/graph.json` has ≥5 communities (goal: ≥10 based on baseline of 61)
7. Edge relation types include `references`, `imports`, `depends_on`, `calls` (not just `contains`)
8. `graphos-out/graph.html` exists and is valid HTML (≥1KB)
9. `graphos-out/GRAPH_REPORT.md` exists
10. No `EdgeId ""` in `src/`
11. **Performance gates**: extraction < 5 minutes, Leiden clustering < 30 seconds

**What would constitute a FAIL:**
- `cabal run graphos -- .` crashes with MVar deadlock (Task 4 not fixed)
- `graph.json` has <30 edges despite all prior fixes (references/call hierarchy not working — Tasks 2/3 not effective)
- `graph.json` has edges but all are `Contains` type (references and call hierarchy produce no edges — capability checks failing)
- `cabal test` count drops below 90 (regression from test updates)
- `graph.json` has ≥100 nodes but communities < 5 (community detection not receiving edges properly)
- Extraction takes >5 minutes (references/call hierarchy requests too many or too slow)
- `cabal build` produces warnings (code quality regression)

### Affected Modules

| Module | Layer |
|--------|-------|
| N/A (validation only) | — |
| `graphos-out/graph.json` | Output — node count, edge count, edge types, community count |
| `graphos-out/graph.html` | Output — HTML visualization exists |
| `graphos-out/GRAPH_REPORT.md` | Output — report generated |

### Prerequisites

- Tasks 1-4 completed and passing their individual check criteria
- `cabal build` clean on the working tree
- `nix-shell shell.nix` available for build environment
- Sufficient disk space for pipeline output (~100MB for large repos)

### Risks

| Risk | Mitigation |
|------|------------|
| Graphos repo is small (8105 nodes baseline) — may not exercise all edge types | Validate edge types present even if counts low; baseline from full run is 67289 edges |
| LSP server (haskell-language-server) may not return references/calls | Capability check governs behavior; low edge count acceptable if capabilities missing |
| Extraction time may exceed 5 minutes on large repos | Timeout cap in design (top-10 symbols, 5s per request) should bound worst case |
| Community detection may fail on edgeless graph | If edges = 0, Leiden returns single community — indicates Task 2/3 failure, not Task 5 failure |

## Result

Pending — first cycle.
