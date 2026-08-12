<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 7 — 78K-node benchmark on solario — DO

**Task slug**: `07-78k-node-benchmark`
**Attempt**: 1
**Status**: pending

## Summary

Run the full pipeline on the solario codebase, verify all new fields are populated, and measure browser performance via `graphos serve`. This is the final Check gate for the entire change.

## Detail

### Concrete Changes

**None — validation only.** This task executes the pipeline on the solario dataset and measures results.

### Execution Steps

1. Regenerate the solario graph:
   ```bash
   cabal run graphos -- <solario-path>
   ```
   If regeneration is too slow (> 5 min), use the existing `../../solario/graphos-out/graph.json` and run a minimal re-export through the new pipeline.

2. Serve the output:
   ```bash
   graphos serve --dir graphos-out --port 8080
   ```

3. Open `http://localhost:8080/graph.html` in a browser (Chrome/Edge with DevTools).

4. Run automated checks:
   ```bash
   # Verify community_id population
   python3 -c "
   import json
   g = json.load(open('graphos-out/graph.json'))
   nodes = g['nodes']
   print(f'Total nodes: {len(nodes)}')
   non_null = sum(1 for n in nodes if n.get(\"community_id\") is not None)
   print(f'Non-null community_id: {non_null}')
   print(f'All populated: {non_null == len(nodes)}')
   agg = g.get('community_aggregates', [])
   print(f'Community aggregates: {len(agg)}')
   "
   ```

   ```bash
   # Verify SQLite
   sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM nodes;"
   sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM edges;"
   sqlite3 graphos-out/graph.sqlite "SELECT COUNT(*) FROM community_edge_pairs;"
   ```

   ```bash
   # Verify COOP/COEP headers
   curl -I http://localhost:8080/graph.html
   ```

5. Measure browser performance (DevTools Performance tab):
   - Initial overview load time
   - Drill-down latency (click a community → measure until interactive)
   - Pan/zoom frame rate
   - Tab memory usage (DevTools Memory)

### Key Decisions

- **Automated + manual verification**: JSON/SQLite/headers checks are automated via CLI. Browser performance requires manual measurement with DevTools.
- **Baseline comparison**: Compare node/edge/community counts against the baseline `../../solario/graphos-out/graph.json` to ensure no regression.
- **Accept the wait**: If regeneration takes > 5 min, accept the wait — this is the real-world validation path. Don't shortcut with a partial re-export.

### Dependencies

- Requires: Tasks 1-6 completed and passing
- Reads: `tasks/07-78k-node-benchmark/plan.md`
- Unlocks: `tasks/07-78k-node-benchmark/check.md`
