# Task 4 — Upgrade graph.html navigator search to call /api/query with fallback — PLAN

**Task slug**: `04-upgrade-html-navigator-search-to-api`
**Attempt**: 1
**Status**: pending

## Summary

Rewrite `showSearchResults` in `Infrastructure.Export.HTML` to:
- (a) attempt `fetch('/api/query?q=' + encodeURIComponent(q) + '&mode=bfs')` (debounced 200ms) when `q.length >= 2`;
- (b) on success render a header with `verdict (best score: X) [hash: H]`, a "Did you mean: ...?" line when `suggestions` is non-empty, and ranked scored nodes (score-desc) as clickable result items calling `focusNode(nid)`;
- (c) highlight the matched subgraph on the vis-network canvas (matched node ids highlighted, others dimmed, matched edges emphasized) and a "Reset" that restores colors;
- (d) on fetch failure set `apiAvailable=false` and fall back to the existing client-side substring filter over `allNodes`.

Keep the self-contained HTML guarantee (no external JS).

## Detail

### Scope of this task

- **`src/Graphos/Infrastructure/Export/HTML.hs`**
  - Modify `showSearchResults` JS function:
    - When `q.length >= 2`, attempt `fetch('/api/query?q=' + encodeURIComponent(q) + '&mode=bfs')`.
    - On success: parse JSON, render verdict header (score, hash), suggestions ("Did you mean?"), ranked nodes (score descending) as clickable items calling `focusNode(nid)`.
    - On failure (network error, non-200, JSON parse error): set `apiAvailable=false`, fall back to existing substring filter.
  - Add subgraph highlighting:
    - Track `currentNodesDataset` / `currentEdgesDataset` (already tracked as `overviewNodesDataset`/`overviewEdgesDataset` or `drilldownNodesDataset`/`drilldownEdgesDataset`).
    - On search results, highlight matched node ids (bright color, larger size), dim others (opacity 0.2), emphasize matched edges.
    - "Reset" button restores original colors.
  - Add `apiAvailable` flag: set to `true` on first successful fetch, `false` on failure. When `false`, always use substring fallback.

### Check Criteria (defined before code exists)

1. **Module compiles**
   - Command: `cabal build --flag dev`
   - PASS: build succeeds with no warnings.
   - FAIL: compile error or `-Werror` warning.

2. **HTML contains fetch + fallback**
   - Generated `graph.html` contains the `/api/query` fetch call and the fallback branch.
   - PASS: assert via string search in the HTML-generation Hspec test (search for `fetch('/api/query` and `apiAvailable`).

3. **Tests pass**
   - Command: `cabal test --flag dev`
   - PASS: all tests pass including the new HTML generation assertions.

4. **Manual: API search**
   - `graphos .` then `graphos serve` then open `graph.html`, type a 2+ char query.
   - PASS: verdict + scored nodes + highlighted subgraph appear; clicking a result focuses the node; Reset restores.

5. **Manual: file:// fallback**
   - Open `graph.html` via `file://`.
   - PASS: substring fallback works (no fetch errors, results shown via client-side filter).

### Affected modules / files

- Modify: `src/Graphos/Infrastructure/Export/HTML.hs`
- Modify: `tests/` (HTML generation Hspec)

### Prerequisites

- Task 2 complete: `/api/query` endpoint exists and returns proper JSON.
- `graph.html` is served by `graphos serve` (Task 3).

### Risks

| Risk | Mitigation |
| --- | --- |
| vis-network dataset references differ between overview/drilldown phases | Reuse the currently active dataset (track a `currentNodesDataset`/`currentEdgesDataset` like the existing selection code). |
| CORS headers on `file://` | Fallback path doesn't need CORS; only API path needs it (handled by Task 2). |
| Large result sets overwhelm UI | Limit to top 20 results from API; existing substring filter limits to 50. |
| `fetch` from `file://` fails silently | Wrap in try/catch, set `apiAvailable=false`, fall back to substring. |

## Result

Pending — plan complete. Next step is `do.md`.
