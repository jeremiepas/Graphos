# Task 4 — Upgrade graph.html navigator search to call /api/query with fallback — DO

**Task slug**: `04-upgrade-html-navigator-search-to-api`
**Attempt**: 1
**Status**: in-progress → completed

## Summary

Rewrote `showSearchResults` in `Infrastructure.Export.HTML` to first attempt a fetch to `/api/query`, then render verdict, suggestions, and ranked scored nodes. On fetch failure, falls back to the existing client-side substring filter. Added subgraph highlighting on the vis-network canvas.

## Detail

### What was implemented

1. **`src/Graphos/Infrastructure/Export/HTML.hs`**
   - Added `apiAvailable` flag (initialized to `true`) that tracks whether the API is reachable.
   - Rewrote `showSearchResults` to:
     - When `q.length >= 2` and `apiAvailable`, attempt `fetch('/api/query?q=' + encodeURIComponent(q) + '&mode=bfs')`.
     - On success: parse JSON, render verdict header with score and hash, render "Did you mean?" suggestions, render ranked scored nodes (score descending) as clickable items calling `focusNode(nid)`.
     - On failure (network error, non-200, JSON parse error): set `apiAvailable=false`, fall back to existing substring filter.
   - Added `highlightSubgraph(nodeIds)` function:
     - Highlights matched nodes (bright color, larger size, bold border).
     - Dims unmatched nodes (opacity 0.2).
     - Emphasizes matched edges (brighter color, thicker).
     - Tracks original colors to restore on reset.
   - Added "Reset" button handler that clears highlighting and restores original colors.
   - Added `currentHighlightedNodes` tracking to prevent duplicate highlights.

### Key decisions

- `apiAvailable` flag is set to `true` initially, then `false` on any fetch failure. Once `false`, always uses substring fallback.
- Subgraph highlighting uses the currently active dataset (`drilldownNodesDataset` if in drilldown phase, `overviewNodesDataset` if in overview).
- Matched nodes are highlighted with the community color + bright border; unmatched nodes get opacity 0.2.
- Reset button restores original node colors and removes edge emphasis.
- Results limited to top 20 from API (vs 50 for substring fallback).

### Concrete changed files

- `src/Graphos/Infrastructure/Export/HTML.hs` — rewrote `showSearchResults`, added `highlightSubgraph`, `resetHighlight`, `apiAvailable` flag

## Result

Implementation complete. Ready for `check.md`.
