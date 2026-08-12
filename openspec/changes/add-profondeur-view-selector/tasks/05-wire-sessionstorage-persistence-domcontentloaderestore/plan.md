# Task 5 — Wire sessionStorage persistence + DOMContentLoaded restore — PLAN

**Task slug**: `05-wire-sessionstorage-persistence-domcontentloaderestore`
**Attempt**: 1
**Status**: pending

## Summary

Persist `currentDepth` to `sessionStorage` key `graphos_depth` on every `switchDepth` call; persist `graphos_neighborhood_node` and `graphos_neighborhood_hops` on Custom-depth changes. On `DOMContentLoaded`, read these keys and restore the depth (default `overview` if absent or invalid). Wrap `sessionStorage` writes in `try/catch`.

## Detail

### Scope

- File: `src/Graphos/Infrastructure/Export/HTML.hs`
- Changes to `htmlBody`: (a) add `function persistDepth()` that writes `graphos_depth`, and optionally `graphos_neighborhood_node`/`graphos_neighborhood_hops` to `sessionStorage`; (b) call `persistDepth()` at the end of `switchDepth`; (c) in the `DOMContentLoaded` handler, read `graphos_depth` before `renderCommunityList()`/`initOverview`; (d) if the key is one of the four valid values, call `switchDepth` with it (passing restored `graphos_neighborhood_node`/`graphos_neighborhood_hops` for Custom); (e) if absent or invalid, default to `overview`.

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied:**
- `html-depth-selector/spec.md` — "Reload preserves selected depth": selecting Full, then reloading, loads directly into Full depth
- `html-depth-selector/spec.md` — "Reload preserves Custom neighborhood parameters": selecting Custom, focusing node X with hops 3, then reloading, restores Custom with hops 3 and renders X's N=3 neighborhood
- `html-depth-selector/spec.md` — "Absent keys default to Overview": fresh session loads into Overview depth

**PASS conditions:**
1. Selecting `Full`, then reloading the page (same tab), loads directly into `Full` depth
2. Selecting `Custom`, focusing node `X`, hops `3`, then reloading, restores `Custom` with hops `3` and re-renders `X`'s N=3 neighborhood
3. A fresh session (cleared `sessionStorage`) loads into `Overview`
4. `cabal build` exits with zero warnings
5. `cabal test` exits with code 0

**FAIL boundaries:**
- FAIL if `sessionStorage` throws in private-mode browsers (uncaught exception crashes init)
- FAIL if `graphos_depth` contains an invalid value and the page does NOT default to Overview (no fallback)
- FAIL if Custom depth parameters (node id, hops) are not restored on reload
- FAIL if `cabal build` produces any warnings
- FAIL if `cabal test` exits non-zero

### Affected modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — `htmlBody` function (embedded JavaScript, `persistDepth`, DOMContentLoaded init)

### Prerequisites

- Task 2 (multi-depth dispatcher) is complete — `switchDepth` exists as the single depth transition point
- Task 4 (Custom depth) is complete — `renderCustom(nodeId, hops)` accepts both parameters

### Risks

- **Private-mode browser**: `sessionStorage.setItem()` throws `SecurityError` in Safari private mode. Mitigation: wrap all `sessionStorage` writes in `try/catch` — the function should silently fail and default to Overview on read.
- **Stale node references**: A `graphos_neighborhood_node` key may point to a node no longer in `allNodes` (e.g., data changed between sessions, or the node was removed). Mitigation: check that the restored node id exists in `nodeAdj` before calling `renderCustom`; fall back to Overview if not.
- **Tab isolation**: `sessionStorage` is per-tab, so switching tabs and reloading the other tab will not restore state. This is by design (per-session, not per-user). No mitigation needed — this is the intended behavior per design Decision 3.
- **Key collision**: The keys `graphos_depth`, `graphos_neighborhood_node`, `graphos_neighborhood_hops` are prefixed with `graphos_` to avoid collisions with other scripts. No other Graphos component uses `sessionStorage`.

## Result

<!-- Pending implementation -->
