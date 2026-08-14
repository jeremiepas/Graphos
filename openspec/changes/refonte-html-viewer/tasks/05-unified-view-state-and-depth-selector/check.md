# Check — 5.C Unified view state and depth selector

## Verification Plan
- [ ] Verify the depth selector offers four levels and defaults to `Overview`.
- [ ] Verify depth switching destroys the old renderer and leaves one canvas.
- [ ] Compare `Custom` N=2 node set with `graphos neighbors <id> --depth 2`.
- [ ] Verify N is clamped to 1–6 and a warning appears for >2,000 nodes.
- [ ] Verify state survives reload and stale references fall back to `Overview`.
- [ ] Verify no `btnBack` element or listener remains.
- [ ] Run `cabal test` for export regressions.

## Results

| Criterion | Status | Notes |
|---|---|---|
| Depth selector UI | | |
| Default `Overview` | | |
| Renderer lifecycle | | |
| Custom N-hop accuracy | | |
| `sessionStorage` persistence | | |
| `btnBack` removed | | |
| Regression tests | | |

## Verdict
*Pending implementation*
