# Act — 4.A Extract viewer assets and vendor the renderer

## Outcome
PASS

## Summary
Viewer CSS/JS are now real asset files embedded at compile time.
vis-network is vendored, making `graph.html` fully self-contained and offline-capable.
`HTML.hs` is cleaner and the duplicated options blocks are gone.

## Learnings / Next Steps
- No material build-time delta; accept as-is.
- Proceed to task 5.
