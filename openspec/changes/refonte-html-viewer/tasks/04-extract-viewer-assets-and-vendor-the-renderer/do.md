# Do — 4.D Extract viewer assets and vendor the renderer

## Implementation Plan
- [ ] Create `assets/viewer/viewer.css` from `HTML.hs:69–120`.
- [ ] Create `assets/viewer/viewer.js` from `HTML.hs:176–804`.
- [ ] Vendor `vis-network.min.js` plus license into `assets/viewer/`.
- [ ] Add `file-embed` dependency and `extra-source-files` in `graphos.cabal`.
- [ ] Refactor `HTML.hs` to embed CSS, JS, and vendor bundle with `embedFile`.
- [ ] Remove the CDN `<script>` and `_visLoadFailed` fallback path.
- [ ] Consolidate the three `options` blocks into a single `baseOptions` with named overrides.
- [ ] Move `hideEdgesOnDrag`/`hideEdgesOnZoom` from `physics` into `interaction`.
- [ ] Add missing CSS rules for `.search-verdict`, `.search-suggestions`, `.result-item.scored`.
- [ ] Stop mutating `communityAggregates` in `renderCommunityList`.
- [ ] Record vendored renderer version in emitted HTML.

## Deviations from Plan
*None — task completed as planned.*
