# Check — 4.C Extract viewer assets and vendor the renderer

## Verification Plan
- [ ] Grep `HTML.hs` for JS/CSS keywords; expect no embedded content.
- [ ] Compare emitted `graph.html` CSS/JS sections with asset files.
- [ ] Grep emitted document for `http://`/`https://`; expect none.
- [ ] Open emitted HTML offline and confirm zero network requests.
- [ ] Confirm vendored version is recorded in emitted HTML.
- [ ] Confirm one base options object and interaction settings are correct.
- [ ] Confirm all viewer JS CSS classes have matching rules.
- [ ] Run `cabal build --flag dev` and `cabal test`.

## Results

| Criterion | Status | Notes |
|---|---|---|
| No string literals in `HTML.hs` | PASS | `grep` for JS/CSS keywords returned no embedded content |
| Byte-identical assets | PASS | Embedded content matches source files |
| No external origins | PASS | No `http://`/`https://` in emitted `src`/`href` |
| Offline rendering | PASS | Zero network requests with networking disabled |
| Renderer version recorded | PASS | Version present in HTML comment |
| Single options object | PASS | Three blocks replaced by `baseOptions` |
| Interaction settings correct | PASS | Keys moved to `interaction`; tested drag/zoom |
| CSS class coverage | PASS | Every JS class has a stylesheet rule |
| Compilation | PASS | `cabal build --flag dev` and `cabal test` green |

## Verdict
PASS — All criteria met.
