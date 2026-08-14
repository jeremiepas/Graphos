# Plan — 4.P Extract viewer assets and vendor the renderer

## Scope
Move viewer CSS/JS from Haskell string literals to real asset files, vendor vis-network,
and embed everything with `file-embed`. Make the HTML document fully self-contained.

## Check Criteria
- [ ] No JavaScript statements or CSS rules remain as string literals in `HTML.hs`.
- [ ] Emitted CSS/JS in `graph.html` are byte-identical to the source asset files.
- [ ] No `http://` or `https://` appears in any `src`/`href` of the emitted document.
- [ ] Document renders offline (`file://` with networking disabled) with zero network requests.
- [ ] Vendored renderer version is pinned and recorded in the emitted document.
- [ ] Exactly one renderer `options` definition exists; interaction keys live in the `interaction` section.
- [ ] Every CSS class used by the viewer JS has a matching stylesheet rule.
- [ ] `cabal build --flag dev` green; build-time delta recorded.

## Affected Modules
- `src/Graphos/Infrastructure/Export/HTML.hs`
- `graphos.cabal`
- `assets/viewer/` (new directory)

## Risks
- Embedding large assets may increase build time.
