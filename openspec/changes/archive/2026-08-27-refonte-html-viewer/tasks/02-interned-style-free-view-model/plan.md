# Plan — 2.P Interned, style-free view model

## Scope
Refactor `Infrastructure/Export/HTML.hs:806–983` to a projected view-model.
Build string tables for node ids, `source_file`, `kind`, and `relation`.
Emit edges as `[srcIdx, tgtIdx, relIdx]` and drop redundant style fields.
Update the embedded viewer JS to read the new shape so the file keeps working.

## Check Criteria
- [ ] Property test: expanding the interned payload to `(id, label, source_file, kind, relation)`
  tuples equals the in-memory graph tuples for all nodes and edges.
- [ ] Key-set test: no node record contains `color`/`group`/`title`; no edge record
  contains `color`/`arrows`/`dashes`/`width`/`title`/`label`; no signature text in payload.
- [ ] Every distinct `source_file` appears exactly once in the string tables.
- [ ] Determinism: two exports of the same graph produce byte-identical payload sections.
- [ ] Reference corpus: ≤ 200 B/node, ≤ 24 B/edge, total ≤ 30 MB.
- [ ] `cabal build --flag dev` and `cabal test` green with `-Werror`.

## Affected Modules
- `src/Graphos/Infrastructure/Export/HTML.hs`
- `tests/Graphos/Infrastructure/Export/HTMLSpec.hs`

## Risks
- Interning bug corrupts the visual representation (wrong string mapping).
- Payload shape change breaks the viewer if the JS update is incomplete.
