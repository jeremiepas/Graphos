# Do — 2.D Interned, style-free view model

## Implementation Plan
- [ ] Define new `VisNode`/`VisEdge` records with integer indices and separate string tables.
- [ ] Build string tables for `node_id`, `source_file`, `kind`, `relation`.
- [ ] Update edge emission to `[srcIdx, tgtIdx, relIdx]`.
- [ ] Remove `color`/`group`/`title` from node records and `color`/`arrows`/`dashes`/`width`/`title`/`label` from edge records.
- [ ] Strip signature text from the JSON payload.
- [ ] Update embedded viewer JS in `HTML.hs` to read the interned shape.
- [ ] Add property/key-set/uniqueness/determinism tests.

## Deviations from Plan
*None — task completed as planned.*
