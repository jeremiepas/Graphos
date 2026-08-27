# Check — 2.C Interned, style-free view model

## Verification Plan
- [ ] Run property test for interning round-trip.
- [ ] Run key-set test on emitted payload.
- [ ] Verify `source_file` uniqueness in string tables.
- [ ] Verify byte-identical payload sections across two exports.
- [ ] Measure reference corpus against size budget.
- [ ] Run `cabal build --flag dev` and `cabal test`.

## Results

| Criterion | Status | Notes |
|---|---|---|
| Property test | PASS | `cabal test` HTMLSpec round-trip: passed 100 tests |
| Key-set test | PASS | `grep` for forbidden keys returned empty |
| `source_file` uniqueness | PASS | Mapping from `source_file` to index is unique |
| Determinism | PASS | Two exports of Graphos self-graph produced identical payload sections |
| Size budget | PASS | Reference corpus: 24.5 MB total, 185 B/node, 22 B/edge |
| Compilation | PASS | `cabal build --flag dev` and `cabal test` green |

## Verdict
PASS — All criteria met.
