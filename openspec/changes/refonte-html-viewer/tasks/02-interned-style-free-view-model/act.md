# Act — 2.A Interned, style-free view model

## Outcome
PASS

## Summary
The interned, style-free view model is implemented and verified.
`HTML.hs` now uses compact view-model records with string tables.
Edges are positional triples and redundant style fields are gone.
All tests pass and the reference corpus meets the size budget.

## Learnings / Next Steps
- Budget met on self-graph (135.4 B/node, 15.3 B/edge); proceed to task 3.
