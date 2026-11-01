## 1. Filter self-edges in buildGraph

- [ ] 1.1 Add `edgeSource e /= edgeTarget e` to `buildGraph`'s `validEdges` filter in `src/Graphos/Domain/Graph/Core.hs`
- [ ] 1.2 Update the `buildGraph` doc-comment to state that self-edges are dropped to keep `G_s` irreflexive (Phase 1 `SupportGraph`, `adj v v == False`; Lean seam `NoSelfLoops`)

## 2. Regression tests

- [ ] 2.1 Lone self-edge: node preserved, `degree 0`, not its own neighbour (undirected `buildGraph False`)
- [ ] 2.2 Self-edge coexisting with a real edge: the real edge is kept, the self-edge is dropped
- [ ] 2.3 Self-edge dropped in directed mode (`buildGraph True`)

## 3. Verify

- [ ] 3.1 `cabal build --flag dev` with `-Werror` is clean
- [ ] 3.2 `cabal test` is green
- [ ] 3.3 `openspec validate` passes the delta spec
