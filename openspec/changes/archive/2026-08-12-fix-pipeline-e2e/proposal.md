## Why

End-to-end pipeline validation (task 38 of `graphos-product`) revealed three critical bugs blocking production readiness:

1. **LSP extraction produces nodes but almost no edges** — Running `graphos .` extracts 8105 nodes but only 1 edge. The LSP client connects and reads document symbols (nodes) but fails to extract references and call hierarchy edges. Without edges, community detection produces a single community, god-node analysis is meaningless, and the entire knowledge graph is flat.

2. **Pipeline crash: "thread blocked indefinitely in an MVar operation"** — After extraction, the pipeline crashes with an MVar deadlock during the observability/metrics thread cleanup. This prevents clean completion and leaves stale checkpoint files.

3. **Test EdgeId collision** (already fixed) — Test helpers used `EdgeId ""` for all edges, causing `Map.fromList` deduplication that silently dropped edges. This was fixed in the current session but is documented here for traceability.

These issues make the pipeline unusable for its primary purpose: producing a meaningful knowledge graph from a real codebase.

## What Changes

- Fix LSP extraction to correctly extract references and call hierarchy edges, producing a graph with edges proportional to nodes (target: ≥1 edge per 3 nodes minimum)
- Fix MVar deadlock in pipeline shutdown (observability thread cleanup)
- Ensure pipeline completes end-to-end without crashes
- Validate: `cabal run graphos -- .` produces graph.json with ≥100 nodes, ≥5 communities, and proportional edges

## Capabilities

### Modified Capabilities
- `lsp-extraction`: Fix reference and call hierarchy edge extraction — currently produces near-zero edges
- `full-pipeline`: Fix MVar deadlock on shutdown; validate end-to-end completion

## Impact

- **Infrastructure.LSP.Client**: Extraction lifecycle — reference and callHierarchy requests
- **Infrastructure.LSP.Extraction**: Edge conversion from LSP responses
- **Infrastructure.LSP.Protocol**: Request/response handling for references and callHierarchy
- **UseCase.Pipeline**: Shutdown sequence and observability thread management
- **UseCase.Extract**: Parallel extraction orchestration and error handling
- **Tests**: Existing LSP extraction tests need edge-coverage assertions

## PDCA Cycle

- **Plan**: Pipeline produces ≥100 nodes, ≥30 edges, ≥5 communities when run on the Graphos repo itself. `cabal run graphos -- .` completes in <5 min extraction, <30s Leiden. No crashes. All 90+ tests pass.
- **Do**: Fix LSP edge extraction (references + call hierarchy), fix MVar deadlock, add edge-quality assertions to extraction tests.
- **Check**: Run `graphos .` on this repo. Verify: graph.json has proportional edges, ≥5 communities, HTML renders, no crash. `cabal test` green.
- **Act**: Document baseline extraction quality (nodes:edges ratio, community count). Record deviations as next PDCA iteration items per PRD §18.