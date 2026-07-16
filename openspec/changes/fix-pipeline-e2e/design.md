## Context

Running `cabal run graphos -- .` on the Graphos repo produces 8105 nodes but only 1 edge. The pipeline completes detect and extract stages but produces nearly edge-less graphs. Additionally, the pipeline crashes with "thread blocked indefinitely in an MVar operation" during shutdown. The root causes are:

1. **LSP extraction only produces Contains edges from symbol hierarchy** — no Calls/References/Imports edges are extracted because `extractViaLSP` never calls `lspReferences` or `lspCallHierarchyIncoming`.
2. **`extractCallHierarchy` is a stub** that returns `[]`.
3. **`EdgeId ""` in production code** — `symbolTreeToEdges` creates edges with empty EdgeId, causing Map deduplication when multiple edges share the same source-target pair.
4. **MVar deadlock** — observability thread cleanup blocks on shutdown.

## Goals / Non-Goals

**Goals:**
- LSP extraction produces reference edges (Calls/References/Imports) in addition to Contains edges
- Call hierarchy extraction is fully implemented (not stub)
- EdgeId is unique per edge — no deduplication from empty EdgeId
- Pipeline completes end-to-end without MVar crash
- `cabal run graphos -- .` produces graph.json with ≥30 edges on this repo

**Non-Goals:**
- Improving LSP server compatibility (only haskell-language-server is tested)
- Adding tree-sitter edge extraction (separate change)
- Neo4j/Memgraph push validation (task 40)
- Context selection validation (task 39)

## Decisions

### D1: Implement references extraction via `textDocument/references`

**Decision**: After extracting document symbols for each file, send `textDocument/references` requests for the top-level symbols to discover cross-file relationships.

**Alternatives considered:**
- A) Only use `callHierarchy/incomingCalls` — fewer requests but not all servers support it
- B) Only use `references` — broader server support but more requests
- C) **Both references and call hierarchy** — maximum edge coverage; use references as baseline, call hierarchy for deeper call graphs

**Rationale**: References is the most universally supported LSP method for finding cross-file connections. Call hierarchy is a nice addition but has limited server support. Implementing references first gives us the biggest edge-coverage improvement.

### D2: Generate unique EdgeId from source + target + relation

**Decision**: Replace `EdgeId ""` with `EdgeId (source <> "->" <> target <> ":" <> relationToText relation)` in all edge-creation code.

**Alternatives considered:**
- A) Sequential counter — simple but not deterministic across runs
- B) SHA256 hash of source+target — overkill for an ID
- C) **Composite key from domain fields** — deterministic, unique, human-readable

**Rationale**: The EdgeId must uniquely identify each edge. Using source+target+relation is deterministic and matches how edges are naturally identified in graph theory.

### D3: Fix MVar deadlock in observability shutdown

**Decision**: Use `race` or `timeout` when shutting down observability threads. Replace bare `takeMVar` with `timeout`-wrapped versions in the server cleanup path.

**Alternatives considered:**
- A) Use `async` with `cancel` — more complex but cleaner shutdown
- B) **Wrap MVar waits in `timeout`** — minimal change, prevents indefinite blocking
- C) Use STM `TVar` instead of MVar — larger refactor

**Rationale**: Option B is the minimal fix that prevents the deadlock while preserving the existing MVar-based design. The 5-second timeout ensures cleanup completes or times out gracefully.

### D4: Cap reference requests per file

**Decision**: Limit references extraction to top-10 symbols per file to keep extraction time reasonable. Large files can have hundreds of symbols; querying all would be too slow.

**Alternatives considered:**
- A) No limit — thorough but slow (could take hours on large repos)
- B) Top-5 symbols — fast but may miss important connections
- C) **Top-10 symbols sorted by symbol kind priority** (functions/classes first, then variables) — balances coverage and speed

**Rationale**: Top-10 with kind priority gives reasonable coverage while keeping extraction time under 5 minutes for repos under 1000 files.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| LSP servers may not support `references` | Check `scpReferencesProvider` capability before requesting; fall back gracefully |
| Reference extraction slow on large repos | Top-10 symbols per file limit; 5s timeout per request |
| Call hierarchy not supported by all servers | Check capability; skip if not available |
| Unique EdgeId collision | Source+target+relation is unique by definition (same edge = same triple) |
| MVar timeout too aggressive | 5-second timeout is generous for cleanup; if exceeded, log warning and continue |

## Verification Strategy (Check)

1. `cabal build` — zero warnings
2. `cabal test` — all tests pass (including updated EdgeId tests)
3. `cabal run graphos -- .` — pipeline completes without crash
4. Verify graph.json has proportional edges: nodes:edges ratio ≤ 10:1
5. Verify ≥5 communities detected
6. Verify no `EdgeId ""` in production edge creation (grep check)
7. Verify reference edges have Calls/References/Imports relation types (not just Contains)

## Iteration & Rollback (Act)

- If reference extraction causes timeouts: reduce per-file limit from 10 to 5 symbols
- If MVar deadlock persists: escalate to STM-based shutdown
- If edge quality is still low: add call hierarchy extraction as follow-up
- Document extraction quality baseline (nodes:edges ratio) in task 38 Act step
- Feed findings into next PDCA iteration per PRD §18