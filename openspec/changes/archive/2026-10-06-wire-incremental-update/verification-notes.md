# Verification Notes — wire-incremental-update

## Task 6.1 — live runs on this repository (2026-10-06)

Local Ollama (nix store `ollama-0.22.1`, models in `~/.ollama`) serving
`nomic-embed-text` at `http://localhost:11434/v1`.

| Run | Command legs | Result |
|-----|--------------|--------|
| 1 | `graphos src --embed --no-viz --output /tmp/opencode/upd-verify` | `[cache] reused 0 file extraction(s), re-extracted 149`; embedding pass 120 s (4405 vectors, all API) |
| 2 | same command again (warm) | `[cache] reused 149 file extraction(s), re-extracted 0`; embedding pass 9 s (all disk hits from `cache/embeddings/`) |
| 3 | `graphos src --fresh --embed --no-viz --output /tmp/opencode/upd-verify` | no `[cache]` line (consult bypassed; re-extracted + re-embedded everything) |

## Task 6.2 — confluence

| Run | Output |
|-----|--------|
| fresh | `graphos src --fresh --no-viz --output /tmp/opencode/upd-fresh` → 4405 nodes |
| warm | `/tmp/opencode/upd-verify` (run 2) |

Node **id sets equal**: `True`. Edge **(source,target) pairs equal**: `True`.
Matches the Hspec `UpdateConfluenceSpec` (fixture-tree confluence).

## Environment notes

- Embedding projection logging comes from fix-oom-memory-budget-guard (budget
  derived 5.8 GiB, RTS re-exec with `-M`) — orthogonal, working.
- The eviction sweep logs `[cache] evicted N cache entries` only when it
  evicts something; zero evictions are silent (`evictToCap` semantics).