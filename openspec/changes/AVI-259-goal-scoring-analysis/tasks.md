# Child Issues for AVI-259

## P0 Features (Build First)

| # | Task | Score | Status | Dependencies |
|---|------|-------|--------|--------------|
| 01 | atomic-graph-output-writes | 13.5 | ready | none |
| 02 | honor-graphosignore | 13.44 | ready | none |
| 03 | fix-query-relevance-scoring | 11.52 | ready | none |
| 04 | fix-runtime-ram-crash | 10.12 | ready | 01, 02 |
| 05 | checkpoint-and-cluster-only-controls | 9.14 | ready | 01 |
| 06 | enforce-query-token-budget | 8.58 | ready | 03 |

## P1 Features (Build Second)

| # | Task | Score | Status | Dependencies |
|---|------|-------|--------|--------------|
| 07 | mcp-request-resilience | 6.86 | planned | 03 |
| 08 | fix-agent-skill-graphos-cli-tools | 6.86 | planned | 03 |
| 09 | detect-generated-vendored-code | 6.86 | planned | 02, 04 |
| 10 | deterministic-doc-code-edges | 6.22 | planned | 04, 03 |
| 11 | jgf-graph-serialization | 7.84 | planned | 01 |
| 12 | openspec-view | 7.61 | planned | 03, 01 |

## P2 Features (Build Third)

| # | Task | Score | Status | Dependencies |
|---|------|-------|--------|--------------|
| 13 | cluster-composition | 5.71 | planned | 04, 05 |
| 14 | explorer-queries | 5.6 | planned | 03, 12 |

## P3 Features (Build Last)

| # | Task | Score | Status | Dependencies |
|---|------|-------|--------|--------------|
| 15 | add-product-owner-agent | 6.0 | planned | 08 |
| 16 | research-view | 3.93 | planned | 14, 11 |
| 17 | json-graph-web-view | 3.75 | planned | 11, 16 |
| 18 | cypher-eval-graphindex | 3.21 | deferred | none |
| 19 | extract-haskell-libs | 3.75 | deferred | none |
