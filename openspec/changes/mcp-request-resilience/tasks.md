## 1. Domain / Graph query

- [ ] 1.1 Add bounded neighborhood expansion helper (stop at N nodes, report omitted)
- [ ] 1.2 Add bounded community-member enumeration
- [ ] 1.3 QuickCheck: expansion never exceeds cap; omitted count correct

## 2. UseCase / SelectContext

- [ ] 2.1 Honor a node budget during context selection expansion
- [ ] 2.2 Return truncation metadata from selection

## 3. Infrastructure / MCP server

- [ ] 3.1 Wrap each tool request in a configurable wall-clock timeout (STM/async)
- [ ] 3.2 On timeout, harvest and return partial results with `truncated: true`
- [ ] 3.3 Add `maxRequestNodes` and timeout params to `get_community`, `get_neighbors`, `select_context`
- [ ] 3.4 Add `truncated` and `omitted` fields to affected tool responses

## 4. Config & CLI

- [ ] 4.1 Add config `mcp.maxRequestNodes` and `mcp.requestTimeout`
- [ ] 4.2 Document defaults in tool descriptions

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test` green
- [ ] 5.3 MCP smoke test on a mega-community graph: `get_community`/`get_neighbors`/`select_context` return bounded results, no `-32001`
