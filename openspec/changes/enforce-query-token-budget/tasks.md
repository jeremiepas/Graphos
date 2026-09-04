## 1. Node identity

- [x] 1.1 Define short stable `NodeId` derivation (`path#line` + hash on collision)
- [x] 1.2 Add `preview` field to node record; move full snippet out of query responses
- [x] 1.3 Bump graph.json schema version and update FromJSON/ToJSON
- [x] 1.4 Tests: ID stability across re-extraction, collision handling

## 2. Compact serialization

- [x] 2.1 Implement label truncation to `maxLabelChars` in Domain/Context (`truncateLabel` in `Score.hs`; `\8230` ellipsis marker)
- [x] 2.2 Emit only `id`, `source_file`, `score`, `kind`, truncated `label` in list responses
- [x] 2.3 Tests: truncation boundary, field presence (RenderSpec `truncateLabel`/`scoredNodeBytes`)

## 3. Budget enforcement

- [x] 3.1 Rank-then-serialize with running byte counter; stop at budget (`enforceByteBudget`/`enforceResponseBudget`)
- [x] 3.2 Populate `omitted` counts for dropped nodes/edges (`qrespOmittedNodes`/`qrespOmittedEdges`; `qrespEdges` pruned to kept endpoints in `Render.hs`)
- [x] 3.3 Tests: response never exceeds budget; highest scores retained (RenderSpec `enforceByteBudget`/`enforceResponseBudget`)

## 4. Controls & MCP

- [x] 4.1 Add `--max-nodes` and `--max-label-chars` CLI flags (`commonQueryOptsP`, inserted between `label-width` and `edges`)
- [x] 4.2 Expose same params on MCP query tools (`QueryAPI.handleQuery` + MCP `query_graph`)
- [x] 4.3 MCP smoke test: `query_graph` output stays under budget (parity refs wrapped with `enforceResponseBudget` in QueryAPISpec)

## 5. Verification

- [x] 5.1 `cabal build all` clean with `-Werror` (`-flag dev`) — library + `exe:graphos` compile/link, no warnings
- [x] 5.2 `cabal test` green — **709 examples, 0 failures, 2 pending** (via `dist-newstyle/build/x86_64-linux/ghc-9.10.3/graphos-0.1.0.0/t/graphos-test/build/graphos-test/graphos-test`)
- [x] 5.3 Re-run prior failing query; confirm compact bounded output
