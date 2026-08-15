## 1. Node identity

- [ ] 1.1 Define short stable `NodeId` derivation (`path#line` + hash on collision)
- [ ] 1.2 Add `preview` field to node record; move full snippet out of query responses
- [ ] 1.3 Bump graph.json schema version and update FromJSON/ToJSON
- [ ] 1.4 Tests: ID stability across re-extraction, collision handling

## 2. Compact serialization

- [ ] 2.1 Implement label truncation to `maxLabelChars` in Domain/Context
- [ ] 2.2 Emit only `id`, `source_file`, `score`, `kind`, truncated `label` in list responses
- [ ] 2.3 Tests: truncation boundary, field presence

## 3. Budget enforcement

- [ ] 3.1 Rank-then-serialize with running byte counter; stop at budget
- [ ] 3.2 Populate `omitted` counts for dropped nodes/edges
- [ ] 3.3 Tests: response never exceeds budget; highest scores retained

## 4. Controls & MCP

- [ ] 4.1 Add `--max-nodes` and `--max-label-chars` CLI flags
- [ ] 4.2 Expose same params on MCP query tools
- [ ] 4.3 MCP smoke test: `query_graph` output stays under budget

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test` green
- [ ] 5.3 Re-run prior failing query; confirm compact bounded output
