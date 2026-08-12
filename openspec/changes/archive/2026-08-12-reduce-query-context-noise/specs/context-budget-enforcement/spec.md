# context-budget-enforcement

Hard token-budget capping for the `select_context` and `query_graph` MCP tools (PRD
§7.2 budget table, §7.3 compact context format, §16.1 query latency budget). Ensures
the rendered payload emitted to an agent never exceeds the requested budget, that the
highest-ranked query-relevant node is always preserved, and that the reported
`token_estimate` is measured in the same units as the budget.

## ADDED Requirements

### Requirement: Hard token-budget cap on rendered context
The `select_context` and `query_graph` MCP tools SHALL truncate the rendered markdown /
JSON payload so that `token_estimate` is less than or equal to the requested budget.
Truncation MUST drop the lowest-ranked nodes and edges first (by the same relevance
rank used for ordering), MUST preserve the highest-ranked node in every case, and MUST
emit a trailing footer reporting the count of omitted nodes and edges. `token_estimate`
MUST be computed via `FormatContext.countContextTokens` (the ~1.33 × word-count
heuristic) and MUST NOT use raw character length.

#### Scenario: Over-budget payload is truncated to the cap
- **WHEN** `select_context` is called with `budget=3000` against a graph whose untruncated rendering would exceed 9000 tokens
- **THEN** the response `token_estimate` field is ≤ 3000 and the payload ends with a footer stating how many nodes and edges were omitted

#### Scenario: Top-ranked node always preserved
- **WHEN** truncation removes nodes from an over-budget payload
- **THEN** the highest-relevance-ranked node in the pre-truncation set is still present in the rendered output

#### Scenario: Budget smaller than a single node still emits that node
- **WHEN** `select_context` is called with a `budget` smaller than the token cost of the single best-matching node
- **THEN** the output contains exactly that one node and the `token_estimate` reflects its cost, with a footer noting that the budget was exceeded by the minimum-necessary amount

#### Scenario: token_estimate matches budget units
- **WHEN** the response includes a `token_estimate` field
- **THEN** the value is the result of `countContextTokens` applied to the rendered payload, not the character length of the payload

### Requirement: God/hub nodes are not unconditionally force-included
Context-selection strategies (`selectCommunityAware`, `selectRelevanceWeighted`,
`selectPathBased`) MUST NOT seed `scGodNodes` with `take 5 (analysisGodNodes analysis)`.
Hub/god nodes SHALL appear in the selected context only when they are themselves
query-relevant (pass `matchScore > 0`) or are reached by BFS within the configured depth.
The `### Hub Nodes` formatter section SHALL be omitted entirely when no hub node is in
the selected subgraph.

#### Scenario: Unrelated god node is absent from a focused query
- **WHEN** a Focused-complexity query matches nodes in the Parser community and the graph's top god node is `Main` (degree 246) in an unrelated community
- **THEN** `Main` does not appear in the rendered `### Key Nodes` section and no `### Hub Nodes` section is emitted

#### Scenario: Relevant god node is still included
- **WHEN** a query matches a node that happens to also be a top god node
- **THEN** that node appears in the rendered output by virtue of its query relevance, not via god-node force-inclusion

### Requirement: Bounded and relevance-filtered expansion hints
`formatExpansionHints` SHALL cap the number of suggested communities at the top N ranked
by relevance to the query (default N = 8), SHALL omit any community whose member count
exceeds `--max-hint-community-size` (default 50) since "include community X (2563 nodes)"
is not actionable, SHALL omit the chat-history community (`chatCommunityId`), and SHALL
omit the hint section entirely when no community passes both filters.

#### Scenario: Mega-community is hidden from hints
- **WHEN** the selected subgraph's community labels include a community with 2563 members and `--max-hint-community-size` is at its default of 50
- **THEN** that community does not appear in the `### Suggested Context Expansion` section

#### Scenario: Chat community is never suggested
- **WHEN** the chat-history community is present in `scCommunityLabels`
- **THEN** the expansion-hints section does not contain an entry for `chatCommunityId`

#### Scenario: Empty hints section is omitted
- **WHEN** every candidate community is filtered out by the size cap or the chat filter
- **THEN** no `### Suggested Context Expansion` section is rendered at all