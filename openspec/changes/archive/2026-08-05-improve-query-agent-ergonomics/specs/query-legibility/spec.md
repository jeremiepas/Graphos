# query-legibility

Match-quality self-reporting for `graphos query` (PRD §13.1 `graphos query`, §16.1
query latency budget). Ensures an agent can always distinguish a genuine hit from a
weak or empty match, and detect when repeated queries yield no new information.

## ADDED Requirements

### Requirement: Match verdict header
`graphos query` SHALL classify every response as `strong`, `weak`, or `none` based on a
normalized best-match score, and SHALL print the verdict and the best score in a header
line before any results (text mode) and as fields in JSON mode (PRD §13.1).

#### Scenario: Strong match reports verdict
- **WHEN** a query's terms match node labels with a normalized best score at or above the strong threshold
- **THEN** the output begins with a header reporting verdict `strong` and the numeric best score, followed by results

#### Scenario: Weak match is flagged
- **WHEN** a query matches only marginally (normalized best score above zero but below the strong threshold)
- **THEN** the header reports verdict `weak` with the numeric best score, and results are still emitted

### Requirement: No fabricated results on no-match
When no node scores above zero, `graphos query` MUST NOT perform graph traversal and
MUST NOT emit any node or edge results; it SHALL report verdict `none` instead.

#### Scenario: Nonsense query produces no node set
- **WHEN** the query contains only terms absent from the graph vocabulary
- **THEN** the output contains verdict `none`, zero nodes, and zero edges

#### Scenario: Distinct failing queries do not share output
- **WHEN** two different queries both fail to match any node
- **THEN** neither response contains a result node set (previously both returned an identical fallback set)

### Requirement: Did-you-mean suggestions
On verdict `none`, and alongside results on verdict `weak`, `graphos query` SHALL emit
up to 10 suggested terms drawn from the graph's own indexed vocabulary, ranked by edit
distance and shared prefix to the query terms.

#### Scenario: Suggestions on no-match
- **WHEN** a query returns verdict `none` and the index contains tokens within edit distance 2 of a query term
- **THEN** the output lists those tokens as suggestions

#### Scenario: No suggestions fabricated
- **WHEN** no indexed token is within the edit-distance bound of any query term
- **THEN** the suggestions list is empty and clearly marked as such

### Requirement: Result-set hash
Every `graphos query` response SHALL include a short hash computed over the ordered list
of result node ids, in both text header and JSON output, so a caller can detect that a
repeated query returned no new information.

#### Scenario: Identical query yields identical hash
- **WHEN** the same query is executed twice against the same graph file
- **THEN** both responses report the same result-set hash

#### Scenario: Different result sets yield different hashes
- **WHEN** two queries produce different ordered result node id lists
- **THEN** their result-set hashes differ

### Requirement: Relevance-ordered, head-preserving output
`graphos query` SHALL emit result nodes in descending match-score order with the
per-node score visible, and SHALL truncate output at the tail when the `--budget` token
estimate is reached, emitting a footer stating how many nodes and edges were omitted.
The highest-ranked result MUST always be emitted first.

#### Scenario: Descending score order
- **WHEN** a query matches multiple nodes with distinct scores
- **THEN** rendered nodes appear in non-increasing score order

#### Scenario: Budget truncates the tail only
- **WHEN** rendered output would exceed the `--budget` token estimate
- **THEN** the top-ranked node is present in the output and a trailing footer reports the count of omitted nodes and edges
