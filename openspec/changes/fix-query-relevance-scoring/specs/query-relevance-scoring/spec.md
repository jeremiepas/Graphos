# query-relevance-scoring Specification

## ADDED Requirements

### Requirement: Normalized matched-term score
The relevance score of a matched node SHALL be its matched-term count divided by the
number of query terms, i.e. `normalizeScore rawScore (length terms)`, where `rawScore`
is the number of distinct query terms that matched the node and `terms` are the
normalized query terms. The score SHALL be in the range `[0, 1]`.

#### Scenario: Partial term match
- **WHEN** a query has 2 terms and a node matches 1 of them
- **THEN** the node's normalized score is `0.5`

#### Scenario: Full term match
- **WHEN** a query has 2 terms and a node matches both
- **THEN** the node's normalized score is `1.0`

### Requirement: Query-term full-label boost
A matched node SHALL receive a `+0.1` full-label boost when **any** query term exactly
equals the node's full label (compared case-insensitively). The boost SHALL be `0`
otherwise. The boost SHALL be computed from the query terms, never from the node's own
label tokens.

#### Scenario: Query term equals full label
- **WHEN** the query is `foo` and a matched node's full label is `Foo`
- **THEN** the node receives the `+0.1` full-label boost

#### Scenario: No query term equals full label
- **WHEN** the query is `foo bar` and a matched node's full label is `baz`
- **THEN** the node receives no full-label boost (`0`)

#### Scenario: Multi-word label does not self-match
- **WHEN** a node's full label is `foo bar` (two words) and no query term equals `foo bar`
- **THEN** the node receives no full-label boost, regardless of its first token

### Requirement: Non-matching neighborhood nodes score zero
A node in the expanded neighborhood that matched no query term SHALL have a relevance
score of `0.0`. Non-matching neighborhood nodes SHALL NOT receive a flat score from the
full-label boost.

#### Scenario: Single-word neighbor with no match
- **WHEN** a single-word-labeled node is in the expanded neighborhood but matched no
  query term
- **THEN** its score is `0.0` (not `0.1`)

### Requirement: Score-descending ranking with matched nodes first
The ranked result list (`qrespNodes`) SHALL be ordered by score descending. Matched
nodes (score `> 0`) SHALL be ordered before non-matching context neighbors (score
`0.0`).

#### Scenario: Matched nodes rank above context
- **WHEN** the expanded neighborhood contains both matched nodes and non-matching
  single-word neighbors
- **THEN** all matched nodes appear before all `0.0`-scored neighbors in `qrespNodes`
