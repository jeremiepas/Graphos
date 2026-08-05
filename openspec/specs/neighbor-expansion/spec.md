# neighbor-expansion

Cheap foothold expansion via a new `graphos neighbors <node-id>` subcommand (extends
PRD §13.1 command table; workflow doc 15-neighbors). Lets an agent grow context from a
known-good node (e.g. one returned by `explain` or `symbols`) without re-entering fuzzy
search.

## ADDED Requirements

### Requirement: Depth-bounded neighborhood expansion
The CLI SHALL provide `graphos neighbors <node-id> [--depth N]` (default depth 2) which
performs breadth-first expansion from the exact node id over the graph adjacency and
returns all reached nodes and their connecting edges. The command MUST treat the
argument as a node id, not a fuzzy term.

#### Scenario: Depth 1 returns direct neighbors
- **WHEN** `graphos neighbors <id> --depth 1` is run on a node with three adjacent nodes
- **THEN** exactly those three neighbors and their connecting edges are returned

#### Scenario: Depth bound respected
- **WHEN** `--depth 2` is used
- **THEN** no returned node is more than two hops from the start node

#### Scenario: Unknown node id fails explicitly
- **WHEN** the given node id does not exist in the graph
- **THEN** the command reports the id as not found and returns no results

### Requirement: Neighborhood output uses shared noise controls and ranking
`graphos neighbors` output SHALL pass through the same semantic edge filtering, self-edge
collapse, deduplication, and label elision as `graphos query`, and SHALL order nodes by
proximity (closer hops first).

#### Scenario: Trivia filtered from neighborhood
- **WHEN** a neighbor is connected only by a `contains` edge to a trivia node such as `undefined`
- **THEN** that edge is absent from default output

#### Scenario: Proximity ordering
- **WHEN** the expansion reaches nodes at hop 1 and hop 2
- **THEN** all hop-1 nodes are rendered before hop-2 nodes
