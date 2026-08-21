# neighbor-expansion

Cheap foothold expansion via `graphos neighbors <node-id>` subcommand (extends
PRD §13.1 command table; workflow doc 15-neighbors). Lets an agent grow context from a
known-good node (e.g. one returned by `explain` or `symbols`) without re-entering fuzzy
search.

This change widens the argument so agents can pass the display name they just saw in
output, not only the internal `mod_*`/numeric id.

## MODIFIED Requirements

### Requirement: Depth-bounded neighborhood expansion
The CLI SHALL provide `graphos neighbors <id-or-name> [--depth N]` (default depth 2) which
performs breadth-first expansion from the resolved node over the graph adjacency and
returns all reached nodes and their connecting edges. The argument SHALL be resolved in
this order:

1. exact node id match (`Map.lookup arg (gNodes g)`);
2. exact label match via the label index (`symbolLookup` exact case-sensitive path);
3. case-insensitive label match (`symbolLookup` fallback path).

If resolution yields exactly one node, expansion proceeds from that node. If resolution
yields zero nodes, the command SHALL report the argument as not found and return no
results. If resolution yields more than one node, the command SHALL NOT perform fuzzy
traversal and SHALL report every candidate with its distinct source location so the caller
can disambiguate and re-run with a node id. Resolution MUST remain pure (UseCase layer); the
CLI dispatcher (Infrastructure) only wires the resolved id into `neighborhoodExpansion`.


#### Scenario: Direct neighbors by internal id
- **WHEN** `graphos neighbors mod_Graphos.UseCase.QuerySpec --depth 1` is run on a node with three adjacent nodes
- **THEN** exactly those three neighbors and their connecting edges are returned, identical to the pre-change behavior

#### Scenario: Display name fallback resolves a single node
- **WHEN** `graphos neighbors Graphos.UseCase.QuerySpec --depth 1` is run and no node has that exact id but exactly one node has that label
- **THEN** the expansion proceeds from that node and returns the same neighborhood as if the internal id had been passed

#### Scenario: Case-insensitive label fallback
- **WHEN** `graphos neighbors graphos.usecase.queryspec --depth 1` is run and only `Graphos.UseCase.QuerySpec` exists as a label
- **THEN** the case-insensitive fallback resolves that node and the expansion proceeds

#### Scenario: Ambiguous name lists candidates without traversal
- **WHEN** `graphos neighbors parse --depth 1` is run and two distinct nodes have the label `parse` in different source files
- **THEN** both candidates are listed with their node id, label, and source file, no BFS expansion is performed, and no fuzzy traversal is invoked

#### Scenario: Unknown name fails explicitly
- **WHEN** the given argument matches no node id and no label
- **THEN** the command reports the argument as not found and returns no results

#### Scenario: Depth bound respected
- **WHEN** `--depth 2` is used
- **THEN** no returned node is more than two hops from the resolved start node