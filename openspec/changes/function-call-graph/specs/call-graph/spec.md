## ADDED Requirements

### Requirement: Call edges are emitted at function granularity

At `function` granularity the tree-sitter extractor SHALL emit a `calls` edge for every
call located inside the body of an emitted function, method or constructor, with the
same source, target and edge shape as at `fine` granularity. The walk that collects
call edges MUST descend into bodies that are opaque for node emission. No node SHALL be
emitted for a construct inside such a body. For a given file, the set of `calls` edges
whose two ends are nodes emitted at `function` granularity SHALL be identical at
`function` and `fine` granularity.

#### Scenario: Same-file call inside a body at function granularity

- **WHEN** a TypeScript file containing `function a() {}` and `function b() { a(); }` is extracted at `function` granularity
- **THEN** the extraction SHALL contain one `calls` edge from the node of `b` to the node of `a`, and no node for the call expression

#### Scenario: Function and fine granularity agree on call edges

- **WHEN** the same file is extracted at `function` and at `fine` granularity
- **THEN** the `calls` edges between definition nodes SHALL be the same set in both extractions

#### Scenario: Node volume is unchanged

- **WHEN** a file is extracted at `function` granularity before and after this requirement
- **THEN** the set of emitted node ids SHALL be identical

### Requirement: Built graph resolves an imported callee to its definition

The built graph SHALL end a call to an imported name on the definition of that name.
This applies when the import's target is a file of the corpus that emits exactly one
definition node with that name: the graph SHALL then contain a `calls` edge from the
caller's node to that definition node, and SHALL NOT contain a `calls` edge from that
caller to the imported file's module node for that call. When the target file is outside the corpus, emits no definition with that name,
or emits more than one, the edge SHALL keep the target the extractor gave it.
Resolution SHALL be name-based and SHALL NOT use type information.

#### Scenario: Cross-file call ends on the function

- **WHEN** `a.ts` exports `function verdict()` and `b.ts` contains `import { verdict } from './a.js'` and `function wrap() { return verdict(); }`, and both are built into one graph
- **THEN** the graph SHALL contain a `calls` edge from the node of `wrap` to the node of `verdict`, and no `calls` edge from `wrap` to the module node of `a.ts`

#### Scenario: Renamed import resolves through the binding

- **WHEN** `b.ts` contains `import { verdict as v } from './a.js'` and a function that calls `v()`
- **THEN** the `calls` edge SHALL end on the node of `verdict` in `a.ts`

#### Scenario: Callee outside the corpus keeps its target

- **WHEN** a function calls a name imported from a package that is not in the corpus
- **THEN** the `calls` edge SHALL keep the import target node the extractor emitted

#### Scenario: Ambiguous definition is not guessed

- **WHEN** the imported file emits two definition nodes with the callee's name
- **THEN** the `calls` edge SHALL keep the module node as its target

### Requirement: Callers and callees commands

The CLI SHALL provide `graphos callers <node>` and `graphos callees <node>`. Both SHALL
turn the argument into a node through `resolveNodeArg`, and SHALL render `NotFound` and
`Ambiguous` as the other node-argument commands do. `callers` SHALL return the nodes
that reach the argument through `calls` edges in the incoming direction; `callees`, in
the outgoing direction. Both SHALL accept `--graph`, `--budget`, `--json`,
`--label-width`, `--edges`, `--max-nodes`, `--path` and `--depth` (default 1). Edges of
any other relation SHALL be ignored. Each result row SHALL carry the node id, label,
kind, source file, line range when present, the distance from the argument, and the
confidence of the edge that reached it.

#### Scenario: Direct callers of a function

- **WHEN** `graphos callers verdict --json` is run on a graph where `wrap` and `K.run` call `verdict` and `verdict` calls `helper`
- **THEN** the output SHALL list exactly `wrap` and `K.run`, each at distance 1, and SHALL NOT list `helper`

#### Scenario: Direct callees of a function

- **WHEN** `graphos callees verdict --json` is run on the same graph
- **THEN** the output SHALL list exactly `helper`

#### Scenario: Depth extends the traversal

- **WHEN** `graphos callers helper --depth 2 --json` is run on the same graph
- **THEN** the output SHALL list `verdict` at distance 1 and `wrap` and `K.run` at distance 2

#### Scenario: Other relations are ignored

- **WHEN** a module node `contains` the argument and another file `imports` its module
- **THEN** neither SHALL appear in the output of `callers`

#### Scenario: Ambiguous name is not guessed

- **WHEN** `graphos callers main` is run on a graph where two files each define `main`
- **THEN** the command SHALL print both candidates with id, kind and source file, and exit with a non-zero status

### Requirement: A graph without call edges is reported as such

`graphos callers` and `graphos callees` SHALL report a graph that holds no call edges.
When the loaded graph contains no edge with relation `calls`, both SHALL print that
fact and SHALL exit with a non-zero status. With `--json` the output SHALL be a JSON object with an `error` code
of `no_call_edges`. An empty result on a graph that does hold call edges SHALL remain a
successful, empty result.

#### Scenario: Graph built without call edges

- **WHEN** `graphos callers verdict` is run on a graph whose edges are only `contains` and `imports`
- **THEN** the command SHALL print that the graph holds no call edges and exit with a non-zero status

#### Scenario: Function with no caller in a graph that has call edges

- **WHEN** `graphos callers wrap --json` is run on a graph that holds `calls` edges, none of which ends on `wrap`
- **THEN** the output SHALL be an empty result and the exit status SHALL be zero
