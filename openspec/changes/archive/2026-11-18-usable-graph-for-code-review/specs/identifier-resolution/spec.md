## ADDED Requirements

### Requirement: Identifier index covers every code node

`Domain.Graph.Index` SHALL index every node under its identifier — the node label as produced by
the extractor (PRD §3.2), which for code nodes is the declared name and for module nodes the file
name — both exactly and lowercased, in addition to the existing tokenized label index. A node whose
label is a declaration body or a whole file SHALL NOT exist in a conforming graph (see
`extraction-quality`), so the identifier index is the label index with identifiers as keys. (PRD
§13 query commands, workflows 04–06)

#### Scenario: Function found by its declared name
- **WHEN** `./src/db.ts` declares `export function dbGetById(id: UserId): User | undefined { … }`
  and the graph is built
- **THEN** `giLabelIndex` maps `dbGetById` and `dbgetbyid` to that node's id, verifiable by
  `cabal test` on the index built from the extraction

#### Scenario: Module found by its file name
- **WHEN** the graph contains the module node for `./src/workers/workflow-task-worker.ts`
- **THEN** `giLabelIndex` maps `workflow-task-worker.ts` to that node's id

### Requirement: One resolution contract for node arguments

`Graphos.UseCase.Query.resolveNodeArg :: Text -> Graph -> GraphIndex -> NodeResolution` SHALL be
the only way `symbols`, `explain`, `path` and `neighbors` turn a CLI argument into a node (PRD
§13, workflows 04–06). Resolution order: (1) exact node id, (2) exact identifier, (3)
case-insensitive identifier. Exactly one match yields `ResolvedSingle`; more than one yields
`Ambiguous` with every candidate (id, label, kind, source file, line); none yields `NotFound`.
Resolution SHALL NOT tokenize the argument, score candidates, or traverse the graph; fuzzy
matching remains the behaviour of `query` only.

#### Scenario: Exact identifier wins without fuzzy search
- **WHEN** `resolveNodeArg "executeSharedRunCommand"` is evaluated on a graph with one node
  labelled `executeSharedRunCommand`
- **THEN** the result is `ResolvedSingle` with that node's id, verifiable by `cabal test`

#### Scenario: Same identifier in two files is ambiguous
- **WHEN** two files each declare a function named `main`
- **THEN** `resolveNodeArg "main"` returns `Ambiguous` with both candidates and their source
  files, and never silently picks one

#### Scenario: Unknown name is not found
- **WHEN** `resolveNodeArg "doesNotExist"` is evaluated
- **THEN** the result is `NotFound`, not a best-effort fuzzy node

### Requirement: Node-argument commands report resolution outcomes

`graphos symbols`, `graphos explain`, `graphos path` and `graphos neighbors` SHALL render
`NotFound` as a message naming the argument plus up to five suggestions from the identifier index,
exiting with a non-zero status, and SHALL render `Ambiguous` as a candidate list (id, kind, source
file, line) that lets the caller re-run with an id. With `--json` the same outcomes SHALL be
emitted through the existing `renderNotFoundJSON` / `renderAmbiguousJSON` renderers with no
interleaved log lines. (PRD §13.2 flags)

#### Scenario: Explain refuses to guess
- **WHEN** `graphos explain "15996_shared-run-command_Function:executeSharedRunCommand"` is run
  and no node has that id or identifier
- **THEN** stdout reports `Node not found` with suggestions and the exit status is non-zero,
  instead of printing an unrelated node

#### Scenario: Path with an ambiguous endpoint lists candidates
- **WHEN** `graphos path main dbGetById` is run and `main` resolves to two nodes
- **THEN** the command prints the two candidates for `main` with their ids and source files and
  computes no path
