# spec-graph-checks

The deterministic check catalogue and the `graphos speccheck` command. No LLM in this
stage: exact graph algorithms over the spec graph, with certificates on gating verdicts.
The Domain checker mirrors the Lean model shipped with this change.

## ADDED Requirements

### Requirement: Certificate-carrying cycle check

The `speccheck` SHALL detect dependency cycles over the `depends_on`, `refines` and
`supersedes` relations restricted to spec-artifact nodes. An acyclic verdict SHALL carry
a topological order of those nodes as its certificate, and the verdict SHALL be accepted
only after re-checking the certificate (every node exactly once; every checked edge goes
forward in the order) — the property proved in the Lean model: a valid topological order
implies no closed walk exists. A cyclic verdict SHALL name at least one closed walk as
its witness.

- **Plan**: Certificates make the gate's trust independent of the sort implementation —
  the linear-time re-checker is the trusted component, and it is the thing the Lean model
  certifies.
- **Do**: Kahn-style sort in `Domain/SpecCheck.hs`; certificate re-check as a separate
  pure function mirroring the Lean `isTopoOrder`.
- **Check**: Scenarios below.
- **Act**: If sort performance matters on huge spec corpora, optimize the sort freely —
  the re-checker and its Lean twin must not change.

#### Scenario: Cycle is found with a witness

- **WHEN** requirement A `depends_on` B, B `refines` C, and C `depends_on` A
- **THEN** speccheck reports a cycle naming the closed walk A → B → C → A and exits
  non-zero

#### Scenario: Acyclic verdict carries its order

- **WHEN** the spec relations form a DAG
- **THEN** the JSON report contains a topological order over the spec nodes that the
  re-checker accepts

### Requirement: Coverage checks with path certificates

The `speccheck` SHALL report every active `Requirement` node from which no path (over
`satisfies`/`references`/`contains` edges, bounded depth) reaches a code node
(**unimplemented**), and every code community no requirement reaches (**unspecified**,
reported per community, not per node). Every *implemented* verdict SHALL carry the
witnessing path, re-checked edge by edge before acceptance — path certificates are sound
by construction (Lean model). Unimplemented/unspecified findings SHALL be warnings by
default and gating only under `--strict-coverage`.

- **Plan**: Positive claims are certifiable; negative claims (no path exists) depend on
  extraction completeness, so they warn rather than block by default.
- **Do**: BFS with path tracking over `GraphIndex` adjacency; per-community rollup for
  the unspecified direction.
- **Check**: Scenarios below.
- **Act**: If unimplemented warnings are dominated by extraction gaps rather than real
  gaps, tighten the semantic-edge extraction before ever making coverage gating default.

#### Scenario: Unimplemented requirement is reported

- **WHEN** an active requirement has no path to any code node within the depth bound
- **THEN** it appears in the report as unimplemented with its source span, and the exit
  code is zero unless `--strict-coverage` is set

#### Scenario: Implemented verdict is witnessed

- **WHEN** a requirement reaches a function through `satisfies` and `contains` edges
- **THEN** the JSON report lists that path, and every hop of it is an existing edge of
  the graph

### Requirement: Contradiction candidates and structural findings

The `speccheck` SHALL enumerate contradiction candidates — every unordered pair of
distinct active requirements holding `constrains` edges to the same target node — plus:
model-asserted `conflicts_with` edges (always candidates), stale supersession (an
inactive/superseded decision still referenced by an active artifact), duplication
candidates (pairs in the same community whose embedding similarity exceeds the
configured threshold, when embeddings are present), and single-point-of-failure decisions
(Decision nodes that are articulation points between spec communities). Candidate
enumeration SHALL be complete for the shared-target pattern (Lean model: both-constrain
implies membership in the candidate list); candidates SHALL NOT gate by themselves — they
feed `conflict-adjudication`.

- **Plan**: The graph's job is to make the candidate set small and complete; judgment
  stays out of this stage entirely.
- **Do**: Pair enumeration by folding the `constrains` edge list grouped by target;
  reuse Analysis articulation points and the embeddings store.
- **Check**: Scenarios below.
- **Act**: If shared-target pairs explode on hub targets, group the finding per target
  with the pair list capped and marked truncated — completeness of enumeration is over
  the graph, presentation may bound.

#### Scenario: Shared-target pair is surfaced

- **WHEN** requirement R1 and requirement R2 both hold `constrains` edges to node
  `graph.html`
- **THEN** the unordered pair (R1, R2) appears in the contradiction candidates with the
  shared target named

#### Scenario: Stale supersession is flagged

- **WHEN** decision D is marked superseded and an active requirement holds a `references`
  edge to D
- **THEN** speccheck reports the stale reference with both spans

### Requirement: speccheck command and report contract

The `graphos speccheck` command SHALL run the full catalogue over a graph
(`--graph`, default `graphos-out/graph.json`), emit a markdown report on stdout and a
single JSON document under `--json` (findings typed by check, each with severity,
spans, and certificate or witness where applicable), send errors to stderr, and exit
non-zero exactly when at least one gating finding exists (confirmed conflicts, cycles;
plus coverage under `--strict-coverage`). `--check NAME` (repeatable) SHALL restrict the
run. The command SHALL be registered with subcommand help and a worked example per
`cli-subcommand-help`.

- **Plan**: Same stdout/stderr and single-document discipline as `query-cli-contract`
  and the diff command — a CI gate must be scriptable.
- **Do**: CLI wiring; one findings structure rendered twice.
- **Check**: Scenarios below.
- **Act**: If CI wants SARIF or similar later, add a format — the findings structure is
  the contract.

#### Scenario: Clean corpus exits zero

- **WHEN** speccheck runs on a spec graph with no cycles and no confirmed conflicts
- **THEN** the report states the counts per check and the exit code is zero

#### Scenario: JSON is one document

- **WHEN** `graphos speccheck --json` runs
- **THEN** stdout is exactly one JSON document and all progress/errors are on stderr
