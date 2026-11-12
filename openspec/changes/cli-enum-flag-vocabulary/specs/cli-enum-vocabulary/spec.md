# cli-enum-vocabulary

## Purpose

Define the accepted token vocabulary, metavar, help derivation, numeric bounds and
error shape of every enum-valued and numeric option on the `graphos` CLI, so that
the vocabulary a command documents is exactly the vocabulary it accepts
(PRD §13.1 commands, §13.2 flags).

## ADDED Requirements

### Requirement: Enum options accept their documented lowercase vocabulary

Every enum-valued option SHALL accept the lowercase token vocabulary below, matching
case-insensitively. The vocabulary is the one already used by `PushMode`'s JSON
instances, by `graphos.yaml`, by workflow docs 01/09/12/13, and by spec
`12-neo4j-push` (PRD §13.2).

| Option | Commands | Tokens |
| --- | --- | --- |
| `--edge-density` | run, merge | `sparse`, `normal`, `dense`, `maximum` |
| `--neo4j-push-mode` | run | `full`, `subgraph`, `community` |
| `--mode` | push | `full`, `subgraph`, `community` |
| `--memgraph-push-mode` | run | `full`, `subgraph`, `community` |
| `--mode` | push-memgraph | `full`, `subgraph`, `community` |
| `--granularity` | run | `fine`, `function`, `file` |
| `--detect-mode` | run | `exclude`, `collapse`, `off` |
| `--edges` | query, path, explain, symbols, neighbors, research | `semantic`, `all` |
| `--cluster-algorithm` | run | `leiden`, `bunch` |

#### Scenario: Lowercase edge density is accepted
- **WHEN** `graphos --edge-density sparse .` is run
- **THEN** the option parses to `Sparse` and the command proceeds instead of failing with `cannot parse value`

#### Scenario: Short push-mode token is accepted
- **WHEN** `graphos --neo4j-push-mode full .` is run
- **THEN** the option parses to `FullPush`, satisfying spec `12-neo4j-push`'s documented override

#### Scenario: Memgraph push mode uses the same vocabulary as Neo4j
- **WHEN** `--memgraph-push-mode subgraph` is passed
- **THEN** the option parses to `MemgraphSubgraph`

#### Scenario: Matching is case-insensitive
- **WHEN** `--edge-density SPARSE`, `--edge-density Sparse` and `--edge-density sparse` are each passed
- **THEN** all three parse to the same `EdgeDensity` value

### Requirement: Help text and metavar derive from the reader's token table

Each enum option SHALL be built from a single token table that supplies the reader,
the `metavar`, and the token list printed in `--help`. No option may print a token it
rejects, and no option may accept a primary token it does not print. Every enum option
SHALL declare a metavar (`DENSITY`, `MODE`, `LEVEL`, `ALGO`) so usage renders
`--edge-density DENSITY` rather than `--edge-density ARG`.

#### Scenario: Every documented token parses
- **WHEN** the test registry iterates every enum option and feeds it each token printed in that option's help body
- **THEN** every one of those invocations parses successfully

#### Scenario: Every accepted primary token is documented
- **WHEN** the test registry iterates every enum option's primary token table
- **THEN** each token appears verbatim in the rendered `--help` body for that option

#### Scenario: Usage shows a metavar
- **WHEN** `graphos --help` is rendered
- **THEN** the usage line contains `--edge-density DENSITY` and contains no `--edge-density ARG`

#### Scenario: Registry covers every enum option
- **WHEN** an enum-valued option exists in the parser that has no entry in the registry
- **THEN** the registry-coverage test fails

### Requirement: Rejected enum values name the accepted vocabulary

An unparseable enum value SHALL produce an error naming the option, quoting the
offending value, and listing every primary token, then exit non-zero without running
any pipeline stage.

#### Scenario: Unknown density lists the vocabulary
- **WHEN** `graphos --edge-density sparce .` is run
- **THEN** stderr contains `--edge-density`, the quoted value `sparce`, and all of `sparse`, `normal`, `dense`, `maximum`, and the exit code is non-zero

#### Scenario: No stage runs on a rejected value
- **WHEN** an enum option is given an unknown value
- **THEN** no detect/extract/build output is produced and no output directory is written

### Requirement: Constructor spellings remain accepted as aliases

The Haskell constructor spellings previously required by `option auto` SHALL continue
to parse to the same values, so no invocation accepted before this change is rejected
after it. Aliases SHALL NOT appear in `--help`, in `renderCommandReference`, or in the
documentation.

| Option | Aliases |
| --- | --- |
| `--edge-density` | `Sparse`, `Normal`, `Dense`, `Maximum` |
| `--neo4j-push-mode`, push `--mode` | `FullPush`, `SubgraphPush`, `CommunityPush` |
| `--memgraph-push-mode`, push-memgraph `--mode` | `MemgraphFull`, `MemgraphSubgraph`, `MemgraphCommunity` |

#### Scenario: Existing invocation keeps working
- **WHEN** `graphos --edge-density Sparse .` is run
- **THEN** it parses to `Sparse` exactly as it did before this change

#### Scenario: Alias and primary token agree
- **WHEN** `--memgraph-push-mode MemgraphSubgraph` and `--memgraph-push-mode subgraph` are parsed
- **THEN** both yield the same `PipelineConfig` value

#### Scenario: Aliases stay out of help
- **WHEN** the `--help` body for `--edge-density` is rendered
- **THEN** it contains `sparse` and does not contain `Sparse`

### Requirement: Numeric options are bounded and carry a metavar

Every numeric option SHALL be parsed by a bounded reader that rejects out-of-range
values at parse time with a message naming the option and its range, and SHALL declare
a metavar (`N`, `PORT`, `SECONDS`, `FLOAT`). Where `0` is a documented sentinel it
remains legal.

| Bound | Options |
| --- | --- |
| `1..65535` | `--port`, `--metrics` |
| `>= 1` | `--threads`, `--lsp-concurrency`, `--max-leiden-iterations`, `--min-comm-size`, `--timeout`, `--otel-shutdown-timeout`, `--minified-threshold`, `--max-sampled-sources`, `--exact-betweenness-node-cap`, `--neo4j-subgraph-size`, `--memgraph-subgraph-size`, `--subgraph-size` |
| `>= 0` | `--budget`, `--label-width`, `--depth`, `--boundary-hops`, `--max-nodes`, `--max-label-chars` |
| `> 0.0` | `--resolution` |

#### Scenario: Non-positive thread count is rejected
- **WHEN** `graphos --threads 0 .` or `graphos --threads=-1 .` is run
- **THEN** parsing fails with a message naming `--threads` and its range, and no stage runs

#### Scenario: Out-of-range port is rejected
- **WHEN** `graphos serve --port 99999` is run
- **THEN** parsing fails with a message naming `--port` and the range `[1,65535]`

#### Scenario: Documented sentinel stays legal
- **WHEN** `graphos query "term" --max-nodes 0 --max-label-chars 0` is run
- **THEN** parsing succeeds and the documented unlimited/default behaviour applies

#### Scenario: Non-positive resolution is rejected
- **WHEN** `graphos --resolution 0 .` is run
- **THEN** parsing fails with a message naming `--resolution` and its range

### Requirement: Command reference and documentation match the readers

`renderCommandReference` and the published documentation SHALL use the same primary
tokens as the readers. `docs/workflows/01-full-pipeline.md` and
`docs/workflows/09-merge.md` SHALL document `--edge-density` as a token-valued option
rather than the numeric `N` / `0.0` they describe today.

#### Scenario: Reference prints the density vocabulary
- **WHEN** `renderCommandReference` is rendered
- **THEN** it lists `sparse|normal|dense|maximum` for `--edge-density` and stays within its 60-line budget

#### Scenario: Workflow docs no longer describe a float
- **WHEN** `docs/workflows/01-full-pipeline.md` and `docs/workflows/09-merge.md` are inspected
- **THEN** neither documents `--edge-density` with a numeric default of `0.0`

#### Scenario: Push-mode docs match the reader
- **WHEN** `docs/workflows/12-neo4j-push.md` and `docs/workflows/13-memgraph-push.md` are inspected
- **THEN** both document `full | subgraph | community` for their push-mode flags
