# extraction-fidelity-harness Specification

## Purpose

Provide an on-disk ground-truth oracle for extraction fidelity, so claims like "the graph
contains the imports" are measured rather than asserted. The harness is the acceptance gate for
the `import-resolution`, `extraction-quality` and `gitignore-parsing` deltas in this change, and
a standalone diagnostic for any repository. It lives in the `graphos-test` test suite as
dedicated Hspec spec modules, compiled with the rest of the project — no external language
runtime or dev dependency is required.

## Requirements

### Requirement: Import-edge validation against on-disk ground truth

`tests/Graphos/Fidelity/ImportEdgesSpec.hs` SHALL parse every source file of a repository
directly from disk, resolve each import/re-export specifier to a file, and compare the resulting
pair set with the `imports` edges present in a `graph.json`. It SHALL report the ground-truth
pair count, the graph edge count, and the precision/recall gaps as explicit missing/extra pair
listings. It SHALL fail the Hspec spec when precision or recall falls below a configured
threshold (default 0.99), so it can gate CI.

#### Scenario: Perfect agreement passes

- **WHEN** the spec runs against a graph whose `imports` edges exactly match the on-disk pairs
- **THEN** it reports 0 missing and 0 extra pairs and the Hspec spec passes

#### Scenario: Missing edges fail the gate

- **WHEN** the graph contains no `imports` edges at all (today's tree-sitter output) and the
  repository has 203 ground-truth pairs
- **THEN** the spec reports recall 0.0 with 203 missing pairs and the Hspec spec fails

#### Scenario: Machine-readable output

- **WHEN** the spec runs in CI (`cabal test`)
- **THEN** the Hspec report contains the counts, the thresholds, and the missing/extra pair
  lists as structured assertion failures

### Requirement: Graph coverage accounting

`tests/Graphos/Fidelity/GraphCoverageSpec.hs` SHALL compare the set of source files on disk with
the set of files present in a `graph.json` and report the difference grouped by the ignore rule
class that most plausibly explains it (root-anchored build output, depth-independent tooling,
`.gitignore`, unexplained). It SHALL fail the Hspec spec when any file is unexplained.

#### Scenario: Full coverage passes

- **WHEN** every source file on disk is present in the graph
- **THEN** the spec reports 0 missing files and the Hspec spec passes

#### Scenario: Missing files are grouped by cause

- **WHEN** 85 files under directories named `build` and 1 file under `src/templates` are absent
  from the graph
- **THEN** the spec reports 85 under the build-output class, 1 as unexplained, and the Hspec
  spec fails

### Requirement: Pattern-selected subgraph extraction

`src/Graphos/UseCase/Subgraph.hs` (pure module tested via
`tests/Graphos/UseCase/SubgraphSpec.hs`) SHALL extract a subgraph from an existing `graph.json`
by selecting *core* files from a list of path patterns grouped into named subsystems, expanding
a *boundary* tier of files that import a core file or are imported by one, and an *external*
tier of package dependencies. Output SHALL conform to the `graph-json-contract` schema so it is
directly consumable via `--graph`.   Every node SHALL carry its tier, subsystem and architectural
layer, and every edge SHALL carry a provenance marker distinguishing edges taken from the source
graph from edges derived by the module. A CLI subcommand `graphos subgraph` SHALL expose the
functionality.

#### Scenario: Output loads in the query family

- **WHEN** a subgraph is extracted from a `graph.json` and passed via `--graph`
- **THEN** `graphos query`, `graphos explain` and `graphos neighbors` operate on it without
  schema errors

#### Scenario: Tiers and provenance are explicit

- **WHEN** a subgraph is extracted with a two-subsystem configuration
- **THEN** every node declares one of `core`, `boundary`, `external`, core nodes declare their
  subsystem, and every edge declares whether it came from the source graph or was derived

#### Scenario: Derived edges are unnecessary once extraction is fixed

- **WHEN** the source graph already contains `imports` edges for the selected files and the
  module is run with derivation disabled
- **THEN** the extracted subgraph has the same import edge set as with derivation enabled

### Requirement: Harness usage is documented

`README.md` SHALL document the three harness components: purpose, invocation (as Hspec specs or
CLI subcommands), flags, exit codes, and the fact that they are part of the standard `cabal
test` / `graphos` build — no external interpreter required.

#### Scenario: Documented invocation matches the implementation

- **WHEN** the invocation shown in `README.md` for each component is executed against
  `graphos-out/graph.json`
- **THEN** each component runs and exits with a documented status, and every flag shown exists
  in the CLI parser or spec module
