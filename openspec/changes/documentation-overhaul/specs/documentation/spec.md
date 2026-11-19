# documentation

Diátaxis-structured documentation tree with a single index, an accurate front-door README,
single-sourced governance docs, and a `cabal test`-enforced drift guard. Grounded in the
workflow docs (`docs/workflows/01…18`) and the `openspec/config.yaml` project context (no PRD.md
exists in this repository; config rule reference fixed by this change).

## ADDED Requirements

### Requirement: Documentation index and Diátaxis structure

The repository SHALL provide `docs/README.md` as the single documentation entry point, organizing
every audience-facing document under exactly one Diátaxis quadrant (Tutorials, How-to/Workflows,
Reference, Explanation). Every `*.md` file under `docs/` — excluding `docs/internal/`,
`docs/math/`, and `docs/math-requirements/` (the latter two indexed as a single "Formal
foundations" entry) — SHALL be reachable from this index. Internal working notes and
point-in-time snapshots SHALL live under `docs/internal/` and SHALL NOT appear in the audience
index.

#### Scenario: index covers the documentation tree

- **WHEN** `cabal test` runs the DocsSpec index check over `docs/`
- **THEN** every audience-facing `*.md` under `docs/` is linked from `docs/README.md`, and no
  file under `docs/internal/` is linked from it

#### Scenario: snapshot is marked as point-in-time

- **WHEN** `docs/internal/openspec-state-report.md` is read
- **THEN** its header states the generation date, the source commit, and the command to
  regenerate it

### Requirement: README front door accuracy

`README.md` SHALL serve as the front door only: identity, install, quickstart, capability
summary, output tour, and links into the four `docs/` quadrants. It SHALL contain exactly one
"LSP security notes" section and SHALL NOT duplicate any section. Deep reference content (full
file-type tiers, full CLI reference, MCP tool list) SHALL live under `docs/reference/` and be
linked from the README.

#### Scenario: no duplicated sections

- **WHEN** `cabal test` runs the DocsSpec README check
- **THEN** `README.md` contains exactly one heading titled "LSP security notes" and no two
  identical headings

#### Scenario: front door links resolve

- **WHEN** `cabal test` runs the DocsSpec link check over `README.md`
- **THEN** every relative link and anchor in `README.md` resolves to an existing file or heading

### Requirement: CLI reference coverage

`docs/reference/cli.md` SHALL document every subcommand exposed by the CLI parser
(`src/Graphos/CLI/Parser.hs`), including `ingest`, `research`, `clustereval`, and
`install-skill`, which the current README omits.

#### Scenario: parser and reference agree

- **WHEN** `cabal test` compares the parser's subcommand names against `docs/reference/cli.md`
- **THEN** every parser subcommand name appears in the reference, and the test fails naming any
  missing subcommand

### Requirement: MCP tool reference parity

`docs/reference/mcp-tools.md` SHALL list exactly the tool names exported by the MCP server
(`allTools` in `src/Graphos/Infrastructure/Server/MCP.hs`; 13 tools at the time of this change).
Documentation stating an MCP tool count SHALL match that list's length.

#### Scenario: documented tools match the server

- **WHEN** `cabal test` compares the tool names in `docs/reference/mcp-tools.md` against
  `allTools`
- **THEN** the two sets are equal, and the test fails naming any tool missing from or extra in
  the documentation

### Requirement: Workflow index integrity

The `docs/workflows/README.md` index table SHALL correspond 1:1 with the `NN-*.md` files on disk
in `docs/workflows/`: every file has exactly one row, every row links to an existing file, and
row numbering matches file numbering.

#### Scenario: index rows match files on disk

- **WHEN** `cabal test` compares the index table rows against `docs/workflows/*.md`
- **THEN** the sets are equal in both directions and every link target exists (today
  `14-observability.md`, `15-config-init.md`, `16-lsp-discovery.md` are linked but absent, and
  `14-symbols.md`, `15-neighbors.md`, `16-agent-scaffolding.md` exist but are unlinked — this
  scenario fails until fixed)

### Requirement: Link integrity across documentation

Every relative link and intra-document anchor in `README.md`, `CONTRIBUTING.md`, and
`docs/**/*.md` SHALL resolve to an existing file or heading, using GitHub anchor slugging rules.

#### Scenario: broken link fails the suite

- **WHEN** `cabal test` runs the DocsSpec link check and a document links to a missing file or
  heading (e.g. `CONTRIBUTING.md` → `README.md#language-server-requirements` today)
- **THEN** the test fails and names the source file, the link target, and the line

### Requirement: Single-sourced governance documentation

The branch/merge model SHALL be described normatively in exactly one document (`GITFLOW.md`,
trunk-based per the accepted Option A). `CONTRIBUTING.md` SHALL link to `GITFLOW.md` for the
branch model and to `docs/reference/release-policy.md` for the release process, and SHALL NOT
prescribe a `develop`-based integration flow or duplicate the release procedure.

#### Scenario: contributing defers to the branch model owner

- **WHEN** `cabal test` runs the DocsSpec governance check over `CONTRIBUTING.md`
- **THEN** `CONTRIBUTING.md` contains a link to `GITFLOW.md`, contains no instruction to open
  PRs against or merge through a `develop` branch, and contains a link to the release policy
  instead of an inline release procedure
