## Purpose

Build a single knowledge graph from multiple named filesystem sources (repositories, worktrees, corpora) declared in graphos.yaml, with per-source node identity and taggging, multi-root watching, and a configured output directory.

## ADDED Requirements

### Requirement: Source declarations in graphos.yaml

`graphos.yaml` SHALL support a `sources:` key: a list of entries with `name` (non-empty, unique across the list), `path` (existing directory; `~` expansion and env-var-style placeholders permitted), and an optional `ignore` list (extra ignore patterns scoped to that source, in addition to its `.gitignore`/`.graphosignore`). When `sources:` is absent or empty, the pipeline SHALL behave exactly as today (single positional PATH argument). The config SHALL also support an `output:` key (graph output directory) that the `-o` CLI flag overrides when given. Config loading MUST reject `sources:` with duplicate or empty names with a validation error naming the offending entry. (PRD §13, workflow 15)

#### Scenario: Absent sources key keeps single-path behavior

- **WHEN** `graphos.yaml` has no `sources:` key and the user runs `graphos .`
- **THEN** the pipeline SHALL scan the positional PATH argument exactly as before and produce identical NodeIds, graph outputs, and cache keys to a pre-change build

#### Scenario: Duplicate source names rejected

- **WHEN** config contains two sources both named `repoA`
- **THEN** config loading SHALL fail with an error message naming the duplicated source name

#### Scenario: Nonexistent source path rejected

- **WHEN** a source path does not exist or is not a directory
- **THEN** config validation SHALL fail with an error naming the source and the offending path

#### Scenario: Output key honored

- **WHEN** `graphos.yaml` sets `output: my-graph-out` and no `-o` flag is passed
- **THEN** all graph artifacts SHALL be written to `my-graph-out/`

### Requirement: Unioned multi-root detection with source-qualified paths

When `sources:` is configured, the detect stage SHALL walk every source root (respecting each root's own `.gitignore`/`.graphosignore` plus the source's `ignore` list) and the resulting file set SHALL be the union of all walks. Each detected file SHALL be identified as `sourceName/relativePath` where `relativePath` is relative to that source's root. All NodeIds derived from a multi-source file set SHALL incorporate the full `sourceName/relativePath` so identical relative paths in different sources yield distinct NodeIds. A file present in two overlapping roots (e.g., a worktree nested inside a parent repository) SHALL be attributed to the source whose root is the longest matching prefix, appearing exactly once in the file set. (PRD §3.1, detect stage)

#### Scenario: Same relative path in two sources yields distinct nodes

- **WHEN** source `repoA` and source `repoB` both contain `src/Utils.hs`
- **THEN** the graph SHALL contain two distinct nodes with `nodeSourceFile` values `repoA/src/Utils.hs` and `repoB/src/Utils.hs` and distinct NodeIds

#### Scenario: Overlapping roots attribute to longest prefix

- **WHEN** source `main` is `~/repo` and source `wt` is `~/repo/.worktrees/feat`, and both walks encounter `~/repo/.worktrees/feat/src/Main.hs`
- **THEN** the file SHALL appear exactly once in the file set, attributed to source `wt` with identifier `wt/src/Main.hs`

#### Scenario: Per-source gitignore respected

- **WHEN** source `repoB`'s `.gitignore` excludes `generated/` and the source also lists `ignore: ["vendor"]`
- **THEN** files under `repoB`'s `generated/` and `vendor/` directories SHALL be absent from the unioned file set while other sources' directories of the same names are unaffected

### Requirement: Node source tagging

Every node extracted from a multi-source run SHALL carry a `nodeSource` field whose value is the source name it was detected under, and its JSON serialization SHALL emit a nullable `source` key. Nodes from single-source (legacy positional-PATH) runs SHALL have `nodeSource = null` and serialize `source: null`. Downstream exports (HTML viewer, report) SHALL include the source tag in their view models so nodes can be visually distinguished by source. (PRD §5.1 node schema, §3.2 build stage)

#### Scenario: Multi-source nodes carry source tags

- **WHEN** the pipeline runs over two configured sources and exports `graph.json`
- **THEN** every node from each source has a `source` key equal to that source's name, and no node has a null `source`

#### Scenario: Legacy single-path nodes remain null

- **WHEN** the pipeline runs without `sources:` configured
- **THEN** every exported node's `source` key is `null`

#### Scenario: Viewer distinguishes sources

- **WHEN** the HTML viewer renders a multi-source graph
- **THEN** each node's source is available in the view model and nodes from different sources are visually distinguishable

### Requirement: Multi-root watch with source attribution

With `sources:` configured, `--watch` SHALL start one recursive filesystem watcher per source root. Each change event SHALL be attributed to the source owning the event path by longest-prefix ownership (same rule as detection) before being passed to the incremental pipeline with its source-qualified identifier. Watcher callbacks SHALL receive a real list of file paths, not a joined string. Debounce coalescing SHALL operate on the union of events across all watchers. (PRD §3.4, workflow 03)

#### Scenario: Change in one source triggers incremental update

- **WHEN** a file in source `repoB` is modified during `--watch` over three configured sources
- **THEN** the watcher SHALL coalesce and pass source-qualified paths to the incremental pipeline, which re-extracts only the changed files

#### Scenario: Nested worktree event not double-claimed

- **WHEN** a file changes inside `~/repo/.worktrees/feat/src/` during `--watch` with both `main` (`~/repo`) and `wt` (`~/repo/.worktrees/feat`) configured
- **THEN** the event SHALL be attributed to `wt` exactly once, not to both watchers

### Requirement: Add or remove a source via update run

Adding or removing a `sources:` entry and running an update (`--update`) SHALL produce the graph for the new source set such that: files of newly added sources are extraction-cache misses (extracted and cached), files of unchanged sources are extraction-cache hits (extraction skipped), and files of removed sources are absent from the file set so their nodes and edges vanish from the exported graph. (PRD §3.4, workflow 02)

#### Scenario: Adding a source extracts only the new source

- **WHEN** a graph exists for sources `[repoA, repoB]` and `repoC` is added, then `graphos --update` runs
- **THEN** repoA and repoB files SHALL be extraction-cache hits and repoC files extracted, and the exported graph SHALL contain repoC's nodes

#### Scenario: Removing a source drops its nodes

- **WHEN** `repoB` is removed from `sources:` and `graphos --update` runs
- **THEN** no node in the exported graph has `source: "repoB"` and no edge references a former repoB node

#### Scenario: Existing source NodeIds stable across source additions

- **WHEN** repoC is added and an update run completes
- **THEN** repoA's nodes retain the same NodeIds they had before the addition