# staging-preserve

## Context

A full rebuild (`graphos <path> --fresh -o <out>`, or any staged rebuild) writes
its artifacts into a fresh staging directory and then swaps it into `<out>` via
directory renames. Before the swap, `carryOverState` moved only the four
persistent-state subdirectories (`cache/`, `memory/`, `debug/`, `traces/`) from
the old `<out>` into staging, so the swap's rename-and-delete discarded every
other entry in `<out>` — including a file the user (or a plugin) placed there.

## ADDED Requirements

### Requirement: Preserve non-Graphos entries across a staged rebuild

Graphos SHALL carry over every entry in the output directory that it does not
regenerate into the staging directory before the swap, so a rebuild never
discards a file Graphos did not write. The swap is a rename-and-delete: the old
output becomes `<out>.prev-<ts>-<pid>` and is deleted once the fresh staging
directory is promoted, so anything left in the old output on the swap path is
lost.

- **Plan**: enumerate the old output directory and carry over every entry not in
  the Graphos-owned allowlist (`ownedArtifacts`) into the staging directory; leave
  owned entries in the old output to be replaced on the swap.
- **Do**: replace the fixed `carryOverEntries` list in
  `UseCase/Pipeline/Staging.hs` with an enumeration of the old output minus the
  owned allowlist.
- **Check**: the scenarios below.

#### Scenario: a foreign file survives a rebuild

- **WHEN** a file `NOTES.txt` is placed in `<out>` and the output is rebuilt with
  `--fresh -o <out>`
- **THEN** `NOTES.txt` is still present after the rebuild and the owned artifacts
  (`graph.json`, …) are refreshed to the new version.

#### Scenario: a foreign directory survives a rebuild

- **WHEN** a nested directory (e.g. `user-dataset/rows.csv`) is placed in `<out>`
  and the output is rebuilt with `--fresh -o <out>`
- **THEN** the directory is carried over verbatim and its contents are unchanged,
  while the owned artifacts are refreshed.

#### Scenario: unknown or hidden entries survive regardless of name

- **WHEN** arbitrary or hidden entries (`.hidden-config`, `weird-name-123.dat`)
  are present in `<out>` and the output is rebuilt
- **THEN** every entry Graphos does not regenerate survives the rebuild.

### Requirement: Replace owned regenerated artifacts on the swap

Graphos SHALL replace, on the swap, the artifacts it regenerates (`graph.json`,
`graph.checkpoint.json`, `GRAPH_REPORT.md`, `graph.html`, `community_graph.json`,
`embeddings.json`, `index.json`, `graph.cypher`, `memgraph.cypher`, `obsidian/`);
these owned entries are left in the old output and overwritten by the fresh build.

- **Plan**: the owned allowlist (`ownedArtifacts`) names these regenerable
  artifacts; `carryOverState` skips them so the swap replaces them.
- **Do**: keep `ownedArtifacts` in `UseCase/Pipeline/Staging.hs` as the short
  allowlist of Graphos-owned names.
- **Check**: a rebuild updates the owned artifacts to the fresh version.

#### Scenario: owned artifacts are refreshed on the swap

- **WHEN** the output is rebuilt with `--fresh -o <out>`
- **THEN** `graph.json`, `GRAPH_REPORT.md` and `community_graph.json` are present
  and contain the fresh run's contents (not the previous run's).

### Requirement: Refresh owned directories instead of merging stale content

Graphos SHALL not carry an owned **directory** (e.g. `obsidian/`) over verbatim,
because the staging build writes fresh files into its own copy and a verbatim
carry-over would merge the fresh build with stale files from the previous run.

- **Plan**: exclude owned directories from carry-over so the swap replaces them
  wholesale.
- **Do**: the owned allowlist excludes owned directory names from carry-over;
  they are replaced by the fresh build on the swap.
- **Check**:

#### Scenario: an owned directory is refreshed, not merged

- **WHEN** `obsidian/old.md` exists from a previous run and the next run writes
  `obsidian/new.md`
- **THEN** `obsidian/old.md` is gone and `obsidian/new.md` is present — the fresh
  build replaced the directory rather than merging with stale content.

### Requirement: Preserve persistent state as before

`cache/`, `memory/`, `debug/`, `traces/` SHALL continue to survive a rebuild,
carried over along with every other non-owned entry.

- **Check**: a rebuild leaves `cache/`, `memory/`, `debug/`, `traces/` intact.

#### Scenario: persistent state survives a rebuild

- **WHEN** `cache/`, `memory/`, `debug/` and `traces/` are present in `<out>`
  from a previous run and the output is rebuilt with `--fresh -o <out>`
- **THEN** those four persistent-state directories are still present after the
  rebuild and their contents are unchanged — they are carried over along with
  every other non-owned entry.

## Non-Goals

- Refusing to rebuild when the output holds foreign files: the non-interactive
  rebuild cannot ask the user, so preserving is the least-surprising policy (see
  design decision D1).
