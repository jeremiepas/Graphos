# atomic-output-writes Specification

## Purpose
TBD - created by archiving change atomic-graph-output-writes. Update Purpose after archive.
## Requirements
### Requirement: Atomic artifact writes

The system SHALL write each primary output artifact to a temporary file in the
target directory and atomically rename it into place, so a reader never observes
a partially written artifact.

#### Scenario: interrupted write leaves prior file intact
- **WHEN** a run is interrupted while writing `graph.json`
- **THEN** the previous `graph.json` remains valid and complete
- **AND** no partially written `graph.json` is present at the final path

#### Scenario: successful write replaces atomically
- **WHEN** `graph.json` is written successfully
- **THEN** the final file is the complete new content with no intermediate truncation observable

### Requirement: Staged rebuild swap

The system SHALL perform a full rebuild in a staging location and swap it into the
output directory only after all artifacts are written successfully.

#### Scenario: failed rebuild preserves existing output
- **WHEN** a full rebuild fails partway through
- **THEN** the existing `graphos-out/` contents are unchanged

### Requirement: Preserve non-regenerated files across staged rebuilds

The system SHALL preserve, across a staged full rebuild, every entry in the
output directory that Graphos does not regenerate. Graphos-owned artifacts —
`graph.json`, `graph.checkpoint.json`, `pipeline.checkpoint.json`,
`GRAPH_REPORT.md`, `graph.html`, `community_graph.json`, `embeddings.json`, and
any other name listed in `ownedArtifacts` — are refreshed by the atomic swap;
every other entry, including user files, hidden files, and unknown directories,
is carried into staging before the rename and survives the rebuild unchanged.
Owned directories (such as `obsidian/`) are fully replaced by the fresh build
rather than merged with the previous run's contents. A carried-over entry never
replaces an artifact the fresh build already wrote under the same name: the
staged copy wins.

#### Scenario: foreign file survives a rebuild
- **WHEN** a full rebuild runs against an output directory that already contains a file Graphos does not own
- **THEN** that file is preserved with its original content after the rebuild
- **AND** the owned artifacts are refreshed to the new build's content

#### Scenario: owned directory is replaced, not merged
- **WHEN** a full rebuild regenerates an owned directory such as `obsidian/`
- **THEN** the new build's contents fully replace the previous directory
- **AND** stale entries left by the prior run are not present in the output

### Requirement: Startup graph validation

The system SHALL validate an existing `graph.json` before use and fail with a
clear, actionable message if it is corrupt.

#### Scenario: corrupt graph reported clearly
- **WHEN** `graph.json` is truncated or invalid JSON
- **THEN** the tool reports the corruption and the path
- **AND** suggests recovering from the checkpoint or rebuilding

#### Scenario: valid graph loads normally
- **WHEN** `graph.json` is well-formed
- **THEN** it loads without warnings

