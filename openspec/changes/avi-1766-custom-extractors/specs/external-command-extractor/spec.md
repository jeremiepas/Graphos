# external-command-extractor

## ADDED Requirements

### Requirement: External command extractor mode

Graphos SHALL support an extractor configured with mode: command, running an
external process per file (or batch) and ingesting stdout as graph.json-shaped
nodes and edges (PRD §3.2 extract stage).

#### Scenario: Single-file command extractor

- **WHEN** graphos.yaml maps ".pl": { mode: command, command: ["perl-graphos-extract", "{file}"] }
  and a .pl file is extracted
- **THEN** Graphos runs the command with {file} substituted, parses stdout JSON
  with nodes/edges, and accumulates them into that file's extraction

#### Scenario: Batch mode

- **WHEN** extractor sets batch: true and three files share it
- **THEN** Graphos invokes command once for all files and routes each returned
  node/edge by source file

### Requirement: Validation and id prefixing

Graphos SHALL validate command output against graph.json node/edge schema, prefix
node ids with directory bucket when missing, drop invalid records (not fatal),
and fall back to stub for that file.

#### Scenario: Invalid JSON becomes warning not abort

- **WHEN** a command emits malformed JSON for x.pl
- **THEN** build continues, x.pl stub-extracted, per-file error recorded

#### Scenario: Id prefixing prevents collisions

- **WHEN** two files both emit node id fn:main
- **THEN** merged graph contains two distinct prefixed ids

### Requirement: Per-file error recording

Graphos SHALL record a per-file error for non-zero exit, timeout, or invalid JSON.
One file's failure SHALL NOT abort build or other files.

#### Scenario: Command failure does not abort build

- **WHEN** extraction for a.pl fails but b.pl succeeds
- **THEN** build completes, a.pl stub-extracted with recorded error, b.pl contributes

### Requirement: Command extractor caching parity

Successful command extraction SHALL be cached and fingerprinted so unchanged files
skip re-invocation and config changes invalidate stale cache.

#### Scenario: Cached command extraction reused

- **WHEN** a .pl file unchanged between runs
- **THEN** second run loads cache and does not invoke command

### Requirement: Security guard for command extractors

Graphos SHALL run only declared argv commands (no shell), apply ignore/sensitive
filtering, and gate command extractors behind allow_command_extractors
(default true CLI, false library).

#### Scenario: Sensitive file not extracted by command

- **WHEN** a file is sensitive/ignored
- **THEN** no command process spawned and file skipped
