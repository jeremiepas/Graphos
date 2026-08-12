# Capability: agent-scaffolding

## Purpose

Generate and install agent scaffolding files (skills and sub-agents) for supported AI coding assistants, enabling them to interact with a Graphos knowledge graph via the `graphos` CLI. Provides `graphos init --agents` to create target-appropriate configuration files idempotently.

## ADDED Requirements

### Requirement: Init flag for agent scaffolding
`graphos init` MUST accept an optional `--agents [TARGETS]` flag (PRD §13.1) where `TARGETS` is an optional comma-separated list drawn from `opencode`, `claude`, `generic`. When `--agents` is absent, `graphos init` MUST behave exactly as before (generate `graphos.yaml` only) and SHALL print a one-line hint that `--agents` exists.

#### Scenario: Init without the flag is unchanged
- **WHEN** `graphos init` is run without `--agents` in a directory with no `graphos.yaml`
- **THEN** only `graphos.yaml` is created, and the output includes a hint mentioning `--agents`

#### Scenario: Invalid target is rejected
- **WHEN** `graphos init --agents foo` is run
- **THEN** the command exits with an error naming the invalid target and listing the valid targets, and no files are written

### Requirement: Scaffold files per target
For each selected target, `graphos init --agents` MUST generate the corresponding files (PRD §13, §17):
- `opencode`: `.opencode/skills/graphos/SKILL.md` and `.opencode/agent/graphos-navigator.md`
- `claude`: `.claude/skills/graphos/SKILL.md` and `.claude/agents/graphos-navigator.md`
- `generic`: `.agents/graphos.md`

Parent directories MUST be created as needed.

#### Scenario: Explicit single target
- **WHEN** `graphos init --agents opencode` is run in a fresh directory
- **THEN** `.opencode/skills/graphos/SKILL.md` and `.opencode/agent/graphos-navigator.md` are created, and no `.claude/` or `.agents/` files are created

#### Scenario: Explicit multiple targets
- **WHEN** `graphos init --agents claude,generic` is run in a fresh directory
- **THEN** `.claude/skills/graphos/SKILL.md`, `.claude/agents/graphos-navigator.md`, and `.agents/graphos.md` are created, and no `.opencode/` files are created

### Requirement: Target auto-detection
When `--agents` is passed with no target list, the system MUST select targets by detection: include `opencode` if `.opencode/` exists, include `claude` if `.claude/` exists; if neither directory exists, all three targets MUST be selected. The detection decision MUST be implemented as a pure function over directory-existence facts (architecture-purity: no IO in Domain/UseCase).

#### Scenario: Detects existing opencode directory
- **WHEN** `graphos init --agents` is run in a directory containing `.opencode/` and no `.claude/`
- **THEN** only opencode files (plus no others) are scaffolded

#### Scenario: Nothing detected scaffolds all targets
- **WHEN** `graphos init --agents` is run in a directory with neither `.opencode/` nor `.claude/`
- **THEN** files for all three targets are created

### Requirement: Idempotent, non-destructive writes
Scaffolding MUST never overwrite an existing file. Each pre-existing scaffold path MUST be skipped with a per-file message, remaining files MUST still be written, and the command MUST exit successfully (same contract as `graphos.yaml` generation, PRD §13.1).

#### Scenario: Partial pre-existing scaffold
- **WHEN** `.opencode/skills/graphos/SKILL.md` already exists and `graphos init --agents opencode` is run
- **THEN** the skill file is left byte-identical, a skip message names it, `.opencode/agent/graphos-navigator.md` is still created, and the exit code is 0

### Requirement: Parser-derived command reference
The generated skill content MUST include a command/flag reference rendered from the actual optparse-applicative parser at generation time. Every command and flag token appearing in that reference MUST exist in the real CLI parser (PRD §13.2); the reference MUST NOT be hand-maintained prose.

#### Scenario: Reference matches the parser
- **WHEN** the skill file is generated
- **THEN** every `--flag` and subcommand token in its command reference section is present in the parser's help output (verified by cabal test)

#### Scenario: Removed flag disappears from scaffold
- **WHEN** a flag is removed from the CLI parser and the project is rebuilt
- **THEN** newly generated skill files no longer mention that flag, with no template edit required

### Requirement: Sub-agent capability contract
The generated sub-agent definition MUST restrict the sub-agent to graphos operations and read-only project access: shell access limited to `graphos *` commands, file write/edit denied. The definition MUST explicitly permit graph maintenance: full builds (`graphos <path>`), incremental refresh (`graphos . --update`), and `graphos ingest` on new files, and MUST instruct preferring `--update` and `--no-viz` for refreshes.

#### Scenario: Sub-agent frontmatter restricts tools
- **WHEN** the opencode sub-agent file is generated
- **THEN** its frontmatter denies write/edit tools and permits bash only for `graphos *` commands

#### Scenario: Sub-agent prose permits graph maintenance
- **WHEN** any target's sub-agent/agent file is generated
- **THEN** its body states the agent may build, update, and ingest into the graph, and must never modify project source files

### Requirement: YAML frontmatter on generated skills
Every generated skill file, including project-local skills created by `graphos init --agents`, MUST begin with a YAML frontmatter block containing `name` and `description` keys, followed immediately by the `generated by graphos <version>` stamp. The block SHALL use standard YAML delimiters `---` on its own lines.

#### Scenario: Project-local skill frontmatter
- **WHEN** `.opencode/skills/graphos/SKILL.md` is generated
- **THEN** its first lines are `---`, `name: graphos`, `description: ...`, `---`, then `generated by graphos <version>`

## MODIFIED Requirements

### Requirement: Version-stamped generated files
Every generated file MUST begin with a stamp identifying the generating tool and version (`generated by graphos <version>`), so future tooling can distinguish scaffolds it owns from user-authored files.

#### Scenario: Stamp present
- **WHEN** any scaffold file is generated
- **THEN** its first lines contain `generated by graphos` followed by the cabal package version

#### Scenario: Stamp follows frontmatter
- **WHEN** any scaffold skill file is generated
- **THEN** the version stamp appears immediately after the YAML frontmatter block

### Requirement: Pure scaffold planning
Scaffold planning (target set + relative paths + rendered contents) MUST be a pure function in Domain/UseCase returning a list of file plans; all filesystem effects (detection facts, existence checks, directory creation, writes, reporting) MUST live in Infrastructure (PRD §4, architecture-purity).

#### Scenario: Plan is testable without IO
- **WHEN** cabal test exercises the scaffold planner with a fixed request and command reference
- **THEN** it yields deterministic file paths and contents matching golden files, with no filesystem access

#### Scenario: Global install planning is pure
- **WHEN** the global opencode install planner is exercised with a fixed command reference
- **THEN** it yields deterministic file paths and contents matching golden files, with no filesystem access
