# Capability: agent-scaffolding

## Purpose

Generate and install agent scaffolding files (skills and sub-agents) for supported AI coding assistants, enabling them to interact with a Graphos knowledge graph via the `graphos` CLI. Provides `graphos init --agents` to create target-appropriate configuration files idempotently.
## Requirements
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

The generated skill content MUST include a command/flag reference rendered from the actual optparse-applicative parser at generation time. Every command and flag token appearing in that reference MUST exist in the real CLI parser (PRD §13.2); the reference MUST NOT be hand-maintained prose. The skill content MUST also include a "Tool: graphos CLI" section that names the `graphos` binary as the tool backing the skill, names `graphos-out/graph.json` as the graph location, and warns that the unrelated `graphify` Python tool (which writes to `graphify-out/`) is a different tool and MUST NOT be used.

#### Scenario: Reference matches the parser

- **WHEN** the skill file is generated
- **THEN** every `--flag` and subcommand token in its command reference section is present in the parser's help output (verified by cabal test)

#### Scenario: Skill names graphos CLI and graphos-out

- **WHEN** the skill file is generated
- **THEN** its body contains a section naming the `graphos` CLI as the tool, names `graphos-out/graph.json` as the graph location, and explicitly states the `graphify` Python tool is a different tool and must not be used

#### Scenario: Removed flag disappears from scaffold

- **WHEN** a flag is removed from the CLI parser and the project is rebuilt
- **THEN** newly generated skill files no longer mention that flag, with no template edit required

### Requirement: Sub-agent capability contract

The generated sub-agent definition MUST restrict the sub-agent to graphos operations and read-only project access: shell access limited to `graphos *` commands, file write/edit denied. The definition MUST explicitly permit graph maintenance: full builds (`graphos <path>`), incremental refresh (`graphos . --update`), and `graphos ingest` on new files, and MUST instruct preferring `--update` and `--no-viz` for refreshes. The sub-agent prose MUST declare the `graphos` CLI as the agent's primary toolset by listing the `graphos` subcommands the agent uses (`query`, `path`, `explain`, `symbols`, `neighbors`, `ingest`, plus the build/refresh forms), MUST name `graphos-out/` as the output directory (not `graphify-out/`), and MUST instruct the agent to use the `graphos` CLI exclusively — never Python, never the unrelated `graphify` tool, never `graphos_*` MCP tools (which are not registered in this environment).

- Plan: stop the agent from reaching for Python `graphify` or non-existent MCP tools by making the `graphos` CLI the explicit, named toolset.
- Do: update `opencodeNavigatorProse` and `claudeNavigatorProse` in `Scaffold.hs` to add the toolset declaration and the anti-guardrails.
- Check: the scenarios below verify the regenerated agent file names `graphos` CLI, `graphos-out/`, and warns against Python/`graphify`/MCP.
- Act: if the model still drifts, the next cycle moves the global `~/.claude/skills/graphify/` skill out of the way (out of scope here).

#### Scenario: Sub-agent frontmatter restricts tools

- **WHEN** the opencode sub-agent file is generated
- **THEN** its frontmatter denies write/edit tools and permits bash only for `graphos *` commands

#### Scenario: Sub-agent prose declares graphos CLI as toolset

- **WHEN** any target's sub-agent/agent file is generated
- **THEN** its body lists the `graphos` subcommands (`query`, `path`, `explain`, `symbols`, `neighbors`, `ingest`, build/refresh) as the agent's tools, names `graphos-out/` as the output directory, and states the agent uses the `graphos` CLI exclusively

#### Scenario: Sub-agent prose forbids Python, graphify, and MCP tools

- **WHEN** any target's sub-agent/agent file is generated
- **THEN** its body instructs the agent NOT to use Python, NOT to use the unrelated `graphify` tool (which writes to `graphify-out/`), and NOT to call `graphos_*` MCP tools (not registered in this environment)

#### Scenario: Sub-agent prose permits graph maintenance

- **WHEN** any target's sub-agent/agent file is generated
- **THEN** its body states the agent may build, update, and ingest into the graph, and must never modify project source files

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

### Requirement: YAML frontmatter on generated skills
Every generated skill file, including project-local skills created by `graphos init --agents`, MUST begin with a YAML frontmatter block containing `name` and `description` keys, followed immediately by the `generated by graphos <version>` stamp. The block SHALL use standard YAML delimiters `---` on its own lines.

#### Scenario: Project-local skill frontmatter
- **WHEN** `.opencode/skills/graphos/SKILL.md` is generated
- **THEN** its first lines are `---`, `name: graphos`, `description: ...`, `---`, then `generated by graphos <version>`

### Requirement: Global skill named `graphos`, not `graphify`

The user-installed global skill at `~/.claude/skills/` SHALL be named `graphos` (directory `~/.claude/skills/graphos/`, `SKILL.md` `name:` frontmatter `graphos`, trigger `/graphos`), not `graphify`. The skill body and `references/` SHALL reference `graphos-out/` as the output directory, not `graphify-out/`. The `~/.claude/CLAUDE.md` integration note SHALL reference `~/.claude/skills/graphos/SKILL.md` and the `/graphos` trigger. This is a one-time rename of the user-environment files (not produced by the repo scaffold); the repo scaffold does NOT write to `~/.claude/skills/`.

- Plan: eliminate the name collision that causes the model to pick the wrong (`graphify`) skill.
- Do: `mv ~/.claude/skills/graphify ~/.claude/skills/graphos`; update `SKILL.md` `name:` and trigger; replace `graphify-out/` → `graphos-out/` in the skill body and `references/`; update `~/.claude/CLAUDE.md`.
- Check: the scenarios below verify the rename.

#### Scenario: Global skill directory renamed

- **WHEN** this change is applied
- **THEN** `~/.claude/skills/graphos/SKILL.md` exists and `~/.claude/skills/graphify/` does not exist

#### Scenario: Global skill name and trigger are graphos

- **WHEN** `~/.claude/skills/graphos/SKILL.md` is read after the rename
- **THEN** its `name:` frontmatter is `graphos` (not `graphify`) and its trigger is `/graphos` (not `/graphify`)

#### Scenario: Global skill references graphos-out

- **WHEN** `~/.claude/skills/graphos/SKILL.md` and its `references/` are read after the rename
- **THEN** they reference `graphos-out/` as the output directory, and no `graphify-out/` path remains

#### Scenario: CLAUDE.md references graphos

- **WHEN** `~/.claude/CLAUDE.md` is read after the rename
- **THEN** it references `~/.claude/skills/graphos/SKILL.md` and the `/graphos` trigger, and contains no `graphify` reference

### Requirement: No stale non-generated agent files contradicting the scaffold

The scaffolded agent files (`.opencode/agent/graphos-navigator.md`, `.claude/agents/graphos-navigator.md`) SHALL be the single source of truth for the Graphos agent in a repo. Any pre-existing agent file in the same directory that is NOT produced by `Scaffold.hs` (no `generated by graphos` stamp) AND references tools not registered in the environment (e.g., `graphos_*` MCP tools) SHALL be removed when `graphos init --agents` regenerates the scaffold, because it misdirects the model to call non-existent tools. This removal SHALL be a one-time cleanup performed as part of this change (manual deletion of `.opencode/agent/explore_graphos.md`); the scaffold itself does NOT auto-delete user-authored agent files (that would violate the idempotent-non-destructive rule for user content).

- Plan: remove the MCP-tool misdirection by deleting the stale `explore_graphos.md`.
- Do: delete `.opencode/agent/explore_graphos.md` (no stamp, references unregistered MCP tools); do not add auto-deletion logic to the scaffold.
- Check: the file no longer exists; the scaffolded `graphos-navigator.md` is the only Graphos agent in `.opencode/agent/`.

#### Scenario: Stale MCP-tool agent removed

- **WHEN** this change is applied
- **THEN** `.opencode/agent/explore_graphos.md` no longer exists, and `.opencode/agent/graphos-navigator.md` is the only Graphos-named agent file in `.opencode/agent/`

#### Scenario: Scaffold does not auto-delete user files

- **WHEN** `graphos init --agents opencode` is run in a repo with a user-authored `.opencode/agent/my-custom-agent.md`
- **THEN** that file is left untouched (the scaffold only writes its own target paths and never deletes user content)

