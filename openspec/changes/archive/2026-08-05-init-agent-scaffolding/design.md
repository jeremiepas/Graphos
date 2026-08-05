# Design: init-agent-scaffolding

## Context

`graphos init` currently generates a single file, `graphos.yaml`, via `initConfigFile` in `app/Main.hs` (a plain `IO ()` helper with an embedded string template and an exists-check). Agent integration (skill for opencode/Claude Code, sub-agent definitions) is maintained by hand outside the repo and drifts from the real CLI.

This change extends init to scaffold agent integration files for three targets. Constraints:

- Clean architecture: Domain has zero IO, UseCase has zero IO implementation, all file writes in Infrastructure.
- The generated skill's command reference must be derived from the actual optparse-applicative parser, never hand-copied.
- Sequenced after `improve-query-agent-ergonomics`: skill prose is written against the post-ergonomics query CLI.

```
graphos init --agents [targets]
        │
        ▼
┌─────────────────────────────────────────────────────────┐
│ Domain: ScaffoldTarget, ScaffoldFile, render plan (pure)│
│ UseCase: plan scaffolding (targets → [ScaffoldFile])    │
│ Infra:   detect targets, write files, report            │
└─────────────────────────────────────────────────────────┘
        │
        ▼
.opencode/skills/graphos/SKILL.md   .opencode/agent/graphos-navigator.md
.claude/skills/graphos/SKILL.md     .claude/agents/graphos-navigator.md
.agents/graphos.md
```

## Goals / Non-Goals

**Goals:**
- `graphos init --agents` generates valid skill + sub-agent files for opencode, Claude Code, and the generic `.agents/` convention.
- Sub-agent is empowered: may run all `graphos` commands (query/path/explain, full builds, `--update`, `ingest`) but no code-editing tools.
- Command/flag reference inside generated skills is rendered from the optparse-applicative parser at generation time.
- Idempotent: existing files are never overwritten; clear created/skipped reporting; version stamp in every generated file.
- Plain `graphos init` behavior unchanged except for a one-line hint about `--agents`.

**Non-Goals:**
- MCP registration (editing `opencode.json` / `.mcp.json`) — future cycle.
- Refreshing/upgrading previously scaffolded files (`graphos agents update`) — future cycle.
- Global (per-user `~/.config`) installation — project-local only.
- Windows path conventions beyond what `filepath` already normalizes.

## Decisions

### D1 — Extend `init` with a flag, not a new top-level command

| Option | Notes |
|--------|-------|
| `graphos init --agents [t1,t2]` (chosen) | One scaffolding entry point; discoverable in `init` help; additive |
| New `graphos agents install` command | More surface area; splits scaffolding across two commands |
| Always scaffold on `init` | Surprising: drops 5 files into dotdirs unasked |

Chosen: `--agents` optional flag taking an optional comma-separated target list (`opencode,claude,generic`). No value ⇒ auto-detect (D2). Plain `init` prints a hint line.

### D2 — Target selection: explicit list wins, else detect, else all

| Option | Notes |
|--------|-------|
| Explicit list overrides; bare `--agents` detects existing `.opencode/` / `.claude/` dirs; nothing detected ⇒ write all three (chosen) | Predictable, zero-question UX, still controllable |
| Always write all three | Litters projects that only use one runtime |
| Interactive prompt | Breaks scripting/CI use of init |

Detection is a pure decision over directory-existence facts gathered in Infrastructure (facts in, plan out — keeps UseCase pure).

### D3 — Layering: pure scaffold plan, IO only at the edge

| Layer | Responsibility |
|-------|----------------|
| Domain (`Domain.Scaffold`) | `ScaffoldTarget` (Opencode/Claude/Generic), `ScaffoldFile` (relative path + content), template substitution types |
| UseCase (`UseCase.Scaffold`) | `planScaffold :: ScaffoldRequest -> CommandReference -> [ScaffoldFile]` — pure mapping of targets to rendered files |
| Infrastructure (`Infrastructure.Scaffold.Writer`) | Detect dirs, check existence, create parents, write files, print created/skipped report |

Alternative considered: keep everything in `app/Main.hs` like `initConfigFile`. Rejected — five files, three formats, and render logic is real behavior that deserves tests; also the existing pattern is already flagged by `refactor-architecture-ports-and-split-god-modules` as a smell to avoid growing.

### D4 — Templates: embedded data files, not inline string literals

| Option | Notes |
|--------|-------|
| `data-files` / `file-embed` of markdown templates with `{{placeholders}}` (chosen: `file-embed` at compile time) | Templates are readable markdown in-repo; binary stays self-contained; no runtime path lookup |
| Inline Haskell string lists (current `defaultConfigYaml` style) | Unreadable for ~80-line markdown; painful diffs |
| Runtime `data-files` lookup | Breaks single-binary installs (`cabal install graphos`) |

Placeholders: `{{VERSION}}`, `{{COMMAND_REFERENCE}}`, `{{GENERATED_STAMP}}`. Substitution is a pure Domain function (no template engine dependency).

### D5 — Parser-derived command reference

| Option | Notes |
|--------|-------|
| Render optparse-applicative `parserHelp`/usage for top-level + subcommands into a fenced block injected at `{{COMMAND_REFERENCE}}` (chosen) | Cannot document flags that don't exist; zero maintenance |
| Static hand-written reference | The exact staleness problem motivating this change |
| Full skill generated from parser metadata | Loses the curated prose (decision flow, caveats) that makes the skill effective |

Hybrid: curated prose (when to query vs build, BFS vs DFS guidance, rules) stays in templates; the mechanical flag/command reference is generated. Requires exposing the parser definition from a module both `app/Main.hs` and the scaffold path can use (moves the `Parser` value out of `Main` into an app-adjacent module — consistent with the ports refactor direction).

### D6 — Sub-agent permissions: graphos-empowered, edit-restricted

Generated sub-agent frontmatter (per target's format) grants:
- bash restricted to `graphos *` (allow) / everything else deny (opencode `permission` syntax; Claude Code `tools` list without Edit/Write where expressible)
- read allowed; write/edit denied
- explicit prose instruction: may build (`graphos <path>`), refresh (`--update`), and `ingest` new files; must never modify project source.

Alternative (read-only navigator) rejected per product decision: the sub-agent should keep the graph fresh autonomously.

### D7 — Idempotency and stamping

Same contract as `graphos.yaml`: per-file exists-check ⇒ skip with message; never overwrite; exit success either way. Every generated file begins with an HTML comment stamp `generated by graphos {{VERSION}}` so a future `agents update` can identify scaffolds it owns.

## Risks / Trade-offs

- [opencode/Claude Code frontmatter formats drift] → Templates versioned in-repo; golden tests catch our regressions; format changes are template-only fixes (no Haskell changes); Act step feeds drift into next cycle.
- [Generated reference bloats skill and burns agent tokens] → Render usage lines only (not full help text); budget the reference block in golden tests (< ~60 lines).
- [`improve-query-agent-ergonomics` changes CLI mid-flight] → Hard sequencing: template prose authored after that change's query contract lands; the parser-derived reference absorbs flag changes automatically.
- [Bare `--agents` writes all three targets in a fresh repo] → Documented behavior + explicit list escape hatch; files are inert markdown if a runtime is absent.
- [Sub-agent allowed to run full builds could be slow/expensive in huge repos] → Template prose instructs preferring `--update` and `--no-viz`; graph freshness is best-effort.
- [Exposing parser outside Main creates coupling] → Small, mechanical extraction; aligns with the in-flight architecture refactor rather than fighting it.

## Verification Strategy (Check)

- `cabal build` clean under `-Wall -Werror` (dev flag).
- `cabal test` (Hspec):
  - **Golden tests**: `planScaffold` output for each target matches checked-in golden files (all five paths + contents).
  - **Consistency property**: every `--flag`/command token appearing in the generated `{{COMMAND_REFERENCE}}` exists in the real parser help (and spot-check the reverse for key commands: query, path, explain, ingest, init).
  - **Idempotency**: Infrastructure writer given pre-existing files skips all, writes none, reports correctly (temp-dir test).
  - **Purity**: `planScaffold` in UseCase/Domain compiles with no IO imports (enforced by architecture-purity spec conventions).
- Manual acceptance: run `graphos init --agents` in a scratch project; confirm opencode lists the skill and sub-agent; confirm Claude Code loads the skill; sub-agent successfully answers a question via `graphos query` and refreshes via `--update`.

## Iteration & Rollback (Act)

- Rollback is trivial: the feature is additive behind a flag; reverting the change restores prior `init` behavior; scaffolded files in user projects are inert markdown that users can delete.
- If Check reveals a target format is wrong: fix template, bump stamp, re-run golden tests — no architectural rework.
- Learnings to standardize for next cycles: (a) template + parser-derived-reference pattern becomes the standard for any future generated docs; (b) collected friction feeds the future `graphos agents update` + MCP-registration change; (c) if detection heuristics annoy users, revisit D2 with real feedback.

## Open Questions

- Exact opencode sub-agent directory: docs use `.opencode/agent/` (singular); verify current opencode version also accepts it before freezing the template path.
- Should the generic `.agents/graphos.md` include frontmatter or stay plain markdown? Default: minimal frontmatter (name/description) — harmless if ignored.
