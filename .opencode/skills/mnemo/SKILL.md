---
name: mnemo
description: Use the mnemo memory graph correctly for this repository. Trigger whenever an agent needs to read, ingest, anchor, or query the Graphos mnemo space, or is about to run any `mnemo` command. The graph lives in the `graphos` space on Neo4j localhost:7477 — never `homelab` — and this repo has NO delivery board, so `mnemo delivery` must never be run here.
---

# Mnemo for the Graphos repository

Mnemo (harness at `/home/jeremie/Documents/perso/poc-graphos/mnemo`) keeps a memory
graph for this repo in Neo4j at `http://localhost:7477`, in the space **`graphos`** —
its own space, **never `homelab`**.

## One Neo4j, one console, many spaces

`7477` is **one** Neo4j instance and therefore **one** console
(`http://localhost:7477`) for every space: `homelab`, `graphos`, `console`, `eval`
and the rest live side by side, told apart only by the `space` property:

```
MATCH (n) WHERE n.space = 'graphos' RETURN n LIMIT 100
```

- Do **not** start a Neo4j or a console per space.
- `mnemo web` renders **exactly one space per process** and has no space switcher.
  A second view is a **second process**, never a second database.

## Always use the wrapper, never bare `mnemo`

The default `~/.config/mnemo/config.yaml` is the **homelab** space. Running bare
`mnemo` targets homelab and will corrupt the wrong space. For Graphos, always use
the wrapper, which exports `MNEMO_CONFIG=$HOME/.config/mnemo/graphos.yaml` and runs
the pinned build:

```bash
mnemo-graphos status
mnemo-graphos anchors check
mnemo-graphos web        # Graphos console → 127.0.0.1:7481
```

- Graphos console: `127.0.0.1:7481`; homelab console: `127.0.0.1:7480` — only
  because two `mnemo web` processes need two ports. The graph itself is always the
  single `7477`.
- Credentials stay in `~/.config/mnemo/env` (mode 600). No secret goes in either
  YAML, and no config is ever copied over another.

## Code graph (project `Graphos`)

The repository is pushed as project **`Graphos`** into the perso Neo4j at
`http://localhost:7474` (shared with the other perso projects).

```bash
mnemo-graphos-project Graphos /home/jeremie/Documents/perso/Graphos
```

- `poc-graphos` is pushed the same way.
- `graphos`' own `--neo4j-push` does **not** write the `project` property mnemo
  reads — always use the wrapper.

## Ingest

`mnemo-ingest-graphos` reads `.opencode/skills/*/SKILL.md`, `.opencode/commands/*.md`
and `AGENTS.md` into the `graphos` space.

```bash
mnemo-ingest-graphos
```

- Its root is the symlink mirror `~/.local/share/mnemo/ingest/graphos`. Ingesting
  the checkout directly would see the duplicate skills of `.worktrees/` and refuse.
- Run it **after editing a skill, a command, or `AGENTS.md`**; a second run is a
  no-op.

## Anchors

Anchors link memory to code.

```bash
mnemo-graphos anchors add --project Graphos <repo-relative paths>
mnemo-graphos anchors check
```

- `anchors check` re-resolves a project in **one query capped at 2,000 rows**, so
  keep the anchor set small: 36 anchors resolve cleanly, but the whole `src/` tree
  is reported falsely stale.
- Always verify with `mnemo-graphos anchors check` and expect `stale anchors: 0`.

## No delivery here — hard rule

The `graphos` config has **no `delivery` section on purpose**. The delivery board
(canvas, catalogue, workspaces) is the **solario/i2p** work in space `homelab`.

- **Never** run `mnemo delivery`, `mnemo delivery import-canvas`, or any delivery
  subcommand against `graphos`.
- Do not import Obsidian delivery canvases, do not create delivery workflows, and
  do not target `--space homelab` for anything related to this repository.

## Quick decision table

| You want to… | Use |
|---|---|
| See Graphos space state | `mnemo-graphos status` |
| Query / browse the memory graph | `mnemo-graphos web` (console on 7481) |
| Verify anchors | `mnemo-graphos anchors check` |
| Link memory to code | `mnemo-graphos anchors add --project Graphos <paths>` |
| Push the code graph | `mnemo-graphos-project Graphos <dir>` |
| Refresh skills/commands/AGENTS.md | `mnemo-ingest-graphos` |
| Any delivery/canvas/catalogue/workflow | ❌ not for this repo |

## Anti-patterns (do not repeat)

- ❌ Running bare `mnemo` (targets homelab, not graphos).
- ❌ `mnemo delivery …` for Graphos — there is no delivery board here.
- ❌ Importing a delivery canvas or workflow into `homelab` for Graphos work.
- ❌ Starting a separate Neo4j/console per space — one instance, one `space` property.
- ❌ Anchoring the whole `src/` tree (over the 2,000-row cap → falsely stale).
