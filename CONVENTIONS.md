# Graphos — Aider Conventions

You are working on **Graphos**, a context graph builder: any input → knowledge graph → clustered communities → HTML + JSON + report. Haskell (GHC 9.10) with Cabal, Clean Architecture (Domain ← UseCase ← Infrastructure).

## Build & test

- Enter the shell: `nix-shell shell.nix` or `devenv shell`
- Build: `cabal build` (uses `-Wall -Werror` with `--flag dev`)
- Test: `cabal test` (Hspec + QuickCheck)
- REPL: `cabal repl`
- Run: `cabal run graphos -- <path>`

Full project identity, constraints, and conventions live in `openspec/config.yaml` — read it before proposing or designing anything.

## OpenSpec PDCA workflow

This repo drives development through OpenSpec using the custom **pdca** schema (`openspec/schemas/pdca/schema.yaml` is authoritative). Specs are in `openspec/specs/`, active changes in `openspec/changes/`, archived in `openspec/changes/archive/`.

### Core commands

- `openspec list` — list active changes
- `openspec status --change <name> --json` — artifact and task status for a change
- `openspec instructions <artifact-id> --change <name> --json` — guidance to create one artifact (`proposal`, `specs`, `design`, `tasks`, `plan`, `do`, `check`, `act`)
- `openspec instructions apply --change <name> --json` — the authoritative input for implementing: context files, progress, task list, and the dynamic apply instruction
- `openspec show <name>` / `openspec validate <name>` — inspect / validate a change
- `openspec archive --change <name>` — archive a fully complete change

Never invent artifact paths or task flow: run the `openspec instructions` command and follow its `contextFiles` and `instruction` fields. Always re-read files from disk (the user may have edited them).

### Proposing a change

1. Ask what the user wants to build; derive a kebab-case change name.
2. `openspec new change "<name>"`
3. For each artifact in dependency order (proposal → specs/design → tasks), run `openspec instructions <artifact-id> --change "<name>" --json` and create the file at `resolvedOutputPath`, using `template` as structure and applying `context`/`rules` as constraints (never copy those blocks into the file).
4. `openspec status --change "<name>"` to confirm.

### Implementing (apply)

Process **ONE task at a time** (small context budget). For each task, follow the PDCA cycle:

1. Run `openspec instructions apply --change "<name>" --json`; read only the top-level files it lists (proposal, specs, design, tasks). Do not load all per-task files.
2. For the next pending task (first unchecked checkbox in `tasks.md`):
   - Read that task's `tasks/<slug>/plan.md`, `do.md`, `check.md` only.
   - If a previous task exists, read its `act.md` "Learnings / Next Steps" first.
   - **Create `plan.md`, `do.md`, and `check.md` before writing any code.** Check criteria MUST be defined in plan.md first.
   - Implement the code per `do.md`.
   - Run the checks from `check.md` verbatim against plan.md criteria; record actual results in `check.md`. Never invent or rewrite criteria.
   - Update `do.md` with deviations.
   - Write `act.md` as the final verdict (PASS/FAIL, summary, learnings).
   - Tick `N.P`, `N.D`, `N.C`, `N.A` boxes in `tasks.md` only when Check passed and Act is OK. If Act is NOT OK, keep the failed trace and start a new P→D→C→A attempt.
3. Pause and ask before moving to the next task.

Rules that override this workflow (from an explicit user choice) win; otherwise follow the apply instruction. Do not treat runtime context or guidance as proof a task is complete.
