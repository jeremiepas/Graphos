# OpenSpec conventions — artifact linking & state tracking

This change (`openspec-view`) reads the existing OpenSpec workspace rather than
introducing new schema types. The conventions below describe how changes,
artifacts, and state are laid out and how `openspec-view` interprets them. New
changes should follow these so the view layer stays accurate.

## Directory layout

```
openspec/
  changes/
    <change-slug>/
      proposal.md          # why + what + capabilities
      design.md            # context, decisions, risks
      tasks.md             # checklist of implementation tasks
      specs/
        <spec-name>/
          spec.md          # delta spec (added/changed vs. main)
  specs/                   # MAIN specs — the current merged state
    <spec-name>/
      spec.md              # baseline that delta specs diff against
```

- `openspec/changes/<slug>/` holds a single change's working artifacts.
- `openspec/changes/<slug>/specs/<spec>/spec.md` is the **delta spec**: what
  this change adds or modifies.
- `openspec/specs/<spec>/spec.md` is the **main spec**: the current merged
  baseline. `diff` compares each delta against it.

## Artifact types and presence markers

Each change has up to four artifact kinds. `openspec-view` marks each as
`done` when the file exists and is non-empty, else `pending`:

| Artifact | File | Shown by |
| --- | --- | --- |
| Proposal | `proposal.md` | `artifacts`, `dashboard` |
| Design | `design.md` | `artifacts` |
| Tasks | `tasks.md` | `artifacts` |
| Specs | `specs/<name>/spec.md` | `artifacts`, `diff` |

## State tracking

A change's **status** is derived from its task list (`tasks.md`):

| Status | Meaning | Derived from |
| --- | --- | --- |
| `in-progress` | has tasks and at least one is incomplete | any `[ ]` task |
| `complete` | all tasks checked off `[x]` | no remaining tasks |
| `no-tasks` | task list has no items | empty task section |
| `archived` | explicitly archived | `openspec/changes/archive/<slug>/` |

`openspec-view changes --status <state>` filters on these; `dashboard` aggregates
counts and flags changes as **stalled** (no task movement) or **behind**
(partially complete).

## Artifact linking

When referencing another change or spec from markdown, link by slug so the view
and humans can resolve it:

- Change: `openspec/changes/<slug>/` (or `<slug>` when unambiguous).
- Spec delta: `openspec/changes/<slug>/specs/<spec-name>/spec.md`.
- Main spec: `openspec/specs/<spec-name>/spec.md`.

## Rules for new changes

1. Create `openspec/changes/<slug>/` with `proposal.md`, `design.md`, and
   `tasks.md`.
2. Add one `specs/<spec-name>/spec.md` per capability the change introduces.
3. Keep `tasks.md` as the single source of truth for status — check off `[x]` as
   you finish work; the view derives state from it.
4. When finished, move the change under `openspec/changes/archive/<slug>/` and
   fold its delta specs into `openspec/specs/`.

These conventions are read-only with respect to existing artifacts — the view
layer never modifies them, it only aggregates what is on disk.
