# openspec-view — CLI usage

The `openspec-view` executable is a dependency-free (`base` / `directory` /
`time` only) surface over the `OpenSpecView` library. It reads the OpenSpec
workspace under `openspec/changes/` (and optionally `openspec/specs/`) and
prints structured views of the workspace state.

It is wired as the `openspec-view` executable component in `graphos.cabal`
(`main-is: OpenSpecView/Main.hs`, `hs-source-dirs: app`).

## Usage

```
openspec-view <changes|artifacts|diff|dashboard> [options]
openspec-view --help
```

## Global options

| Option | Default | Description |
| --- | --- | --- |
| `--changes-root <dir>` | `openspec/changes` | Directory scanned for changes. |
| `--specs-root   <dir>` | `openspec/specs` | Main specs directory used by `diff`. |
| `-h`, `--help` | | Print usage and exit 0. |

## Subcommands

### `changes [--status <state>]`

List every change with status, task counts, and last-modified timestamp. The
optional `--status` filter narrows to `in-progress`, `complete`, `archived`, or
`no-tasks`.

```
openspec-view changes
openspec-view changes --status in-progress
```

### `artifacts <change> --depth SUMMARY|FULL|SECTIONS`

Browse a single change's proposal, specs, design, and tasks. `--depth` controls
scope:

- `SUMMARY` — one line per artifact with a done/pending marker plus spec count.
- `FULL` — full contents of every present artifact.
- `SECTIONS` — full contents split into `## ` sections.

```
openspec-view artifacts openspec-view --depth SUMMARY
openspec-view artifacts openspec-view --depth FULL
```

### `diff <change>`

Compare the change's delta specs (`<change>/specs/<spec>/spec.md`) against the
main specs (`--specs-root/<spec>/spec.md`). Missing main specs are reported as an
error rather than silently treated as empty.

```
openspec-view diff openspec-view
```

### `dashboard [--status <state>]`

Aggregate per-change progress across the whole workspace, with risk highlighting
for stalled or behind changes.

```
openspec-view dashboard
openspec-view dashboard --status in-progress
```

## Build & run

```
cabal build openspec-view
cabal run openspec-view dashboard
# or, standalone:
cabal exec openspec-view -- changes
```
