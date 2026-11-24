# openspec-view

Standalone, `base`-only package that reads an [OpenSpec](https://github.com/Fission-AI/OpenSpec)
workspace and renders it on a terminal:

| Module | What it does |
|---|---|
| `OpenSpecView.ChangeList` | scans `openspec/changes/*/tasks.md`, counts checkboxes, reports status |
| `OpenSpecView.ArtifactView` | loads and renders a change's proposal, design, tasks, and delta specs |
| `OpenSpecView.SpecDiff` | parses requirement blocks and diffs a delta spec against its main spec |
| `OpenSpecView.StateDashboard` | aggregates changes into totals, progress, and risk highlights |

The `graphos view` subcommand is the only consumer; `app/Main.hs` imports the
four modules directly.

## Build & test

```bash
cabal build openspec-view
cabal test  openspec-view-test
```

The disk-scanning specs need an `openspec/changes` tree somewhere above the
working directory. When none is found (for example when the package is checked
out on its own) those examples are marked pending instead of failing; the pure
parsing/diffing/dashboard examples always run.

## Provenance

Extracted from the Graphos repository as part of the `split-graphos-studio`
OpenSpec change (decision D3): OpenSpec artifact rendering does not belong in
the knowledge-graph engine, and the sub-library was the last in-repo coupling
between the CLI and the workspace tooling. The dependency is `base` +
`directory` + `time`; the test suite adds `hspec` only.
