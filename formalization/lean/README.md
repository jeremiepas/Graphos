# Graphos Lean formalization (`formalization/lean/`)

Lean 4 **verification companion** for the Graphos context graph. This package is
built independently of the Haskell project: it is **not** a cabal component, has
**no** FFI, and no runtime coupling with the `graphos` executable.

- Requirements: [`docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`](../../docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md)
- Board scope: Phase 1 + 2 + 3 · CI posture **report-only** · maintenance owner **R&D cell**
- Toolchain: pinned in [`lean-toolchain`](./lean-toolchain) (`leanprover/lean4:v4.14.0`); `elan`/`lake` read it automatically.

## Layout

| Path | Purpose |
| --- | --- |
| `lakefile.toml` | Lake package `GraphosFormalization`, no dependencies |
| `lean-toolchain` | Pinned Lean 4 toolchain |
| `GraphosFormalization.lean` | Root module (library entry point) |
| `GraphosFormalization/Basic.lean` | Package skeleton / build smoke check |

## Build

```bash
cd formalization/lean
lake build
```

`elan` downloads the pinned toolchain on first run. `.lake/` is a build directory
and is gitignored.

## CI posture

`.github/workflows/lean.yml` runs `lake build` with `continue-on-error: true`.
The job surfaces results on the PR but **never blocks a merge** — a failing Lean
build is a signal to the R&D cell, not a release gate.

## Status

Scaffolding only. The named results (Phase 1 auditable modularity `Q`, Phase 2
merged context graph as colimit) are authored under the requirements-layer
authorship decomposition and arrive as their own modules with requirement
citations.
