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
| `GraphosFormalization/Ext/Basic.lean` | Phase 2 objects: `Extraction`, `mergeExtractions`, `emptyExtraction`, `foldMerge`, consistency, `ViewEmbedding` |
| `GraphosFormalization/Ext/Colimit.lean` | Phase 2 universal property: `mergedExtraction_isColimit` (AVI-620 §2) |

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

Phase 2 colimit layer in progress, authored under the requirements-layer
authorship decomposition (AVI-948) with per-module requirement citations.

| Artifact (AVI-620) | Lean name | State |
| --- | --- | --- |
| §2 colimit universal property | `mergedExtraction_isColimit` | **proved** (`Ext/Colimit.lean`) |
| §2 least-upper-bound form | `mergedExtraction_isColimit_iff` | **proved** |
| §3.1 strict colimit + order independence | `mergedExtraction_strictColimit_orderIndependent` | pending (AVI-1303) |
| §3.2 lax colimit / first-wins | `mergedExtraction_laxColimit_oldWins` | pending (AVI-1304) |
| §4 n-way fold complexity | `mergedExtraction_nwayFold_linear` | pending (AVI-1306) |
| Phase 1 auditable modularity `Q` (INV-Q0..Q5) | `modularity_*` | pending (AVI-1293..1298) |

The colimit layer carries the consistency hypothesis explicitly
(`familyConsistent`), so it stays valid regardless of how the AVI-949
feasibility answers land: inconsistent families satisfy its premise only when
they genuinely agree on every shared key.
