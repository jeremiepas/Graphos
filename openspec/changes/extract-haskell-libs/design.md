## Context

Graphos is a single 0.1.0.0 Cabal package exposing ~110 modules across Domain,
UseCase, and Infrastructure layers. Several module clusters are self-contained,
depend on few external libraries, and fill ecosystem gaps that no existing
Hackage package covers. Keeping them private in the `graphos` package means no
other Haskell tool can reuse them and the module boundaries are informal rather
than enforced by Cabal.

The homelab repository already manages K3s-deployed self-hosted GitHub Actions
runners via `scripts/create-runner.sh`, so each new library repo can get CI
without external services.

## Goals / Non-Goals

**Goals:**
- Publish 6 reusable Haskell libraries on Hackage, each in its own public repo.
- Create a Haskell-library template + scaffolding script in the homelab.
- Wire Graphos to consume the published packages and remove duplicated source.
- Enforce clean interface boundaries via Cabal package borders.

**Non-Goals:**
- Splitting Graphos itself into multiple in-repo sub-libraries (polyrepo only).
- Publishing a binary/executable package (all 6 are libraries).
- Changing Graphos's public CLI or output formats.
- Supporting GHC versions other than 9.10 (matches Graphos's Nix shell).

## Decisions

### D1: Separate repos (polyrepo), not monorepo

Each library lives in its own public GitHub repository (`jeremiepas/<name>`)
and is published to Hackage independently. Graphos depends on Hackage versions.

- *Alternative: monorepo under Graphos `libs/`* — rejected: the user explicitly
  requested separate repos with public visibility and independent Hackage
  releases.
- *Alternative: umbrella repo with per-lib subpackages* — rejected: more
  coupling than desired; defeats the goal of independent versioning and
  discoverability on Hackage.

### D2: Shared `graphos-types` foundation package

A sixth package (`graphos-types`) holds the Node/Edge/Graph/Relation/
Confidence types. The other type-dependent packages (`graphos-leiden`,
`graphos-cypher`, `graphos-context`, `graphos-graph-export`) depend on it.

- *Alternative: each lib defines its own minimal Node/Edge record* — rejected:
  duplicated types break composability; consumers mixing libs would face
  conversion overhead.
- *Alternative: skip coupled libs* — rejected: the Leiden and Cypher packages
  are the highest-value extractions and both need the types.

### D3: Dependency extraction order

```
graphos-types          (no intra-Graphos deps)
  ├── graphos-leiden        (depends on graphos-types)
  ├── graphos-cypher        (depends on graphos-types)
  ├── graphos-context       (depends on graphos-types)
  └── graphos-graph-export   (depends on graphos-types)
graphos-lsp-extract    (no intra-Graphos deps — can proceed in parallel)
```

Phase B extracts `graphos-types`. Phase C extracts the 5 dependents (4 depend
on types, 1 is independent). This ordering prevents circular dependencies and
lets each Hackage package pin a concrete `graphos-types` version.

### D4: Template lives in homelab, not Graphos

The library template + scaffolding script are homelab assets
(`templates/haskell-lib-template/`, `scripts/create-haskell-lib.sh`) because
they serve all Haskell projects, not just Graphos.

- *Alternative: template inside Graphos* — rejected: the template is general
  infrastructure, not Graphos-specific.

### D5: CI via self-hosted K3s runners (one per repo)

Each library repo gets a GitHub Actions self-hosted runner deployed via the
existing `scripts/create-runner.sh` pattern. CI runs `cabal build` and
`cabal test` on push.

- *Alternative: GitHub-hosted runners* — rejected: the homelab already runs
  self-hosted runners for Graphos and other repos; consistency is better.
- *Alternative: single shared runner for all 6 repos* — rejected: labels
  allow per-repo isolation and independent scaling.

### D6: devenv.nix as the dev-shell format

The template uses `devenv.nix` (devenv.sh) mirroring Graphos's existing setup
(GHC 9.10, cabal, HLS, hspec-discover).

- *Alternative: plain `shell.nix`* — rejected: Graphos already uses devenv;
  consistency reduces onboarding friction.

### D7: MIT license

All 6 libraries use MIT, matching Graphos.

- *Alternative: BSD-3-Clause* — rejected: consistency with the parent project
  is preferred.
- *Alternative: Apache-2.0* — rejected: adds patent-clause complexity
  unnecessary for these small libs.

### D8: Hackage release via manual tag-triggered workflow

A `.github/workflows/hackage.yml` runs `cabal sdist` + `cabal upload` on
git tags. Releases are manual (not auto-published on every push) to keep
versioning deliberate.

## Risks / Trade-offs

- **Circular dependency between `graphos-types` and Graphos** → mitigated by
  extracting types as a leaf package with zero Graphos imports; Graphos imports
  it, never the reverse.
- **Version skew** (Graphos pinned to an older `graphos-types` while a lib
  moves ahead) → mitigated by PVP version bounds in each `.cabal` and
  deliberate tag-based releases.
- **Extraction breaks Graphos build** → mitigated by extracting in order and
  running `cabal build --flag dev && cabal test` after each wiring step.
- **Maintenance overhead of 6+1 repos** → accepted; the template + scaffold
  script reduce per-repo setup to minutes, and CI is already automated.
- **Hackage namespace collision** → `graphos-*` names are currently unused on
  Hackage (verified); `graphos` itself is not published yet either.

## Migration Plan

1. **Phase A — Template + scaffold** (homelab):
   - Create `templates/haskell-lib-template/`.
   - Create `scripts/create-haskell-lib.sh`.
   - Create 6 runner deployments via `scripts/create-runner.sh`.

2. **Phase B — `graphos-types`**:
   - Scaffold repo, copy `Domain/Types.hs` + `Types/*`.
   - Publish `graphos-types-0.1.0.0` to Hackage.

3. **Phase C — 5 dependent libs** (parallelizable):
   - Scaffold each repo, copy source modules, adapt imports to
     `graphos-types`.
   - Publish each to Hackage.

4. **Phase D — Graphos integration**:
   - Add Hackage deps to `graphos.cabal`.
   - Remove extracted source from `src/`.
   - Verify: `cabal build --flag dev && cabal test`.

## Verification Strategy

- Each library: `cabal build --flag dev && cabal test` in its own repo.
- Graphos after wiring: `cabal build --flag dev && cabal test` passes with no
  regressions (existing Hspec suite covers Community, Cypher, Context, LSP,
  Export, Types).
- Hackage: `cabal sdist` produces a clean tarball; `cabal upload --publish`
  succeeds; a fresh `cabal install graphos-leiden` in a clean shell builds
  without Graphos present.