# gitignore-parsing Capability — Delta

## Purpose

Stop treating any directory named `build` (or `out`, `target`, `dist`) as a build artifact at
arbitrary depth. On a real repository this silently dropped 85 source files under
`src/domain/build/`, `src/services/phase/build/` and `src/lib/build/` — 6.6% of the source tree —
with no way to override, because `UseCase/Detect.hs:180` tests the hardcoded list with `elem`
on the directory basename *before* any pattern (and therefore any negation) is consulted.

## ADDED Requirements

### Requirement: Build-output directory names are anchored to the scan root

Build-output directory names SHALL be pruned only at the scan root, never at arbitrary depth.
The anchored set is `build`, `out`, `target`, `dist`, `dist-newstyle`, `DerivedData`, `.build`;
each SHALL be pruned only when it occurs as a direct child of the scan root, and SHALL NOT be
pruned when nested inside a source tree. Names that denote tooling or VCS state —
`node_modules`, `.git`, `.stack-work`, `.cache`, `__pycache__` and equivalents — keep
depth-independent matching.

#### Scenario: Root build directory is pruned

- **WHEN** a repository contains `./build/output.js` and the scan root is `.`
- **THEN** `./build/output.js` is not extracted

#### Scenario: Nested source directory named build is extracted

- **WHEN** a repository contains `./src/domain/build/build-ledger.ts` and
  `./src/services/phase/build/build-pipeline-executor.ts`
- **THEN** both files are extracted and appear in `graph.json`

#### Scenario: node_modules stays depth-independent

- **WHEN** a repository contains `./packages/app/node_modules/left-pad/index.js`
- **THEN** the file is not extracted

#### Scenario: Coverage is measurable

- **WHEN** the pipeline runs on a repository with 1,291 source files and no user ignore rules
  beyond `.gitignore`
- **THEN** the count of source files present on disk but absent from `graph.json` is zero for
  paths not matched by a root-anchored or depth-independent ignore rule

### Requirement: Hardcoded ignore names are overridable by negation patterns

The fast-path hardcoded directory-name check SHALL consult negation patterns from
`.graphosignore`/`.gitignore` before pruning, so a negation such as `!src/**/build/**` re-includes
a directory that the hardcoded list would otherwise remove. The hardcoded list SHALL be the
lowest-priority layer, consistent with the existing priority order (hardcoded 0, gitignore 1,
graphosignore 2).

#### Scenario: Negation re-includes a hardcoded-ignored directory

- **WHEN** `.graphosignore` contains `!dist/keep/**` and the repository contains
  `./dist/keep/a.ts`
- **THEN** `./dist/keep/a.ts` is extracted

#### Scenario: Without negation the default still applies

- **WHEN** no negation pattern matches and the repository contains `./dist/bundle.js`
- **THEN** `./dist/bundle.js` is not extracted

### Requirement: Ignored path accounting is reported

The detect stage SHALL report the number of paths excluded, grouped by the rule class that
excluded them (root-anchored build output, depth-independent tooling, `.gitignore`,
`.graphosignore`), so that missing files are explainable without re-running the scan.

#### Scenario: Report explains exclusions

- **WHEN** the pipeline completes on a repository where 85 files were excluded by a
  depth-independent rule and 12 by `.gitignore`
- **THEN** the run report contains per-class exclusion counts, and the classes sum to the total
  excluded count
