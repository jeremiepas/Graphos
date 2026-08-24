# Gitignore Parsing

## Purpose

gitignore-parsing capability.

## Requirements
### Requirement: Wildcard Pattern Parsing
The `parsePattern` function SHALL correctly handle `*` wildcards in any position within a `.gitignore` pattern line, producing a `WildcardPattern` that matches zero or more characters within a single path segment.

- **Plan**: Patterns containing `*` (e.g., `.ghc.environment.*`, `result-*`) must match the intended file paths, not be misclassified as suffix or exact patterns.
- **Do**: Add a `WildcardPattern` constructor to `IgnorePattern`. Restructure `parsePattern` to check for `*` before applying other heuristics. Implement `fnmatch`-style matching where `*` matches any characters except `/`.
- **Check**: Scenarios below verify each wildcard position.
- **Act**: If edge cases emerge (e.g., patterns with multiple `*`), add targeted tests and refine the matcher.

#### Scenario: Leading wildcard matches file extension
- **WHEN** `parsePattern "*.log"` is called
- **THEN** it SHALL produce `WildcardPattern "*.log"` (backward-compatible with existing `GlobPattern` semantics for `*.ext` patterns, but using the new constructor)

#### Scenario: Trailing wildcard matches prefix
- **WHEN** `parsePattern "result-*"` is called
- **THEN** it SHALL produce `WildcardPattern "result-*"` and `shouldIgnore [AnnotatedPattern (WildcardPattern "result-*") False 0] "result-1"` SHALL return `True`

#### Scenario: Middle wildcard matches infix
- **WHEN** `parsePattern ".ghc.environment.*"` is called
- **THEN** it SHALL produce `WildcardPattern ".ghc.environment.*"` and `shouldIgnore [AnnotatedPattern (WildcardPattern ".ghc.environment.*") False 0] ".ghc.environment.x86_64-linux"` SHALL return `True`

#### Scenario: Wildcard does not match across path separators
- **WHEN** `shouldIgnore [AnnotatedPattern (WildcardPattern "result-*") False 0] "some/path/result-1"` is called
- **THEN** it SHALL return `True` (single-segment `*` matches within path components that contain the pattern)

#### Scenario: Multiple wildcards in one pattern
- **WHEN** `parsePattern "foo*bar*baz"` is called
- **THEN** it SHALL produce `WildcardPattern "foo*bar*baz"` and `shouldIgnore` SHALL match paths containing `fooXbarYbaz` where X and Y are any non-`/` characters

### Requirement: Double-Star Recursive Matching
The `matches` function SHALL handle `**` in `WildcardPattern` as a recursive directory wildcard that matches zero or more path segments including `/`.

- **Plan**: Patterns like `!.opencode/agent/**` must re-include all files recursively under `.opencode/agent/`.
- **Do**: In the wildcard matcher, treat `**` as matching any sequence of characters including `/`.
- **Check**: Negation with `**` correctly overrides lower-priority ignores.

#### Scenario: Double-star matches nested paths
- **WHEN** `shouldIgnore [AnnotatedPattern (PrefixPattern ".opencode") False 0, AnnotatedPattern (WildcardPattern ".opencode/agent/**") True 2] ".opencode/agent/core/openagent.md"` is called
- **THEN** it SHALL return `False` (negation pattern with `**` overrides the ignore)

#### Scenario: Single star does not match across directories
- **WHEN** `shouldIgnore [AnnotatedPattern (WildcardPattern "*.log") False 0] "src/app/debug.log"` is called
- **THEN** it SHALL return `True` (`*.log` matches the filename component)

### Requirement: Dot-Filename Exact Match
The `parsePattern` function SHALL classify filenames containing dots but no wildcards (e.g., `cabal.project.local`, `.DS_Store`) as `ExactPattern`, not `SuffixPattern`.

- **Plan**: Filenames like `cabal.project.local` should match by name, not as a suffix of arbitrary strings.
- **Do**: Restructure `parsePattern` classification so that dot-containing strings without `*` that don't end with `/` are classified as `ExactPattern`.
- **Check**: Existing tests for suffix patterns still pass; new exact-match tests for dot-filenames pass.

#### Scenario: Dot-filename parsed as exact match
- **WHEN** `parsePattern "cabal.project.local"` is called
- **THEN** it SHALL produce `ExactPattern "cabal.project.local"`

#### Scenario: Dot-filename matches by name
- **WHEN** `shouldIgnore [AnnotatedPattern (ExactPattern "cabal.project.local") False 0] "cabal.project.local"` is called
- **THEN** it SHALL return `True`

#### Scenario: Dot-filename does not match as suffix
- **WHEN** `shouldIgnore [AnnotatedPattern (ExactPattern "cabal.project.local") False 0] "my-cabal.project.local"` is called
- **THEN** it SHALL return `False` (exact match only matches the filename itself, not strings that end with it)

### Requirement: Backward Compatibility
All existing `IgnorePattern` variants (`PrefixPattern`, `SuffixPattern`, `ExactPattern`, `GlobPattern`) SHALL continue to work unchanged. The `GlobPattern` variant SHALL be retained for backward compatibility with hardcoded patterns, though new parsing will prefer `WildcardPattern` for `*`-containing patterns.

- **Plan**: The fix must not break any existing behavior — all current tests must pass.
- **Do**: Add `WildcardPattern` as a new constructor. Keep `GlobPattern` but stop producing it from `parsePattern`. Existing `hardcodedIgnorePatterns` that produce `GlobPattern` still work.
- **Check**: All existing `IgnoreSpec` tests pass without modification.

#### Scenario: Existing GlobPattern still matches
- **WHEN** `shouldIgnore [AnnotatedPattern (GlobPattern ".log") False 0] "app/debug.log"` is called
- **THEN** it SHALL return `True` (existing behavior preserved)

#### Scenario: Existing SuffixPattern still matches
- **WHEN** `shouldIgnore [AnnotatedPattern (SuffixPattern ".min.js") False 0] "app/bundle.min.js"` is called
- **THEN** it SHALL return `True` (existing behavior preserved)

#### Scenario: Existing PrefixPattern still matches
- **WHEN** `shouldIgnore [AnnotatedPattern (PrefixPattern "node_modules") False 0] "node_modules/pkg/index.js"` is called
- **THEN** it SHALL return `True` (existing behavior preserved)

### Requirement: Negation Patterns with Wildcards
Negation patterns (lines starting with `!`) SHALL correctly create `AnnotatedPattern` with `apNegate = True` and `WildcardPattern` when the pattern contains `*`.

- **Plan**: The `!` prefix handling in `parseGitignoreLine` already works; it delegates to `parsePattern`. The fix to `parsePattern` automatically fixes negation patterns too.
- **Do**: Ensure `parseGitignoreLine` with `!` produces negated `WildcardPattern` for wildcard lines.
- **Check**: Negation + wildcard patterns correctly override lower-priority ignores.

#### Scenario: Negation with wildcard pattern
- **WHEN** `parseGitignoreLine 2 "!.opencode/agent/**"` is called
- **THEN** it SHALL produce `AnnotatedPattern { apPattern = WildcardPattern ".opencode/agent/**", apNegate = True, apPriority = 2 }`

#### Scenario: Negation wildcard overrides positive ignore
- **WHEN** `shouldIgnore [AnnotatedPattern (PrefixPattern ".opencode") False 0, AnnotatedPattern (WildcardPattern ".opencode/agent/**") True 2] ".opencode/agent/core/openagent.md"` is called
- **THEN** it SHALL return `False`

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
