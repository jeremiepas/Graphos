## Context

The detection pipeline (`UseCase.Detect`) has three independent sources of filtering that should control what files enter extraction:

1. **Ignore patterns** — which directories/files to skip entirely (`.git/`, `node_modules/`, etc.)
2. **Extension categories** — which file extensions map to which category (code, doc, image, etc.)
3. **Extractor routing** — which extractor (LSP, tree-sitter, stub) handles each extension

Currently, none of these are wired correctly:
- `.graphosignore` exists as code but is never called
- `.gitignore` is never read
- `isIgnored` is a hardcoded 12-entry list
- `Pipeline.hs` calls `detectFiles` (hardcoded extensions) instead of `detectFilesWithExtensions` (config-driven)
- `gcExtractors` is used in `Extract` but `Detect` doesn't benefit from it

The clean architecture constraint: Domain has zero IO. All file system access (reading `.gitignore`, `.graphosignore`) must stay in Infrastructure. UseCase orchestrates by calling Infrastructure to load patterns, then passing them to Domain-adjacent detection logic.

## Goals / Non-Goals

**Goals:**
- `.graphosignore` patterns are loaded and applied during detection
- `.gitignore` patterns (root only) are loaded and applied during detection
- `graphos.yaml` `file_extensions` controls what gets detected via `detectFilesWithExtensions`
- Hardcoded ignore list is expanded to cover common directories
- All three sources (hardcoded + `.gitignore` + `.graphosignore`) are merged with `.graphosignore` taking highest priority
- Default behavior (no config files) produces the same results as today (plus expanded ignore list)

**Non-Goals:**
- Recursive `.gitignore` loading (subdirectory `.gitignore` files) — root only for now
- Full `.gitignore` pattern syntax (negation `!`, character ranges `[a-z]`, double-star `**`) — simplified subset
- Changing the extraction pipeline (only detection changes)
- Removing the hardcoded ignore list (it remains as the base layer)

## Decisions

### D1: Layered ignore pattern merging

**Decision**: Merge three sources of ignore patterns with clear priority: `.graphosignore` (highest) → `.gitignore` (middle) → hardcoded defaults (lowest). A directory/file ignored by any layer is skipped.

**Alternatives considered**:
- A: Only `.graphosignore` — requires users to duplicate `.gitignore` patterns
- B: Only `.gitignore` — doesn't allow Graphos-specific ignores (e.g., `graphos-out/`)
- C: **Layered merge with `.graphosignore` override** — best of both worlds: respects `.gitignore` by default, allows Graphos-specific overrides, and has sane defaults

**Rationale**: Most projects already have `.gitignore` with relevant patterns. `.graphosignore` is for Graphos-specific additions (like `graphos-out/`, `.opencode/`). The hardcoded list catches common directories that might not be in `.gitignore` (`.stack-work/`, `dist-newstyle/`).

**Layer**: `Infrastructure.FileSystem.Ignore` loads and merges all three sources. `UseCase.Detect` receives the merged pattern list.

### D2: Simplified gitignore pattern matching

**Decision**: Support a subset of `.gitignore` syntax: blank lines, comments (`#`), directory patterns (`dir/`), glob patterns (`*.log`, `build/`), and negation (`!pattern`). Do NOT support: character ranges `[a-z]`, double-star `**`, or leading slash anchoring (treat all patterns as relative to root).

**Alternatives considered**:
- A: Full `.gitignore` spec — complex to implement correctly, 30+ edge cases
- B: **Simplified subset** — covers 95% of real `.gitignore` files with 5% of the complexity
- C: Use existing library — no mature Haskell `.gitignore` parser on Hackage that matches our needs

**Rationale**: The vast majority of `.gitignore` files use simple patterns like `node_modules/`, `*.log`, `build/`, `.env/`. Supporting the full spec would require 200+ lines of parser code. The simplified subset handles all common cases and rejects gracefully (unrecognized patterns are treated as exact matches).

**Layer**: `Infrastructure.FileSystem.Ignore` — parse `.gitignore` into `[IgnorePattern]` using the existing `IgnorePattern` type, plus a new `NegatePattern` wrapper for `!` lines.

### D3: Config-driven detection via detectFilesWithExtensions

**Decision**: Change `Pipeline.hs` to call `detectFilesWithExtensions` with `gcFileExtensions` from the loaded `GraphosConfig`, making `graphos.yaml` `file_extensions` actually control detection.

**Alternatives considered**:
- A: Keep `detectFiles` with hardcoded extensions — config has no effect (current bug)
- B: **Use `detectFilesWithExtensions` with config** — respects user config, falls back to defaults when no config
- C: Merge config extensions with hardcoded defaults — confusing, which wins?

**Rationale**: `detectFilesWithExtensions` already exists and works correctly. The only bug is that `Pipeline.hs` calls `detectFiles` instead. The config already has sensible defaults that match `allSupportedExtensions`, so switching is safe. When `graphos.yaml` is absent, `defaultGraphosConfig` provides the same defaults.

**Layer**: `UseCase.Pipeline` — one-line change from `detectFiles` to `detectFilesWithExtensions`.

### D4: Pass ignore patterns through detection function signature

**Decision**: Add `[IgnorePattern]` parameter to `findAllFilesWith` and `detectFilesWithExtensions`. Load ignore patterns in `Pipeline.hs` (Infrastructure call) and pass them down to `Detect`.

**Alternatives considered**:
- A: Load ignore patterns inside `Detect` — violates clean architecture (Detect is UseCase, should not do IO)
- B: **Pass patterns as parameter** — clean architecture compliant: Pipeline (UseCase) calls Infrastructure to load, passes to Detect
- C: Global IORef for patterns — implicit state, hard to test

**Rationale**: Detect is in UseCase layer (no IO). Pipeline orchestrates: it calls `loadGraphosignore` and `loadGitignore` (Infrastructure), merges with hardcoded defaults, and passes the merged `[IgnorePattern]` to `detectFilesWithExtensions`. This keeps the clean architecture boundary intact.

**Layer**: `UseCase.Detect` (signature change), `UseCase.Pipeline` (orchestration), `Infrastructure.FileSystem.Ignore` (gitignore loading).

### D5: Expanded hardcoded ignore list

**Decision**: Expand `isIgnored` from 12 entries to ~30 entries covering common build artifacts, dependency directories, IDE folders, and cache directories across all major ecosystems.

**Rationale**: The current list misses `target/` (Scala/Java), `vendor/` (Go/Ruby), `.next/` (Next.js), `.gradle/` (Android/Java), `.idea/` and `.vscode/` (IDEs), `.cache/`, `.cargo/` (Rust), `coverage/`, `.pytest_cache/`, `.mypy_cache/` (Python), etc. These directories can contain thousands of files that should never be processed.

**Layer**: `UseCase.Detect` — expand the hardcoded list.

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Simplified gitignore parsing misses some patterns | Unsupported patterns fall through to exact-match — conservative (ignores less, not more). Users can add patterns to `.graphosignore` for full control. |
| Config-driven extensions change behavior for existing users | Default `GraphosConfig` matches current `allSupportedExtensions` exactly — no behavioral change without explicit `graphos.yaml` |
| Passing `[IgnorePattern]` through function signatures adds parameters | One-time refactor; improves testability (can test with arbitrary patterns) |
| `.gitignore` negation (`!pattern`) is complex to implement | Support `!` by removing from the ignore set; covers 99% of negation use cases |

## Verification Strategy (Check)

1. **Unit tests**: Add Hspec tests for `loadGitignore`, `shouldIgnore` with gitignore patterns, `mergeIgnorePatterns`, and config-driven detection.
2. **Integration test**: Run `graphos .` on a project with `node_modules/`, verify `node_modules/` files are not detected.
3. **Config test**: Create a `graphos.yaml` with only `.py` in code extensions, verify only `.py` files are detected.
4. **`.graphosignore` test**: Create `.graphosignore` with `src/internal/`, verify that directory is skipped.
5. **`.gitignore` test**: Create `.gitignore` with `*.generated.js`, verify those files are skipped.
6. **Existing tests**: `cabal test` — all must pass.

## Iteration & Rollback (Act)

- **If gitignore parsing is too incomplete**: Add more pattern types in a follow-up (double-star `**`, character ranges). The simplified subset is forward-compatible.
- **If config-driven detection changes behavior**: Add a `--strict-extensions` flag that only processes files matching `file_extensions` (vs. current behavior which processes all supported extensions). Default remains backward-compatible.
- **If performance degrades**: Loading `.gitignore` is one file read at startup — negligible. If pattern matching becomes slow on huge projects, cache the compiled pattern set.
- **Standardize**: Document the ignore precedence order (`.graphosignore` > `.gitignore` > hardcoded) in project context files.