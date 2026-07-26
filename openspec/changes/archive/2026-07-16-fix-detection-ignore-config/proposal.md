## Why

Three interconnected bugs cause Graphos to process far more files than intended, directly contributing to the RAM crash:

1. **`.graphosignore` is dead code** — `Infrastructure.FileSystem.Ignore` exports `loadGraphosignore` and `shouldIgnore`, is listed in `graphos.cabal`, but no module ever imports it. Patterns in `.graphosignore` have zero effect.

2. **`graphos.yaml` extensions are ignored** — `Pipeline.hs` calls `detectFiles` (which uses hardcoded `allSupportedExtensions`) instead of `detectFilesWithExtensions` (which uses `gcFileExtensions` from config). The `file_extensions` and `extractors` sections in `graphos.yaml` have no effect on detection or extraction routing.

3. **`.gitignore` is never consulted** — there is no `.gitignore` handling anywhere. The hardcoded `isIgnored` list (12 entries) misses common directories like `target/`, `vendor/`, `.next/`, `coverage/`, `.gradle/`, `.idea/`, `.vscode/`, `.cache/`, `.cargo/`, etc.

The result: `node_modules/` (thousands of files), `.git/` objects, `dist-newstyle/` build artifacts, `vendor/` dependencies, and other irrelevant files all get fed into extraction, each spawning tree-sitter or LSP parsing — massively inflating memory usage and processing time.

## What Changes

1. **Wire `.graphosignore` into detection** — Load `.graphosignore` patterns in `Detect` and apply `shouldIgnore` to directory entries during recursive traversal, supplementing the hardcoded ignore list.

2. **Wire `.gitignore` into detection** — Add a `loadGitignore` function to `Infrastructure.FileSystem.Ignore` that reads `.gitignore` patterns (honoring negation `!` and directory-only `/` suffixes at a simplified level), and merge these with `.graphosignore` patterns. `.gitignore` patterns are read from the project root only.

3. **Use config-driven extensions in pipeline** — Change `Pipeline.hs` to call `detectFilesWithExtensions` with `gcFileExtensions` from the loaded `GraphosConfig`, so the `file_extensions` section in `graphos.yaml` actually controls what gets detected.

4. **Wire extractors config into detection** — Pass `gcExtractors` through the detection path so that files with `mode: stub` or disabled LSP are handled correctly during detection (e.g., stub-mode files are counted but not routed to LSP).

5. **Expand the hardcoded ignore list** — Add common directories that should always be ignored: `target/`, `vendor/`, `.next/`, `.nuxt/`, `.gradle/`, `.idea/`, `.vscode/`, `.cache/`, `.cargo/`, `bower_components/`, `.direnv/`, `.sass-cache/`, `coverage/`, `.pytest_cache/`, `.mypy_cache/`, `.tox/`, `__pypackages__/`, `.pdm-build/`, `.venv/` (already present), `.env/`, `.yarn/`, `.pnpm-store/`.

## Capabilities

### New Capabilities
- `gitignore-support`: Read and apply `.gitignore` patterns during file detection, merging with `.graphosignore` and hardcoded defaults

### Modified Capabilities
- `detection`: Use config-driven file extensions and extractor routing instead of hardcoded defaults; apply ignore patterns from `.graphosignore`, `.gitignore`, and built-in list
- `extraction`: Use `gcExtractors` from config to route files to correct extractor (LSP, tree-sitter, stub)

## Impact

- **Code**: `UseCase.Detect` (major refactor), `UseCase.Pipeline` (call site change), `Infrastructure.FileSystem.Ignore` (add gitignore support)
- **API**: `detectFiles` gains config parameters; `detectFilesWithExtensions` becomes the primary entry point
- **Behavior**: Large directories (`node_modules/`, `vendor/`, `.git/`, build dirs) are now properly excluded; `graphos.yaml` file_extensions actually takes effect
- **Performance**: Fewer files detected → fewer files extracted → lower memory and faster runs
- **Compatibility**: Default behavior preserved when no config file exists (hardcoded extensions + expanded ignore list)

## PDCA Cycle

- **Plan**: Reduce files fed into extraction by properly respecting ignore patterns and config, directly reducing memory pressure on large codebases.
- **Do**: Wire `.graphosignore` into detection, add `.gitignore` support, use `detectFilesWithExtensions` in pipeline, expand hardcoded ignore list.
- **Check**: On a codebase with `node_modules/`, verify that: (1) `node_modules/` files are not detected, (2) `graphos.yaml` file_extensions changes what gets detected, (3) `.graphosignore` patterns are respected, (4) `.gitignore` patterns are respected. Run `cabal test`.
- **Act**: Update detection tests to cover config-driven and ignore-pattern scenarios. Document the config-driven detection flow in project context.