## MODIFIED Requirements

### Requirement: Config-driven file extension detection

The detection pipeline SHALL use `gcFileExtensions` from the loaded `GraphosConfig` to determine which file extensions are detected, instead of hardcoded `allSupportedExtensions`. When `graphos.yaml` specifies `file_extensions`, those categories override the defaults.

Previously: `Pipeline.hs` called `detectFiles` which used `allSupportedExtensions` (hardcoded). The `file_extensions` section in `graphos.yaml` had no effect on detection.

- **Plan**: Make `graphos.yaml` `file_extensions` actually control what gets detected.
- **Do**: Change `Pipeline.hs` to call `detectFilesWithExtensions path (gcFileExtensions config)` instead of `detectFiles path`.
- **Check**: When `graphos.yaml` specifies only `.py` in code extensions, only `.py` files are detected as code files.
- **Act**: If this changes behavior for users without `graphos.yaml`, verify defaults match old behavior.

#### Scenario: graphos.yaml controls detected extensions
- **WHEN** `graphos.yaml` contains `file_extensions: { code: [.py] }`
- **AND** the project has `main.py`, `app.ts`, and `readme.md`
- **THEN** only `main.py` is detected as a code file
- **AND** `app.ts` is not detected at all (not in any category)
- **AND** `readme.md` is not detected (doc extensions not configured)

#### Scenario: Missing graphos.yaml falls back to defaults
- **WHEN** no `graphos.yaml` file exists
- **THEN** detection uses `defaultFileExtensions` from `GraphosConfig`
- **AND** behavior is identical to the current hardcoded `allSupportedExtensions`

#### Scenario: Partial graphos.yaml merges with defaults
- **WHEN** `graphos.yaml` contains `file_extensions: { code: [.py, .rs] }` (only code category specified)
- **THEN** code detection uses `.py` and `.rs` only
- **AND** doc, image, video, and office categories use default extensions
- **AND** paper category uses default extensions

### Requirement: .graphosignore patterns applied during detection

`.graphosignore` patterns SHALL be loaded from the project root and applied during file detection. The `Ignore` module's `loadGraphosignore` and `shouldIgnore` functions SHALL be called by the detection pipeline.

Previously: `Infrastructure.FileSystem.Ignore` existed but was never imported or used by any module.

- **Plan**: Wire the existing `.graphosignore` support into the detection pipeline.
- **Do**: Import `loadGraphosignore` and `shouldIgnore` in `Pipeline.hs`. Load patterns at pipeline start, pass to `detectFilesWithExtensions`.
- **Check**: A `.graphosignore` file with `src/internal/` causes that directory to be excluded from detection.

#### Scenario: .graphosignore excludes a directory
- **WHEN** the project root has `.graphosignore` containing `src/internal/`
- **AND** `src/internal/secret.hs` exists
- **THEN** `src/internal/secret.hs` is not in the detected files

#### Scenario: .graphosignore excludes files by pattern
- **WHEN** the project root has `.graphosignore` containing `*.generated.*`
- **AND** `app.generated.ts` exists
- **THEN** `app.generated.ts` is not in the detected files

### Requirement: Expanded hardcoded ignore list

The hardcoded `isIgnored` list SHALL be expanded from 12 entries to approximately 30 entries, covering common build artifacts, dependency directories, IDE folders, and cache directories across major ecosystems (Haskell, JavaScript/TypeScript, Python, Rust, Go, Java, .NET, etc.).

Previously: `isIgnored` contained only `[".git", "node_modules", "__pycache__", ".venv", "dist", "dist-newstyle", "build", ".stack-work", "graphos-out", ".opencode", ".tmp", ".obsidian", ".github"]`.

- **Plan**: Cover the most common directories that should never be processed, reducing false positives in detection.
- **Do**: Add `target/`, `vendor/`, `.next/`, `.nuxt/`, `.gradle/`, `.idea/`, `.vscode/`, `.cache/`, `.cargo/`, `bower_components/`, `.direnv/`, `.sass-cache/`, `coverage/`, `.pytest_cache/`, `.mypy_cache/`, `.tox/`, `__pypackages__/`, `.pdm-build/`, `.yarn/`, `.pnpm-store/`, `.svn/`, `.hg/`, `.DS_Store`, `DerivedData/`, `.build/`.
- **Check**: Each added entry is a common directory that should never be traversed.

#### Scenario: Common directories are ignored without config
- **WHEN** a project has directories `target/`, `vendor/`, `.next/`, `.gradle/`, `.idea/`
- **AND** no `.graphosignore` or `.gitignore` exists
- **THEN** none of these directories are traversed during detection

#### Scenario: Original hardcoded entries still ignored
- **WHEN** a project has `.git/`, `node_modules/`, `dist-newstyle/`, `.stack-work/`
- **THEN** these directories are still ignored (backward compatible)