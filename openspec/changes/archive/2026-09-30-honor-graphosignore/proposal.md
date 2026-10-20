## Why

A `.graphosignore` file placed at both the invocation directory and the scan-root
had no effect: after deleting the checkpoint and rebuilding, the target file was
still extracted (61,303 nodes remained) and still appeared in the discovery list.
Users have no reliable, documented way to exclude files, and no feedback on
whether their ignore patterns were loaded or matched.

## What Changes

- Fix `.graphosignore` loading so patterns reliably exclude files during the
  Detect stage.
- Define and document **where** `.graphosignore` is read from relative to the
  `PATH` argument and **what** paths patterns match against (normalized,
  scan-root-relative, gitignore-style globs).
- Add INFO logging: `Loaded N ignore patterns from <file>` and `Ignored M files`.
- Add a repeatable `--ignore GLOB` CLI flag as an unambiguous alternative to the
  file.
- Support standard gitignore semantics: `**`, `*`, leading `/` anchoring, and
  `#` comments.

## Capabilities

### New Capabilities
- `ignore-patterns`: load, resolve, and apply user-supplied ignore globs
  (from `.graphosignore` and `--ignore`) during file discovery, with logging.

### Modified Capabilities
<!-- none: prior ignore behavior was non-functional; this defines the contract -->

## Impact

- **Infrastructure/FileSystem/Ignore** module: fix pattern matching and file
  resolution.
- **UseCase/Detect** stage: apply ignore filter to the candidate file set.
- **CLI (optparse-applicative)**: add `--ignore`.
- **Logging**: pattern-load and match counts.
