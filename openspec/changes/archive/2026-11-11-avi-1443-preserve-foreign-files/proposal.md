## Why

A full rebuild with `graphos <path> --fresh -o <out>` (or any staged rebuild)
writes its artifacts into a fresh staging directory and then atomically swaps it
into `<out>`: the old `<out>` is renamed to `<out>.prev-...` and deleted once the
new one is in place. The staged swap only carries the persistent-state
subdirectories (`cache/`, `memory/`, `debug/`, `traces/`) back into staging, so
the rename-and-delete discards everything else that was in `<out>` — including
any file the user placed there. A rebuild silently destroys files Graphos did
not write.

## What Changes

- The staged swap carries over **every entry in the old output directory that
  Graphos does not regenerate** into the staging directory before the swap, so
  the atomic swap never discards a file Graphos did not write.
- Graphos-owned artifacts (the regenerable ones listed in `ownedArtifacts`) are
  left in the old output and simply replaced by the fresh build on the swap;
  owned **directories** (e.g. `obsidian/`) are never carried over verbatim, so
  the swap does not silently resurrect stale files from the previous run.

## Capabilities

### Modified Capabilities
- `staging-preserve`: the staged full-rebuild swap preserves foreign output
  entries instead of deleting them.

## Impact

- **UseCase/Pipeline/Staging**: `carryOverState` now enumerates the old output
  directory and carries over non-owned entries; adds `ownedArtifacts`.
- **Tests**: staging spec covers foreign-file survival and owned-directory
  replacement across a staged rebuild.
- No CLI, config, or downstream behavior changes.
