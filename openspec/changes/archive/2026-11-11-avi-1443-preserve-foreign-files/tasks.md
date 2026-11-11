# Tasks — Preserve foreign output files across a staged rebuild (AVI-1443)

## 1. Staged swap preserves non-Graphos entries (implements the delta spec)

### 1.1 Replace the fixed carry-over list with an enumerate-and-skip allowlist
- [x] In `UseCase/Pipeline/Staging.hs`, replace the fixed `carryOverEntries` list
  with a `carryOverState` that enumerates the old output directory and carries
  over every entry not in the Graphos-owned allowlist `ownedArtifacts`.
- [x] Keep `ownedArtifacts` as the short allowlist of regenerable Graphos
  artifacts (`graph.json`, `graph.checkpoint.json`, `GRAPH_REPORT.md`,
  `graph.html`, `community_graph.json`, `embeddings.json`, `index.json`,
  `graph.cypher`, `memgraph.cypher`, `obsidian/`); owned entries are left in the
  old output and replaced on the swap.
- [x] Owned directories (e.g. `obsidian/`) are excluded from verbatim
  carry-over so the swap replaces them instead of merging stale files.
- [x] Persistent state (`cache/`, `memory/`, `debug/`, `traces/`) continues to
  survive — carried over along with every other non-owned entry.

### 1.2 Tests
- [x] `Graphos.UseCase.StagingSpec`: foreign file survives a rebuild and the
  owned artifact is refreshed.
- [x] Foreign directory is carried verbatim across a rebuild.
- [x] Owned directory (`obsidian/`) is refreshed, not merged, across a rebuild.
- [x] Unknown / hidden entries survive regardless of name.

### 1.3 Check
- [x] `devenv shell -- cabal build graphos` (dev flag, `-Wall -Werror`) green.
- [x] `devenv shell -- cabal test graphos-test` green.
- [x] Manual smoke: build into `<out>`, drop a foreign file into `<out>`, rebuild
  with `--fresh -o <out>` — the foreign file remains and owned artifacts update.
- [x] `openspec validate AVI-1443-preserve-foreign-files` passes.

## 2. Review follow-ups (haskell-reviewer, AVI-1443)

- [x] Own `pipeline.checkpoint.json`: the pipeline resume state is cleared on a
  successful build, so a copy left in the old output is stale and must never be
  resurrected into the fresh output.
- [x] Guard `carryOverState` against clobbering a staged artifact: `renameFile`
  overwrites its destination, so a Graphos artifact missing from
  `ownedArtifacts` would silently revert to the previous run's copy. Skip any
  name already present in staging.
- [x] Tests: stale `pipeline.checkpoint.json` is not carried over; the staged
  copy wins on a name collision; `memory/`, `debug/` and `traces/` survive.
- [x] `Graphos.UseCase.StagingSpec`: 17 examples, 0 failures.
- [x] Full suite: 1342 examples, 0 failures, 3 pending.
- [x] End-to-end repro from GitHub #76: `NOTES.txt` survives both
  `--fresh` and plain rebuilds; `graph.json` is refreshed.
