# Tasks — Preserve foreign output files across a staged rebuild (AVI-1443)

## 1. Staged swap preserves non-Graphos entries (implements the delta spec)

### 1.1 Replace the fixed carry-over list with an enumerate-and-skip allowlist
- [ ] In `UseCase/Pipeline/Staging.hs`, replace the fixed `carryOverEntries` list
  with a `carryOverState` that enumerates the old output directory and carries
  over every entry not in the Graphos-owned allowlist `ownedArtifacts`.
- [ ] Keep `ownedArtifacts` as the short allowlist of regenerable Graphos
  artifacts (`graph.json`, `graph.checkpoint.json`, `GRAPH_REPORT.md`,
  `graph.html`, `community_graph.json`, `embeddings.json`, `index.json`,
  `graph.cypher`, `memgraph.cypher`, `obsidian/`); owned entries are left in the
  old output and replaced on the swap.
- [ ] Owned directories (e.g. `obsidian/`) are excluded from verbatim
  carry-over so the swap replaces them instead of merging stale files.
- [ ] Persistent state (`cache/`, `memory/`, `debug/`, `traces/`) continues to
  survive — carried over along with every other non-owned entry.

### 1.2 Tests
- [ ] `Graphos.UseCase.StagingSpec`: foreign file survives a rebuild and the
  owned artifact is refreshed.
- [ ] Foreign directory is carried verbatim across a rebuild.
- [ ] Owned directory (`obsidian/`) is refreshed, not merged, across a rebuild.
- [ ] Unknown / hidden entries survive regardless of name.

### 1.3 Check
- [ ] `devenv shell -- cabal build graphos` (dev flag, `-Wall -Werror`) green.
- [ ] `devenv shell -- cabal test graphos-test` green.
- [ ] Manual smoke: build into `<out>`, drop a foreign file into `<out>`, rebuild
  with `--fresh -o <out>` — the foreign file remains and owned artifacts update.
- [ ] `openspec validate AVI-1443-preserve-foreign-files` passes.
