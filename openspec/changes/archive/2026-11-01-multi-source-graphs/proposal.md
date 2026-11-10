## Why

Graphos graphs one filesystem root per run: the single positional PATH argument walks one directory tree, and every exported node's identity is derived from a path relative to that one root. Real projects are not one root — a feature under development spans the main checkout plus one or more git worktrees, a microservice spans several sibling repositories, and a knowledge corpus spans cloned papers and notes. Merging such graphs today requires separate runs plus `graphos merge`, whose old-wins `Map.union` leaves stale nodes and discards deletions — and the per-file cost profile is wrong: adding one source to an N-source graph re-extracts everything instead of only the new source. (PRD §3.4, workflow 02; PRD §13, workflow 09)

Meanwhile LLM community labels — the most expensive pipeline output per token — are recomputed from scratch on every run even when Leiden leaves most communities untouched, because no label memory exists between runs. Adding a small source to a large multi-repo graph currently costs the full labeling bill. (PRD §5, workflow 11)

## What Changes

- **Declarative sources in `graphos.yaml`**: a new `sources:` key lists named roots (name + path + optional per-source ignore patterns). Absent key keeps today's single-positional-PATH behavior unchanged. A new `output:` key moves the graph output directory from CLI-only (`-o`) into config; CLI flag still overrides.
- **Multi-root detect**: the file set becomes the union of all sources' walks, each file identified as `sourceName/relativePath`. Path-prefixed NodeIds make cross-repo same-path files distinct (today two roots with `src/Utils.hs` collide). Single-source runs keep their existing NodeIds (no prefix) — zero migration.
- **Node source tagging**: every node gains a `nodeSource` field (the source name; `null` for legacy single-path runs), enabling viewer/query coloring by source and source-scoped reporting.
- **Multi-root watch**: `--watch` starts one fsnotify tree per source; each event is attributed to a source via longest-prefix ownership so a worktree nested inside its parent repo is not double-claimed. Per-source `.gitignore`/`.graphosignore` resolution is preserved.
- **Add/remove source = one `--update` run**: the update path (cache-accelerated full rebuild from the current file set, per the `wire-incremental-update` change) absorbs source changes for free — new sources' files are extraction-cache misses (extracted), existing sources' files are cache hits (skipped), removed sources' files leave the file set (nodes vanish). No graph-surgery machinery.
- **Community label cache**: `<output>/cache/label-cache.json` persists labels between runs, keyed by member-set fingerprint and model. Lookup is fuzzy v1: bidirectional containment (a new community inherits a cached label when both the old community survives in it and it is mostly the old one, ≥ 0.8 each way) — boundary drift keeps labels, splits and merges re-label. The cache is a last-run snapshot: rewritten to the current run's communities after each labeling pass.
- Hardcoded `graphos-out/` stragglers (Cache.hs cacheDir, Manifest path, MCP conversations dir, research command output) learn the configured output directory.

## Capabilities

### New Capabilities
- `multi-source-graphs`: Named multi-root sources in graphos.yaml — config schema, unioned file-set detection with `sourceName/relativePath` identification and NodeId prefixing, node source tagging, multi-root watch with longest-prefix ownership, and configured output directory. (PRD §3.1 detect stage, §3.4 workflow 02, workflow 03 watch)
- `community-label-cache`: Persistent LLM label reuse across runs — member-set fingerprint + model keyed cache, fuzzy v1 bidirectional-containment matching, last-run snapshot semantics, survival across staged-rebuild swaps. (PRD §5, workflow 11)

### Modified Capabilities
- `node-schema`: The canonical Node field set gains a 13th field `nodeSource` (Maybe — null for legacy single-path runs); legacy field-absence guarantees are restated to include it.
- `03-watch-mode`: Watch requirement extends from one directory to one fsnotify tree per configured source, with per-source ignore resolution and source attribution of events.
- `15-config-init`: `graphos init` template gains `sources:` and `output:` example sections alongside existing defaults.

## Impact

- **Domain**: `Types/Node.hs` (+`nodeSource` field, present-bits, JSON), `Config/Core.hs` + `Config` split modules (+`gcSources`, `gcOutput`), `Graph/Core.hs` (NodeId prefixing contract), `Labeling.hs` (label-cache types).
- **UseCase**: `Detect.hs` (multi-root union walk), `Pipeline/Core.hs` + `Incremental.hs` (source-aware config, label-cache consult/persist around labeling), `Label.hs` (cache-aware label lookup), `Export` view models (source tag in HTML/report outputs).
- **Infrastructure**: `Config.hs` (YAML parsing of `sources:`/`output:`), `FileSystem/Watcher.hs` (N watchTrees, event→source attribution, drop the comma-joined-string callback in favor of `[FilePath]`), `FileSystem/Cache.hs` + `Manifest.hs` (use configured output dir), `Server/MCP.hs` + research command (configured output dir).
- **CLI**: `app/Main.hs` + `CLI/Parser.hs` (`sources:` present → PATH positional optional; `--update` semantics unchanged).
- **Cost profile guarantee** rides on the `wire-incremental-update` change landing first (extraction cache, `--update` confluence); label cost reduction is delivered by this change's `community-label-cache`.
- **No breaking CLI changes**: existing single-path invocations, flags, and graph.json consumers (community_id consumers already tolerate nullable fields) keep working; `nodeSource` serializes as nullable `source`.