## Context

Today `cfgInputPath` is a single positional argument (CLI/Parser.hs:67), detect walks one root, NodeIds embed a dirHash of the walked relative path (`makeNodeId`, TreeSitter/Convert.hs:398), and `--watch` runs one fsnotify tree whose callback receives a comma-joined path string (Watcher.hs:57). The output dir is CLI-only (`-o`, default `graphos-out`) with several hardcoded stragglers (Cache.hs:36, Manifest.hs:40, MCP.hs:68, Main.hs:541). The in-flight `wire-incremental-update` change defines update runs as cache-accelerated full rebuilds from the current file set — deletions vanish by construction and `--fresh` output converges with `--update` output. See proposal.md for motivation.

## Goals / Non-Goals

**Goals:**
- One graph over N named roots, declared in `graphos.yaml`, with add/remove-source cost ≈ extract(new source) + label(new communities only)
- Cross-source NodeId uniqueness without migrating existing single-source graphs
- Source visibility on every node (field, JSON, exports)
- Watch mode extended to N roots with unambiguous event ownership
- LLM label reuse across runs via a persistent, fuzzy-matched cache

**Non-Goals:**
- Graph-surgery primitives (prune-by-source, incremental merges of retained graphs) — the update-rebuild path makes them unnecessary
- Stable community IDs across runs (disposable; per user decision)
- Embedding cache changes (already keyed by NodeId; prefixing keeps it correct)
- Editing `wire-incremental-update` artifacts — sequenced before this change, not absorbed

## Decisions

### D1 — Source-qualified paths as the identity carrier (Domain + UseCase)

A file in a multi-source run is identified everywhere as `sourceName/relativePath`. `nodeSourceFile` carries the prefixed path; `makeNodeId`'s dirHash folds the prefixed directory, so distinct sources yield distinct NodeIds. Single-source runs keep bare relative paths — existing NodeIds, cache keys, and embeddings are untouched.

| layer | change |
|---|---|
| Domain | `SourceConfig { scName, scPath, scIgnore }` list on `GraphosConfig`; `Node.nodeSource :: Maybe ShortText` (13th field, present-bits) |
| UseCase | detect walks each source root (existing per-root ignore resolution), longest-prefix-deduplicates the union, and prefixes paths |
| Infrastructure | `Config.hs` parses/validates `sources:`; extractors receive prefixed paths unchanged (they are path-agnostic) |

Alternatives considered: (a) separate `nodeRepo` field + unprefixed paths — rejected: NodeIds would still collide across sources since `makeNodeId` hashes only the relative dir; (b) canonical absolute paths — rejected: breaks portability and cache relocation.

### D2 — Longest-prefix ownership for overlapping roots (UseCase)

Sources are sorted by descending path length; a file belongs to the first root that is a path-prefix of it. Applied twice: once in detect (file-set union), once in watch (event attribution). Same rule in both places guarantees a changed file's incremental extraction matches its detect-time identity.

Alternatives considered: (a) first-declared-wins — rejected: silently order-dependent for nested worktrees; (b) excluding nested roots at config validation — rejected: nested worktrees are a primary use case, not an error.

### D3 — `nodeSource` as first-class field (Domain)

Thirteenth canonical field on `Node`, `Maybe ShortText`, present-bit like other optionals, serialized as nullable `source`. Chosen over `nodeExtra` blob because source identity is query-relevant (viewer coloring, future `--source` query scoping, report stats) and `nodeExtra` is a misc bin (capturedAt, childCount, doc_body) not a query surface. `Nothing` for single-path runs keeps the node-schema guarantee: `source: null` mirrors `community_id: null` handling downstream. amends the node-schema spec's field list.

Alternatives considered: (a) `nodeExtra.source` — rejected per above; (b) always-tagging with a synthetic default source name — rejected: would silently change every existing graph.json.

### D4 — Label cache: last-run snapshot + bidirectional fuzzy matching (UseCase + Domain)

Cache lives at `<output>/cache/label-cache.json`, persisted with the staged-rebuild `cache/` carry-over. Entry schema:

```
{ "model": "llama3.2",
  "entries": [
    { "fingerprint": sha256(sorted member NodeIds),
      "members": [NodeId ...],      -- kept for fuzzy intersection
      "label": "...", "labeledAt": "..." } ] }
```

Lookup per new community: exact fingerprint hit → reuse; else fuzzy scan over same-model entries; bidirectional containment both ≥ 0.8 → inherit; largest-|∩| wins, lowest community id tie-breaks; else LLM label. After the pass the file is rewritten to exactly the current run's communities (snapshot semantics — bounded size, no eviction logic needed).

The bidirectional rule exists precisely to stop merges inheriting a stale label: unidirectional containment makes the union of two communities inherit the first operand's label. Splits fail old-side containment (each half is < 0.8 of old), merges fail new-side (each old is < 0.8 of the union) — both re-label, which is the desired behavior.

Pure matching functions (`fingerprintOf`, `matchLabel :: [Entry] -> Set NodeId -> Maybe (Entry, MatchKind)`) live in `Domain.Labeling`; persistence and consult/persist orchestration in UseCase.Label via the existing port pattern; file IO in Infrastructure.

Alternatives considered: (a) exact-match only v1 — rejected per user decision (fuzzy v1 chosen); (b) unidirectional containment — rejected: mislabels merges; (c) appending history + eviction — rejected: unbounded growth and eviction soundness work for a marginal reuse gain.

### D5 — Add/remove source rides the update-rebuild path (UseCase, no new mechanism)

No graph surgery. Changing `sources:` then running `--update` rebuilds from the new unioned file set: new sources' files are extraction-cache misses, unchanged sources' files are hits, removed sources' files are absent so their nodes vanish. Requires `wire-incremental-update` to land first (sequencing constraint, not a code dependency — this change works without it, just without the cost guarantee).

Alternatives considered: (a) prune-by-tag + merge for O(new-source) rebuilds — rejected: duplicates deletion correctness machinery that rebuild gets for free and re-cluster is full anyway (Incremental.hs re-clusters every run, so community-identity savings never materialize); (b) per-source subgraphs stitched at export — rejected: cross-source edges and clustering need one graph.

### D6 — Multi-root watcher (Infrastructure)

N `watchTree` sessions under one fsnotify manager; events land in one shared debounced `MVar (Set (Source, FilePath))`. Attribution applies D2's longest-prefix rule to the event path. Callback signature becomes a real list of `(sourceName, relativePath)` pairs, replacing the `", "`-join protocol (fragile: a path containing `", "` corrupts the split). Single-root watch keeps its entry point.

Alternatives considered: (a) watching the common ancestor of all roots — rejected: floods events outside sources and can't attribute nested roots correctly; (b) one watcher thread per source with independent debounces — rejected: N triggers per burst instead of one coalesced run.

### D7 — `output:` config key unblocks stragglers (Domain + Infrastructure)

`gcOutput :: Maybe FilePath` on config (CLI flag still wins via existing flag-merge). The hardcoded `graphos-out` sites (Cache.hs:36, Manifest.hs:40, MCP.hs:68, Main.hs:541) resolve the effective output dir instead. Default remains `graphos-out` — no behavior change unless configured.

Alternatives considered: leaving stragglers — rejected: with `output:` configurable, cache state and conversations dir silently diverging from exports would corrupt label-cache and embedding persistence.

## Risks / Trade-offs

- [Leiden instability near new sources can exceed fuzzy threshold] → Mitigation: communities reshuffled beyond 0.8 containment simply re-label; cost guarantee degrades gracefully to "label the changed region", never wrongness.
- [Fuzzy inheritance cements a label while a community drifts 5% per run] → Mitigation: labeledAt recorded; snapshot semantics bound staleness to one run's disappearance (a community that drops out for a run loses its cache entry). Re-label via `--fresh`-style cache bypass if ever needed.
- [Path prefixing changes `nodeSourceFile` consumers (context formatting, report)] → Mitigation: prefix is `sourceName/` — displays naturally; context formatting derives location from line fields per node-schema spec.
- [fsnotify watch limits with many large roots] → Mitigation: inotify is per-watcher-session; same-order watcher count as sources (typically 2–5). Documented limit; not addressed further.
- [Config `output:` diverging between query commands (`--graph` defaults) and pipeline output] → Mitigation: D7 resolves defaults from effective config at load time; query-family flags unchanged.

## Migration Plan

1. Deploy after `wire-incremental-update` (cost guarantee; label cache independent of it).
2. Single-source users: zero action — no `sources:` key means identical behavior and NodeIds.
3. Adopters: add `sources:` + `output:` to graphos.yaml, run `graphos --update`; first run is a cold build of all sources (all extraction misses), subsequent add/remove runs hit the cost profile.
4. Rollback: remove `sources:`/`output:` keys; graphs and caches revert to single-path behavior (cache keys for prefixed paths simply go unused).

## Verification

- `cabal build` with `-Wall -Werror` (all layers compile; no partial functions introduced).
- `cabal test`: config validation (duplicate/empty names, bad paths), union-detect dedup (overlapping roots), NodeId distinctness across sources, `nodeSource` serialization (null vs named), watcher attribution + debounce coalescing, label-cache exact/fuzzy/split/merge/tie-break QuickCheck properties (bidirectional threshold), last-run snapshot rewrite, staged-rebuild survival, zero-LLM-call unchanged run.
- Confluence check on this repo: `graphos . --update` vs `graphos . --fresh` produce equal node/edge sets (inherits wire-incremental-update's invariant).