# Tasks — Consumable Graph Outputs

## 1. Subcommand help (smallest, unblocks discovery of everything else)

### 1.1 Helper on every registration
- [ ] Add `<**> helper` to the nine bare `info` registrations in `CLI/Parser.hs` (`cypher`, `push`, `push-memgraph`, `merge`, `ingest`, `subgraph`, `serve`, `init`, `install-skill`)
- [ ] Usage examples in the footers of `subgraph`, `cypher` (and `diff` when it lands)
- [ ] Hspec parser test: every registered subcommand answers `--help` with its own text (structural — fails on a future helper-less registration)
- [ ] Check: `graphos subgraph --help` and `graphos cypher --help` show their own flags, not the top-level list

## 2. Cypher structured output (parallel-safe with 1)

### 2.1 Wire and extend the renderers
- [ ] Dispatch `--json` to the existing `renderCypherResultJSON` in the cypher success path; add `--format json|csv|tsv` (`--json` ≡ `--format json`, default tsv); reject conflicting flags
- [ ] CSV renderer in `UseCase/Query/Render.hs` with RFC 4180 quoting
- [ ] Hspec: format dispatch table; csv quoting cases; error path leaves stdout empty under `--json`
- [ ] Check: the four scenarios of the `cypher-query` delta

## 3. Subgraph seed modes (depends on 1 for its help text)

### 3.1 Seed resolution + hop expansion
- [ ] `subgraphOpts`: repeatable `--path`, `--seeds FILE`, stdin fallback (non-TTY, no other seed source), `--hops N` (default 1); `--config` mode untouched; seedless error naming all four forms
- [ ] Seed resolution (glob over `source_file`, comment lines in seed files) + undirected hop expansion in the subgraph use-case
- [ ] Hspec: path/file/stdin parity on a fixture; hop-1 expansion set; seedless error
- [ ] Check criteria first: the feedback's three originally-failing invocations now work or fail with guidance

### 3.2 Output + summary
- [ ] Contract-shaped output via `exportSubgraphJSON`; `--graphml` reuses the GraphML exporter; final summary line (nodes, edges, path, % of source retained)
- [ ] Check: extracted sub-graph round-trips into `graphos cypher --graph`

## 4. Graph diff (parallel-safe with 3)

### 4.1 Domain + UseCase
- [ ] Extend `Domain.Graph.Diff` with changed-node detection (`Map.intersectionWith`; changed = differing label, kind, signature or span); keep `graphDiff` name/shape backward compatible
- [ ] New `UseCase.Diff`: per-file rollups (added/removed/changed symbol lists), repeatable `--scope` glob filtering
- [ ] Hspec: changed-detection cases per field; scope filtering; identical graphs → empty diff

### 4.2 CLI + report formats
- [ ] `graphos diff OLD NEW [--scope P] [--json] [--canvas OUT]` in Parser/Main (registered with helper + example); markdown report (summary + per-file) on stdout; `--json` single document; errors on stderr, non-zero exit
- [ ] Hspec: report rendering golden files; JSON single-document property
- [ ] Check: the six scenarios of the `graph-diff-cli` delta

## 5. Canvas export (depends on 3 and 4 for entry points)

### 5.1 Shared writer
- [ ] Extract the vault canvas writer into `Infrastructure/Export/Canvas.hs`; add file-level aggregation, category classification (test/doc/config by path convention), change-status coloring, deterministic layout
- [ ] `--obsidian` vault export keeps using the shared writer (no behavior change)
- [ ] Hspec: golden `.canvas` files; determinism = byte-identical on repeated runs

### 5.2 Entry points
- [ ] `subgraph --canvas OUT` (category colors) and `diff --canvas OUT` (status colors)
- [ ] Check: canvas opens in Obsidian rendering file nodes + dependency edges (manual scenario)

## 6. Conformance (last)

### 6.1 Full pass
- [ ] `cabal build` (dev flag, -Werror) and `cabal test` green
- [ ] Walk every scenario across the five delta specs against the repo's own extraction; tick in the PR description
- [ ] Acceptance bar: confirm each of the feedback's three workaround scripts is now replaceable by one graphos command (subgraph / diff / --canvas)
- [ ] `openspec validate consumable-graph-outputs` passes
