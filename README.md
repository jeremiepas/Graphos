# Graphos

**Context graph builder** — turns a folder of code, docs, papers, and images into a navigable knowledge graph with community detection, so you (or an LLM) can query a codebase instead of re-reading it.

## What Graphos Does

Graphos extracts structure from your project and builds a graph: nodes are functions, classes, files, concepts, and documents; edges are `contains`, `imports`, `calls`, and inferred relationships. It then runs Leiden community detection to cluster related code, and produces an interactive HTML viewer, a queryable `graph.json`, and a plain-language `GRAPH_REPORT.md`.

**Extraction is tree-sitter-first by default** — no language server to install, works offline, and covers every language below out of the box. LSP (Language Server Protocol) is available as an opt-in override per extension when you want richer semantic info (hover, native cross-file references, real call hierarchies) and are willing to install and trust a language server. See [Tree-sitter vs. LSP](#tree-sitter-vs-lsp) below.

## Supported File Types

### Code (tree-sitter by default)

| Tier | Extensions | What you get |
|------|-----------|------------|
| Full symbols + resolved `imports` + `calls` | `.go` `.rs` | Class/function/method nodes, repo-resolved import edges to real files, name-matched call edges |
| Full symbols + `calls`, imports unresolved | `.ts` `.tsx` `.js` `.jsx` | Symbol nodes and call edges; import edges exist but target a specifier-derived placeholder, not a repo-verified file |
| Full symbols + resolved `imports`, no `calls` | `.rb` `.java` | Repo-resolved import edges; the call-matching mechanism only recognizes the `call_expression`/`new_expression` node names that Go/Rust/TS/JS grammars use — Ruby's/Java's call nodes are named differently, so no call edges |
| Full symbols, no `calls`, no resolved imports | `.py` `.pyw` `.php` `.kt` `.kts` `.pl` `.pm` `.psgi` `.cs` | Class/method/field nodes only — same `call_expression` naming gap, and none of these have a dedicated import resolver |
| Full symbols, Haskell's own import resolver, no `calls` | `.hs` `.lhs` | Module-qualified imports resolved via Haskell-specific logic, not the generic tree-sitter import path |
| Structure/data only | `.json` `.yaml` `.yml` `.sql` `.md` `.rst` `.adoc` | File- or document-level nodes, no symbols, no edges beyond containment |
| C/C++ family | `.cpp` `.hpp` `.cc` `.cxx` `.hh` `.hxx` | Full symbols via the `cpp` grammar; `#include` becomes an `Import` node but isn't resolved to a verified file |
| Grammar missing → stub only | `.c` `.h` `.nix` | One empty node per file ([#158](https://github.com/jeremiepas/Graphos/issues/158)) |
| No default extractor → stub unless you configure LSP | `.mjs` `.cjs` `.scala` `.swift` `.lua` `.zig` `.ex` `.exs` `.m` `.mm` `.jl` `.vue` `.svelte` `.dart` `.ps1` | Empty stub node unless you set `mode: lsp` for the extension in `graphos.yaml` and install the matching server |

`calls` edges are syntactic, name-based matching against a fixed pair of AST node names — not a semantic call graph, and currently only triggers for the four languages above whose grammars happen to use those exact node names. A language server's `callHierarchy` (opt-in LSP mode) is the only genuinely semantic cross-file call graph Graphos can produce, for any language with a server.

### Non-code

| Type | Extensions | What actually happens |
|------|-----------|------------|
| Docs | `.md` `.txt` `.rst` `.adoc` `.org` | Structural parsing — headers, `#tags`, `[[wikilinks]]`. Code↔doc links are deterministic (co-location, symbol mentions, path references) — **no LLM is involved**. |
| Office | `.docx` `.pptx` `.xlsx` `.doc` `.ppt` | Converted toward markdown and extracted structurally. `.doc`/`.ppt` support is minimal. Treat this path as best-effort, not a primary use case yet. |
| Papers | `.pdf` | Text extraction via `pdftotext` (poppler) plus regex-based structure heuristics (titles, numbered sections). Three granularity modes: `small` (titles only), `medium` (+ sections, default), `large` (+ paragraphs). No citation parsing. |
| Images | `.png` `.jpg` `.jpeg` `.webp` `.gif` | Real LLM vision analysis — **disabled by default**; set `vision.enabled: true` and point it at an OpenAI-compatible vision endpoint. |

There is no audio/video transcription today, despite what older copies of this README claimed — don't configure for it.

## Tree-sitter vs. LSP

| Aspect | tree-sitter (default) | LSP (opt-in override) |
|--------|-------------|---------------|
| Setup | None — grammar is compiled in | Install and trust a language server binary |
| Offline | Yes | No — needs the server running |
| Cross-file imports | Real for Go/Rust/Ruby/Java; placeholder-only for most others | Native, via the server's own project model |
| Call graph | Name-matched, same-file or import-resolved only | Semantic, via `callHierarchy`/`incomingCalls` |
| Hover/type info | Not available | Available |
| New/unsupported language | Needs a vendored or Hackage grammar binding | Just point `graphos.yaml`'s `lsp:` section at an installed server |

To opt a file extension into LSP mode:

```yaml
extractors:
  ".py":
    mode: lsp
    languageId: python
lsp:
  ".py":
    command: pyright-langserver
    args: ["--stdio"]
```

### LSP security notes

- Third-party LSP servers may execute project build tools — `jdtls` and `kotlin-language-server` can run Gradle/Maven on startup. Only run Graphos with LSP servers you trust, against projects you trust.
- To opt an extension *out* of LSP, set its command to an empty string in `graphos.yaml`:
  ```yaml
  lsp:
    ".java":
      command: ""
  ```

## Pipeline

```
detect() → extract() → build() → cluster() → infer() → analyze() → export()
```

Each stage is a pure function (Domain/UseCase layers have zero IO); side effects — file reads, LSP processes, LLM calls — live only in Infrastructure.

## Architecture

```
src/Graphos/
├── Domain/              -- Pure types and logic, no IO
│   ├── Types.hs         -- Node, Edge, Extraction, Confidence
│   ├── Graph.hs         -- Graph operations (add, merge, query, shortest path)
│   ├── Community.hs     -- Leiden community detection
│   ├── Analysis.hs      -- God nodes, surprising connections, suggested questions
│   ├── Query/Cypher/     -- openCypher/GQL parser + evaluator
│   └── Config/           -- graphos.yaml schema and defaults
│
├── UseCase/              -- Orchestration, still pure
│   ├── Pipeline/         -- Full pipeline, incremental updates, staging
│   ├── Extract/          -- tree-sitter + LSP + Haskell-specific extraction
│   ├── Cluster.hs        -- Community detection orchestration
│   ├── Report.hs         -- GRAPH_REPORT.md generation
│   ├── SelectContext.hs  -- Token-budgeted context selection for LLM callers
│   └── Query.hs          -- BFS/DFS graph search
│
└── Infrastructure/       -- IO boundary, all side effects here
    ├── LSP/              -- Language server client, protocol, capability detection
    ├── Extract/TreeSitter/ -- Grammar bindings, AST→graph conversion, import resolution
    ├── FileSystem/       -- Watcher (--watch), content-hash cache, ignore rules
    ├── Export/           -- JSON (JGF), HTML, Obsidian, Neo4j, Memgraph, community graph
    └── Server/           -- MCP stdio server, HTTP query API + static viewer
```

### Clean Architecture Principles

1. **Dependencies point inward**: Domain ← UseCase ← Infrastructure. Domain knows nothing about tree-sitter, LSP, IO, or any library.
2. **All domain logic is pure**: graph operations, community detection, analysis — testable without mocks.
3. **Extractors are adapters**: the domain receives extraction results; it doesn't know whether they came from tree-sitter or an LSP server.
4. **Standard output format**: `graph.json` is a versioned [JSON Graph Format (JGF)](https://jsongraphformat.info) document (`application/vnd.jgf+json`), with Graphos-specific data (communities, god nodes, cohesion) under `graph.metadata.graphos`. Legacy (pre-JGF) files keep loading during a deprecation window; `graphos migrate-graph <file>` upgrades them.

## Node Embeddings

Graphos supports node-level embeddings for semantic edge inference. When enabled, the pipeline generates a vector embedding for every node and writes them to an `embeddings.json` sidecar file alongside `graph.json`.

```bash
graphos . --embed          # or embedding.enabled: true in graphos.yaml
```

Embeddings are generated in **batches** (`embedding.batchSize`, default 64 texts per API request) with **bounded parallelism** (`embedding.concurrency`, default 1 = sequential). Identical node texts are embedded only once and their vectors shared.

**Bounded memory.** Vectors are held as compact unboxed `Vector Double` (~8 bytes/dim vs ~40+ for a boxed list), assigned incrementally per batch (no full fresh-table copy), and released after semantic-edge inference so clustering and export never hold the whole table live. Vectors persist in the `embeddings.json` sidecar (always written via a streaming, atomically-renamed path) and are re-loaded on demand.

```yaml
embedding:
  enabled: true
  model: nomic-embed-text
  batchSize: 64
  concurrency: 1
```

**Disk cache.** Results are cached under `graphos-out/cache/embeddings/` (one file per entry, keyed by `sha256(model <> text)`): a model change invalidates stale entries for free, and unchanged nodes cost zero API calls on reruns.

## Querying the Graph

### `graphos query` — heuristic graph search

```bash
graphos query "how does authentication work?"
graphos query "how does authentication work?" --dfs --budget 5000
```

This is a term-indexed BFS/DFS search over node labels, not natural-language understanding — it scores and ranks matches, reports a verdict and suggestions when nothing scores well, and respects a token `--budget` (default 2000). Five context-selection strategies (`Focused`, `ModuleLevel`, `CrossModule`, `Architectural`, `Exploratory`) pick how much surrounding context to include based on the query's apparent scope.

### `graphos cypher` — a real openCypher/GQL engine

```bash
graphos cypher "MATCH (n:Function)-[:calls]->(m) WHERE n.label CONTAINS 'login' RETURN n, m"
graphos cypher "CREATE (n:Note {label: 'reviewed'})" --write   # mutates graph.json (see caveat below)
```

Supports `MATCH`/`WHERE`/`RETURN` for reads and `CREATE`/`MERGE`/`SET`/`REMOVE`/`DELETE` for writes (behind `--write`, read-only otherwise). `kind` is a Cypher **label** (`MATCH (n:Module)`), not a property. Mutations made this way persist to `graph.json` with a timestamped backup, but **the next full extraction run overwrites them** — mutations aren't replayed into fresh extractions.

### Other graph lookups

```bash
graphos path "AuthModule" "Database"       # shortest path
graphos explain "RequestHandler"           # node + all its edges
graphos neighbors "RequestHandler" --depth 2
graphos symbols "AuthModule"               # exact-name lookup
```

All of these accept `--graph path/to/graph.json` (default `graphos-out/graph.json`).

### MCP server

```bash
graphos --mcp graphos-out/graph.json
```

Starts a real stdio JSON-RPC server (not a placeholder) exposing the graph to any MCP-compatible client. 13 tools: `query_graph`, `cypher_query`, `cypher_mutate`, `get_node`, `get_neighbors`, `get_community`, `god_nodes`, `graph_stats`, `shortest_path`, `bridge_nodes`, `select_context`, `add_conversation`, `conversation_history` — the last two give an MCP client persistent, cross-session Q&A memory under `graphos-out/memory/`.

### HTTP API

```bash
graphos serve --dir graphos-out --port 8080
```

Serves the static HTML viewer and a JSON query API: `GET /api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`, and `POST /api/cypher/mutate`.

## Multi-Source Graphs

Graphos can merge several repositories into one graph, with node IDs prefixed per source to avoid collisions. Configure it in `graphos.yaml`, not via a CLI flag:

```yaml
sources:
  - name: backend
    path: ../backend
  - name: frontend
    path: ../frontend
```

`--watch` then runs one file-watcher tree per source, and a community-label cache avoids re-labeling communities that didn't change on incremental runs.

## Watch Mode & Incremental Updates

```bash
graphos . --watch     # run once, then re-extract on file changes
graphos . --update    # one-shot incremental run against the last checkpoint
```

Both are real, fsnotify-backed features: changed files are re-extracted and merged into the existing graph, and community re-clustering is scoped to the communities actually affected, not the whole graph.

## Community Detection

Leiden community detection (`Domain/Community.hs`), real implementation, not a stub:

```bash
graphos . --resolution 1.0 --min-comm-size 3 --max-leiden-iterations 50
```

Higher `--resolution` produces more, smaller communities. `--edge-density sparse|normal|dense|maximum` tunes how aggressively co-location/semantic edges are added before clustering.

## Install

```bash
cabal install graphos
```

Or with stack:

```bash
stack install graphos
```

### Language Server Requirements (optional — only if you opt an extension into LSP mode)

```bash
npm install -g typescript-language-server typescript   # TypeScript/JS
pip install python-lsp-server                           # Python
go install golang.org/x/tools/gopls@latest              # Go
rustup component add rust-analyzer                       # Rust
cabal install haskell-language-server                     # Haskell
```

Run `graphos lservers` to see what Graphos finds on your `PATH`.

## Ignore Patterns

Graphos honours `.gitignore` and `.graphosignore` files to exclude build artifacts, dependencies, and other irrelevant files. Use `--ignore GLOB` to add additional patterns at runtime.

### `.graphosignore`

Create a `.graphosignore` file in your project root. Syntax matches `.gitignore`:

```gitignore
dist/
build/
target/
*.pdf
*.mp4
```

**Where it is read:** from the scan root directory (the directory passed to `graphos <DIR>`), not the current working directory.

**Match semantics:**
- Patterns match against scan-root-relative paths using normalized forward slashes
- `*` matches within a path component; `**` matches across components
- Leading `/` anchors to the scan root; trailing `/` matches directories only; `!` negates a pattern
- Comments (`#`) and blank lines are ignored

### `--ignore` flag

```bash
graphos . --ignore "**/vendor/**" --ignore "*.log"
```

Merged with `.gitignore`/`.graphosignore` patterns.

## Extraction Fidelity Harness

Ground-truth validation of extraction quality, built into the standard `cabal test` suite — no external interpreter needed.

| Component | Purpose | Invocation |
|-----------|---------|------------|
| `ImportEdgesSpec` | Precision/recall oracle for `imports` edges against the real filesystem | `cabal test --match ImportEdges` |
| `GraphCoverageSpec` | File-coverage accounting, grouped by ignore-rule class | `cabal test --match GraphCoverage` |
| `graphos subgraph` | Extract a pattern-selected subgraph from an existing `graph.json` | see below |

### `graphos subgraph`

Extracts a subgraph by selecting *core* files from named path patterns, expanding a *boundary* tier of files that import or are imported by a core file, and an *external* tier of package dependencies:

```bash
graphos subgraph --graph graphos-out/graph.json --config subgraph-config.json \
  --out graphos-out/subgraph.json
```

```json
{
  "subsystems": [
    { "name": "detect", "patterns": ["src/UseCase/Detect/**"] }
  ],
  "max_hops": 1,
  "include_derived": true
}
```

## OpenSpec Governance Tooling (for Graphos's own development workflow)

Graphos ships a few commands that verify its own spec-driven development process — useful if you adopt the same workflow, not needed to use Graphos as a graph builder:

```bash
graphos view changes              # OpenSpec change status dashboard
graphos speccheck --specs openspec   # dependency cycles, stale specs, coverage gaps
graphos intentcheck --specs intent.json --graph graphos-out/graph.json
graphos impact --before before.json --after after.json --gate
```

## CLI Reference

```bash
# Full pipeline
graphos .                              # current directory
graphos ./my-project --directed        # preserve edge direction
graphos ./my-project --no-viz          # skip HTML
graphos ./my-project --granularity file   # fine|function (default)|file
graphos ./my-project --ignore "**/vendor/**" --ignore "*.log"

# Exports
graphos ./my-project --obsidian        # Obsidian vault
graphos ./my-project --neo4j           # Cypher file
graphos ./my-project --neo4j-push bolt://localhost:7687   # live push
graphos ./my-project --memgraph-push bolt://localhost:7688
graphos ./my-project --community-graph # condensed community_graph.json

# Merge two previously-built graphs
graphos merge graphA.json graphB.json -o merged-out

# Upgrade a legacy graph.json to the current JGF envelope
graphos migrate-graph graphos-out/graph.json

# Push an already-built graph.json to Neo4j/Memgraph without re-extracting
graphos push --graph graphos-out/graph.json --uri http://localhost:7474
graphos push-memgraph --graph graphos-out/graph.json --uri bolt://localhost:7688

# Scaffold a graphos.yaml with documented defaults
graphos init
```

### Query Options

| Flag | Default | Description |
|------|---------|-------------|
| `--dfs` | bfs | Use DFS traversal instead of BFS |
| `--budget N` | 2000 | Token budget for query results |
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |

## What You Get

```
graphos-out/
├── graph.json             # JGF-format graph — query it, migrate it, parse it with any JGF tool
├── GRAPH_REPORT.md        # Summary, extraction coverage, communities, god nodes, bridges, questions
├── graph.html             # Interactive viewer (skip with --no-viz)
└── cache/                 # Content-hash cache — reruns only re-process changed files
```

Opt-in, by flag: `obsidian/` (`--obsidian`), `graph.cypher` (`--neo4j`), `memgraph.cypher` (`--memgraph`), `community_graph.json` (`--community-graph`), `embeddings.json` (`--embed`), `memory/` (conversation history via `graphos query` or the MCP server).

## License

MIT
