# Graphos

**Context graph builder** — uses the Language Server Protocol to extract code as a graph, consolidate knowledge into a context graph, and save it with your project — so you use fewer tokens per LLM call.

## What Graphos Does

Graphos takes any folder of code, docs, papers, and images and builds a navigable knowledge graph with community detection. It produces interactive HTML, queryable JSON, and a plain-language audit report.

The key innovation: **LSP to generate context graphs** and create context graphs to optimise LLM context. Graphos connects to any language server. If a language has an LSP server, Graphos can extract its structure. That means:

- TypeScript, JavaScript, Python, Go, Rust, Java, C#, Haskell, Erlang, Zig — **all supported**
- Elm, PureScript, Idris, Agda — **all supported**
- Your custom DSL with an LSP — **supported**
- Every new language server that ships — **automatically supported**

## Supported File Types

| Type | Extensions | Extraction |
|------|-----------|------------|
| Code | `.py` `.ts` `.js` `.jsx` `.tsx` `.go` `.rs` `.java` `.c` `.cpp` `.h` `.hpp` `.rb` `.cs` `.kt` `.kts` `.scala` `.php` `.swift` `.lua` `.zig` `.ps1` `.ex` `.exs` `.m` `.mm` `.jl` `.vue` `.svelte` `.dart` `.hs` `.lhs` | AST via tree-sitter + call-graph (cross-file for all languages) + docstring/comment rationale + LSP |
| Docs | `.md` `.txt` `.rst` `.adoc` `.org` | Concepts + relationships + design rationale via LLM |
| Office | `.docx` `.xlsx` | Converted to markdown then extracted via LLM |
| Papers | `.pdf` | Citation mining + concept extraction |
| Images | `.png` `.jpg` `.jpeg` `.webp` `.gif` | LLM vision — screenshots, diagrams, any language |
| Video/Audio | `.mp4` `.mov` `.mkv` `.webm` `.avi` `.m4v` `.mp3` `.wav` `.m4a` `.ogg` | Transcribed locally with faster-whisper, transcript fed into LLM extraction |

## Pipeline

```
detect() → extract() → build() → cluster() → infer() → analyze() → export()
```

Each stage is a pure function. No shared state, no side effects outside `graphos-out/`.

## Architecture

```
src/Graphos/
├── Domain/           -- Pure types, no IO
│   ├── Types.hs      -- Node, Edge, Extraction, Confidence
│   ├── Graph.hs      -- Graph operations (add, merge, query, shortest path)
│   ├── Community.hs  -- Leiden community detection
│   ├── Analysis.hs   -- God nodes, surprising connections, suggested questions
│   └── Extraction.hs -- Extraction schema, validation
│
├── UseCase/          -- Orchestration, still pure
│   ├── Pipeline.hs   -- Full pipeline orchestration
│   ├── Detect.hs    -- File detection
│   ├── Extract.hs    -- LSP extraction + Haskell stub fallback
│   ├── Build.hs     -- Graph construction from extractions
│   ├── Cluster.hs   -- Community detection
│   ├── Analyze.hs   -- Analysis orchestration
│   ├── Report.hs    -- Report generation
│   ├── Export.hs    -- Export orchestration
│   ├── Query.hs     -- Graph querying (BFS, DFS, shortest path)
│   └── Infer.hs     -- Edge inference (community bridges, transitive deps)
│
└── Infrastructure/   -- IO boundary, all side effects here
    ├── LSP/
    │   ├── Client.hs      -- Connect to language servers
    │   ├── Protocol.hs    -- LSP JSON-RPC protocol types
    │   └── Capabilities.hs -- Language server capability detection
    ├── FileSystem/
    │   └── Watcher.hs     -- File watching for --update
    ├── Export/
    │   ├── JSON.hs        -- graph.json output
    │   ├── HTML.hs        -- graph.html (interactive vis.js)
    │   ├── Obsidian.hs    -- Obsidian vault
    │   ├── Neo4j.hs       -- Cypher generation
    │   ├── GraphML.hs     -- GraphML for Gephi/yEd
    │   ├── SVG.hs         -- Static SVG export
    │   └── Report.hs      -- GRAPH_REPORT.md
    └── Server/
        └── MCP.hs         -- MCP stdio server
```

### Clean Architecture Principles

1. **Dependencies point inward**: Domain ← UseCase ← Infrastructure. Domain knows nothing about LSP, IO, or any library.
2. **All domain logic is pure**: Graph operations, community detection, analysis — all pure functions. Testable without mocks.
3. **LSP is an adapter**: The domain doesn't know about LSP. It just receives extraction results. The LSP client adapter produces those results.
4. **Standard output format**: `graph.json` for interoperability with visualization tools and queries.

## Why LSP Instead of tree-sitter?

| Aspect | tree-sitter | LSP (Graphos) |
|--------|-------------|---------------|
| Language support | 25 hardcoded grammars | Any language with an LSP server |
| New language | Add grammar + recompile | Just install the LSP server |
| Semantic info | Syntax only (AST) | Symbols, references, call hierarchy, type info |
| Cross-file refs | Second-pass inference | Native via LSP `references`/`callHierarchy` |
| Hover/docs | Not available | Available via LSP `hover` |
| Maintenance | Grammar per language | Zero — LSP servers maintained by language teams |
| Offline | Works without language server | Requires LSP server installed |

## Install

```bash
cabal install graphos
```

Or with stack:

```bash
stack install graphos
```

### Language Server Requirements

Graphos auto-detects installed language servers. Install the ones you need:

```bash
# Common language servers (examples)
npm install -g typescript-language-server typescript   # TypeScript/JS
npm install -g vscode-langservers-extracted             # HTML/CSS/JSON
pip install python-lsp-server                           # Python
go install golang.org/x/tools/gopls@latest             # Go
rustup component add rust-analyzer                      # Rust
cabal install haskell-language-server                    # Haskell
```

## Ignore Patterns

Graphos honours `.gitignore` and `.graphosignore` files to exclude build artifacts, dependencies, and other irrelevant files. Use `--ignore GLOB` to add additional patterns at runtime.

### `.graphosignore`

Create a `.graphosignore` file in your project root to declare patterns that should always be excluded. Syntax matches `.gitignore`:

```gitignore
# Exclude build outputs
dist/
build/
target/

# Exclude large binary assets
*.pdf
*.mp4
```

**Where it is read:** `.graphosignore` is read from the scan root directory (the directory passed to `graphos scan <DIR>` or the directory argument to `graphos`), not the current working directory.

**Match semantics:**
- Patterns match against scan-root-relative paths using normalized forward slashes
- Backslashes are converted to forward slashes; `.` and `..` path components are resolved
- `*` matches within a single path component; `**` matches across components
- Leading `/` anchors to the scan root; leading `**/` matches any prefix
- Trailing `/` matches directories only; `!` negates a pattern
- Comments (`#`) and blank lines are ignored

### `--ignore` flag

Pass additional patterns via the CLI. Can be repeated for multiple patterns:

```bash
graphos . --ignore "**/vendor/**" --ignore "*.log"
```

Patterns are merged with `.gitignore` and `.graphosignore` patterns. File-level patterns (e.g., `*.log`) match against the basename; directory patterns (e.g., `vendor/`) match against path components. CLI patterns are applied in addition to any patterns from `.graphosignore` files.

## Extraction Fidelity Harness

The harness validates the fidelity of extraction against ground truth from the source files and
gives users a path/taxonomy-driven subgraph facility. All three components are part of the
standard `graphos` build and `cabal test` — no external interpreter or runtime is required.

| Component | Purpose | Invocation |
|-----------|---------|------------|
| `ImportEdgesSpec` | On-disk oracle for `imports` edges (precision/recall, gap listings) | `cabal test --match ImportEdges` |
| `GraphCoverageSpec` | File coverage accounting grouped by ignore-rule class | `cabal test --match GraphCoverage` |
| `graphos subgraph` | Extract a pattern-selected subgraph from a `graph.json` | `graphos subgraph --graph <g.json> --config <cfg.json> --out <out.json>` |

**Exit codes**: the Hspec specs pass with exit code 0 and fail with a non-zero exit code when
precision/recall (imports) or any unexplained file (coverage) drops below the gate. The
`graphos subgraph` command exits 0 on success, 1 when `--config` is required but missing or the
config/graph files cannot be parsed.

### ImportEdgesSpec

Scans a repository on disk, resolves every import/re-export specifier to a file, and compares the
resulting pair set with the `imports` edges in a `graph.json`. It reports the ground-truth pair
count, the graph edge count, and the precision/recall gaps as explicit `MISSING`/`EXTRA` pair
listings. The spec fails when precision or recall drops below the threshold (default `0.99`).

```bash
cabal test --match ImportEdges
```

### GraphCoverageSpec

Compares the source files on disk with the files present in a `graph.json` and groups any missing
files by the ignore-rule class that most plausibly explains them: root-anchored build output,
depth-independent tooling, `.gitignore`, or unexplained. The spec fails when any file is
unexplained, so the "unexplained" bucket can be fed back into gitignore parsing.

```bash
cabal test --match GraphCoverage
```

### `graphos subgraph`

Extracts a subgraph from an existing `graph.json` by selecting *core* files from path patterns
grouped into named subsystems, expanding a *boundary* tier of files that import a core file or
are imported by one, and an *external* tier of package dependencies. Output conforms to the
`graph.json` contract and is directly consumable via `--graph` (query/explain/neighbors). Every
node carries `tier`/`subsystem`/`layer` metadata and every edge carries a `provenance` marker
(`source` or `derived`).

Flags:

| Flag | Default | Description |
|------|---------|-------------|
| `--graph PATH` | `graphos-out/graph.json` | Source graph to extract from |
| `--config PATH` | — (required) | Subsystem patterns JSON |
| `--out, -o PATH` | `graphos-out/subgraph.json` | Output graph path |
| `--boundary-hops N` | `1` | Import-graph BFS depth for the boundary tier |
| `--no-derive` | derive enabled | Disable deriving `imports` edges from `Import` nodes |

```bash
graphos subgraph --graph graphos-out/graph.json --config subgraph-config.json \
  --out graphos-out/subgraph.json
graphos query "auth" --graph graphos-out/subgraph.json
graphos explain "RequestHandler" --graph graphos-out/subgraph.json
graphos neighbors "RequestHandler" --graph graphos-out/subgraph.json
```

Config schema (`--config`):

```json
{
  "subsystems": [
    { "name": "detect", "patterns": ["src/UseCase/Detect/**"] },
    { "name": "ignore", "patterns": ["src/Infrastructure/FileSystem/Ignore*"] }
  ],
  "max_hops": 1,
  "include_derived": true
}
```

```bash
# Full pipeline on current directory
graphos .

# Specific folder
graphos ./my-project

# Directed graph (preserves edge direction)
graphos ./my-project --directed

# Skip visualization
graphos ./my-project --no-viz

# Incremental update (only changed files)
graphos ./my-project --update

# Watch mode
graphos ./my-project --watch

# Additional ignore patterns
graphos ./my-project --ignore "**/vendor/**" --ignore "*.log"

# Query the knowledge graph (natural language)
graphos query "how does authentication work?"
graphos query "how does authentication work?" --dfs
graphos query "how does authentication work?" --budget 5000
graphos query "how does authentication work?" --graph path/to/graph.json

# Find shortest path between two nodes
graphos path "AuthModule" "Database"
graphos path "AuthModule" "Database" --graph path/to/graph.json

# Explain a node (show all connections)
graphos explain "RequestHandler"
graphos explain "RequestHandler" --graph path/to/graph.json

# List available LSP servers
graphos lservers

# Serve HTML visualization over HTTP
graphos serve --dir graphos-out --port 8080

# MCP server
graphos --mcp graphos-out/graph.json

# Export formats
graphos ./my-project --obsidian
graphos ./my-project --neo4j
graphos ./my-project --graphml
graphos ./my-project --svg
```

### Query Options

| Flag | Default | Description |
|------|---------|-------------|
| `--dfs` | bfs | Use DFS traversal instead of BFS |
| `--budget N` | 2000 | Token budget for query results |
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json file |

## What You Get

```
graphos-out/
├── graph.html       # Interactive graph - click nodes, search, filter by community
├── GRAPH_REPORT.md  # God nodes, surprising connections, suggested questions
├── graph.json       # Persistent graph - query weeks later without re-reading
└── cache/           # SHA256 cache - re-runs only process changed files
```

## License

MIT