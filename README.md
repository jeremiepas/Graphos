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

## Extraction Fidelity Harness

These Haskell modules validate the fidelity of the tree-sitter extraction against ground truth
from the source files. They are part of the acceptance gate for extraction fixes and compile as
part of the standard `cabal build` / `cabal test` — no external interpreter required.

| Component | Description | Invocation |
|-----------|-------------|------------|
| `ImportEdgesSpec` | Validates `imports` edges (precision/recall) | `cabal test --match "/Fidelity/ImportEdges/"` |
| `GraphCoverageSpec` | Reports file coverage in the graph | `cabal test --match "/Fidelity/GraphCoverage/"` |
| `graphos subgraph` | Extracts a pattern-selected subgraph | `graphos subgraph --graph <g.json> --config <cfg.json> --out <out.json> [--boundary-hops N] [--no-derive]` |

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