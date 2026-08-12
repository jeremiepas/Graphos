## Why

LLMs waste tokens re-reading entire codebases on every call because there is no persistent, structured representation of what matters. Developers lack navigable maps of how their code connects. Graphos solves this by extracting code structure via the Language Server Protocol, clustering it with Leiden community detection, and producing persistent, queryable context that saves tokens per LLM call (PRD §1).

## What Changes

- **Seven-stage pipeline**: detect → extract → build → cluster → infer → analyze → export (PRD §3.1)
- **LSP-based extraction**: Auto-detect 30+ language servers, extract AST + symbols + cross-file references (PRD §6)
- **Leiden community detection**: Pure Haskell implementation with resolution tuning, merge strategies, cohesion scoring (PRD §5)
- **Context selection for LLMs**: Five strategies by query complexity, token budget allocation, compact markdown output (PRD §7)
- **MCP server**: stdio JSON-RPC 2.0, 11 tools, non-polluting chat memory via community 0 (PRD §8)
- **Neo4j/Memgraph push**: Three push modes (FullPush/SubgraphPush/CommunityPush), auto-selection by graph size (PRD §9)
- **Multi-format input**: Code, docs, papers, images, video/audio → unified knowledge graph (PRD §11)
- **Observability**: OTLP traces + IORef metrics store + OTLP log bridge + debug-trace JSONL (PRD §10)
- **Nine export formats**: graph.json, graph.html, report, community_graph.json, Obsidian, Neo4j, Memgraph, SVG, GraphML (PRD §12)
- **Incremental pipeline**: SHA256 cache + checkpoint-resume, only changed files re-extracted (PRD §3.4)
- **16 product workflows**: Full pipeline, incremental, watch, query, path, explain, context selection, MCP, merge, ingest, community labeling, Neo4j push, Memgraph push, observability, config init, LSP discovery

## Capabilities

### New Capabilities

**Domain types** (data structures and module interfaces from PRD §4–5, §7, §10, §14):

- `domain-types`: All Domain.Types.* modules (Node, Edge, Graph, Pipeline, Analysis, Ingest), Domain.Config, Domain.Context, Domain.Extraction, Domain.Community, Domain.Graph.Core/FGL/Query/Analysis/Diff/Index, Domain.Analysis, Domain.Labeling — exact data types, function signatures, and module interfaces

**Capability specs** (implementation contracts from PRD §3–12):

- `full-pipeline`: Seven-stage pipeline stages, Build adjacency, FGL adapter, Graph.Query/Analysis/Diff/Index, Extraction validation — PRD §3–4
- `lsp-extraction`: ServerMap, Transport, Protocol, Client, Capabilities, CapabilityParse, Extraction, TreeSitter — PRD §6
- `community-detection`: Leiden algorithm, Resolution, merge, cohesion, representatives — PRD §5
- `context-selection`: SelectContext strategies, FormatContext markdown, Conversation community 0 — PRD §7
- `mcp-server`: JSON-RPC server, 11 tools, chat memory, conversation persistence — PRD §8
- `neo4j-integration`: Three push modes, representatives, streaming — PRD §9
- `multi-format-input`: Ingest, URL detection, SHA256 cache, OfficeConvert, merge — PRD §11
- `observability`: OTLP traces, MetricsStore, Prometheus, log bridge, debug JSONL, env vars — PRD §10

**Workflow specs** (user-facing behavior mapped 1:1 to workflows 01–16):

- `01-full-pipeline`: Stages 1–7 Detect→Extract→Build→Cluster→Infer→Analyze→Export, skip-clustering mode
- `02-incremental-pipeline`: SHA256 cache, checkpoint resume, unchanged file reuse
- `03-watch-mode`: fsnotify watcher, debounce, .gitignore, Ctrl+C stop
- `04-query`: BFS/DFS traversal, inverted index matching, token budget
- `05-path`: Shortest path between two nodes, FGL esp, directed/undirected
- `06-explain`: Full node details, all neighbors, community, bridge status
- `07-context-selection`: Five strategies, budget allocation, compact markdown, chat history filtering
- `08-mcp-server`: stdio startup, 11 tools, community 0, conversation persistence
- `09-merge`: Two-graph merge, dedup, re-cluster
- `10-ingest`: Single file/URL, type detection, embeddings, IngestIndex
- `11-community-labeling`: LLM batch labeling, config, cost
- `12-neo4j-push`: Three modes, representatives, streaming, Cypher batching
- `13-memgraph-push`: Bolt protocol, same modes, ephemeral storage
- `14-observability`: OTLP traces, MetricsStore, Prometheus, logs, debug JSONL, env vars
- `15-config-init`: graphos init, cascade resolution
- `16-lsp-discovery`: graphos lservers, PATH scan, capability check

### Modified Capabilities

_(None — this is the initial product specification)_

## Impact

- **Codebase**: Full `src/Graphos/` tree (Domain/, UseCase/, Infrastructure/) — all three clean architecture layers
- **Dependencies**: FGL, lsp-types, aeson, optparse-applicative, hspec, QuickCheck, stm, async, hs-opentelemetry-sdk, text, containers, bytestring, vector
- **APIs**: CLI interface (`graphos` command with 9 sub-commands), MCP server protocol (11 tools), HTTP static server
- **Output**: `graphos-out/` directory (graph.json, graph.html, report, cache, memory, debug, obsidian, community_graph.json)
- **Configuration**: `graphos.yaml` with LSP mappings, file extension categories, observability, Neo4j/Memgraph settings (PRD §14)
- **Build**: GHC 9.10, -Wall -Wcompat -Werror (dev flag), Cabal 3.0, Nix dev shell

## PDCA Cycle

- **Plan**: Graphos reduces LLM token waste by 5–10x per query via structured, persistent knowledge graphs. Success is measured by: (1) `cabal build` succeeds with zero warnings, (2) `cabal test` passes all Hspec + QuickCheck specs, (3) pipeline extracts ≥10 languages via auto-detected LSP servers, (4) context selection returns ≤3000 tokens for architectural queries, (5) Leiden clustering completes within 30s for 100k-node graphs (PRD §16.1).
- **Do**: Implement all eight capabilities across Domain/UseCase/Infrastructure layers following clean architecture. Build pipeline stages as pure functions with IO at the Infrastructure boundary only.
- **Check**: Verify each capability against its spec scenarios: `cabal build` (zero warnings), `cabal test` (all pass), manual verification of graph.json output structure, MCP tool responses, Neo4j push modes, observability stack integration, and performance targets from PRD §16.1.
- **Act**: Standardize successful patterns into the codebase. For gaps: record findings, adjust resolution parameters or extraction strategies, and begin the next PDCA iteration targeting identified weaknesses (e.g., embedding-based search, mutable graph via MCP per PRD §18).