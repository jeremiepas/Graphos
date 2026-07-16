# 01 — Full Pipeline

> `graphos <path>`

The primary Graphos workflow. Scans a directory, extracts code structure via LSP, builds a knowledge graph, detects communities with Leiden, infers additional edges, analyzes the result, and exports in multiple formats.

---

## Stages

```
┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐
│  DETECT  │──▶│ EXTRACT  │──▶│  BUILD   │──▶│ CLUSTER  │
│    1     │   │    2     │   │    3     │   │    4     │
└──────────┘   └──────────┘   └──────────┘   └────┬─────┘
                                                   │
                                                   ▼
┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐
│  EXPORT  │◀──│ ANALYZE  │◀──│RE-CLUSTER│◀──│  INFER   │
│    7     │   │    6     │   │    5b    │   │    5    │
└──────────┘   └──────────┘   └──────────┘   └──────────┘
```

---

## Stage 1: Detect

**Module**: `UseCase/Detect.hs`

**Input**: Filesystem path

**Process**:
- Recursively scan directory for files by extension
- Categorize into: CodeFiles, DocFiles, PaperFiles, ImageFiles, VideoFiles
- Respect `.gitignore` patterns and sensitive file exclusion rules

**Output**: `Detection { fileCategories, totalFiles, warnings }`

**Why**: Determines which extractors to run. Code files go to LSP, docs to LLM, papers to PDF mining, images to vision, video/audio to transcription.

---

## Stage 2: Extract

**Module**: `UseCase/Extract.hs`

**Input**: Detection + PipelineConfig

**Process** (parallel per language, `cfgThreads`):

```
Code Files ──┬─▶ LSP Extraction      (Infrastructure/LSP/*.hs)
             │    Spawn server → initialize → documentSymbol → references → shutdown
             │
             ├─▶ Tree-sitter Extraction (Infrastructure/Extract/TS/*.hs)
             │    Run tree-sitter CLI → parse CST → convert to Domain types
             │
             └─▶ Stub Extraction (one node per file, last resort)

.hs Files  ──▶ UseCase/Extract/Haskell.hs (specialized import/decl parsing)
.md Files  ──▶ UseCase/Extract/Markdown.hs (headings + wiki-links)
Papers     ──▶ PDF citation mining + concept extraction
Images     ──▶ LLM vision (descriptions + relationships)
Video/Audio▶ Whisper transcription → LLM extraction
```

**SHA256 Cache**: Each file's hash is checked. If unchanged since last run, extraction is skipped.

**Output**: `Extraction { nodes: [Node], edges: [Edge] }`

---

## Stage 3: Build

**Module**: `UseCase/Build.hs`

**Input**: List of Extractions + directed flag

**Process**:
- Merge all extractions into a single graph
- Deduplicate nodes by NodeId, edges by (source, target)
- Build forward and backward adjacency maps

**Output**: `LabeledGraph { gNodes: Map NodeId Node, gEdges: Map (NodeId,NodeId) Edge }`

**Checkpoint**: Saved to `graphos-out/cache/graph.checkpoint.json` for resume capability.

**Why separate from extract**: Build is pure — testable without any IO. Extraction is IO-heavy (LSP servers, LLM calls). Separating them keeps the pure logic isolated.

---

## Stage 4: Cluster

**Module**: `UseCase/Cluster.hs` + `Domain/Community.hs`

**Input**: LabeledGraph + Resolution

**Process**:
1. Convert to FGL graph (`Domain.Graph.FGL.toFGL`)
2. Run Leiden community detection (3 phases: local moving → refinement → aggregation)
3. Merge small communities below `resMinSize` using `resMergeInto` strategy
4. Compute cohesion scores for all communities

**Output**: `(CommunityMap, CohesionMap)`

**Tuning by graph size**:

| Graph Size | gamma | minSize | maxIterations |
|-----------|-------|---------|---------------|
| < 1k nodes | 1.0 | 3 | 50 |
| 1k–10k | 0.8 | 5 | 30 |
| 10k–100k | 0.5 | 10 | 20 |
| 100k+ | 0.3–0.5 | 10–20 | 10–20 |

---

## Stage 5: Infer

**Module**: `UseCase/Infer.hs`

**Input**: EdgeDensity + LabeledGraph + CommunityMap

**Process**: Add edges based on density setting:

| Density | What Gets Inferred |
|---------|-------------------|
| Sparse | No inferred edges |
| Normal | Bridge edges + transitive dependencies |
| Dense | + shared context edges |
| Maximum | + lower thresholds for shared context |

**Output**: Enriched LabeledGraph (original + inferred edges)

**Why**: Raw LSP extraction misses relationships that are structurally implied. Inference recovers bridge edges between communities and transitive dependencies that LSP doesn't report.

---

## Stage 5b: Re-Cluster

**Process**: Run Leiden again on the enriched graph. Inferred edges can shift community boundaries.

---

## Stage 6: Analyze

**Module**: `UseCase/Analyze.hs` + `Domain/Analysis.hs` + `Domain/Graph/Analysis.hs`

**Input**: Enriched graph + CommunityMap + CohesionMap

**Process**:
- Compute community statistics (size, density, cohesion)
- Identify **god nodes**: highest-degree nodes per community
- Find **surprising connections**: edges between distant communities
- Detect **bridge nodes**: articulation points between communities
- Generate **suggested questions** for LLM exploration

**Output**: `Analysis { godNodes, surprisingConnections, suggestedQuestions, bridgeNodes, communityStats }`

**Why**: Analysis turns raw graph data into human- and LLM-consumable insights. God nodes tell you what matters. Bridge nodes tell you what connects modules. Surprising connections reveal hidden coupling.

---

## Stage 7: Export

**Module**: `UseCase/Export.hs` + `Infrastructure/Export/*.hs`

**Input**: Graph + Analysis + Config + Detection

**Process**: Generate all requested output formats.

**Always produced**:
| Format | File | Purpose |
|--------|------|---------|
| JSON | `graph.json` | Persistent, queryable knowledge graph |
| Report | `GRAPH_REPORT.md` | Human-readable audit |
| HTML | `graph.html` | Interactive vis.js visualization (unless `--no-viz`) |

**Optional formats**:
| Format | Flag | Purpose |
|--------|------|---------|
| Community graph | `--community-graph` | Community-level graph for LLM navigation |
| Obsidian vault | `--obsidian` | Markdown files with wiki-links |
| Neo4j Cypher | `--neo4j` | Graph database push |
| Memgraph | `--memgraph` | In-memory graph database push |
| SVG | `--svg` | Static visualization |
| GraphML | `--graphml` | Gephi/yEd format |

**Post-export**: Checkpoint file is removed. `graph.json` is the authoritative artifact.

---

## Skip-Clustering Mode

With `--no-cluster`, the pipeline runs stages 1–3, skips 4–5, and goes directly to report + export. Useful when only the raw graph structure is needed without community assignments.

---

## Configuration

| Flag | Default | Affects |
|------|---------|--------|
| `--directed` | undirected | Build stage: preserve edge direction |
| `--no-viz` | viz on | Export stage: skip HTML generation |
| `--no-cluster` | cluster on | Skip stages 4–5 |
| `--resolution N` | 1.0 | Cluster stage: Leiden gamma |
| `--min-comm-size N` | 3 | Cluster stage: minimum community size |
| `--max-leiden-iterations N` | 50 | Cluster stage: max iterations |
| `--community-graph` | off | Export stage: community-level graph |
| `--edge-density N` | 0.0 | Infer stage: edge inference density |
| `--obsidian` | off | Export stage: Obsidian vault |
| `--neo4j` | off | Export stage: Neo4j push |
| `--svg` | off | Export stage: SVG export |
| `--graphml` | off | Export stage: GraphML export |
| `--label` | off | Post-cluster: LLM community labeling |