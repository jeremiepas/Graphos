# 10 — Ingest

> `graphos ingest <file>`

Ingest a single file or URL into an existing knowledge graph, with optional vector embeddings for semantic search.

---

## Flow

```
┌──────────────────────────────────────────────────────────────┐
│                     INGEST FLOW                              │
│                                                              │
│  graphos ingest src/Auth.hs                                 │
│  graphos ingest https://arxiv.org/abs/2401.12345            │
│       │                                                      │
│       ▼                                                      │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Detect type (file extension / URL pattern)         │  │
│  │                                                      │  │
│  │  File types:                                         │  │
│  │   .hs/.py/.ts → code (LSP/TS/LLM)                  │  │
│  │   .md/.txt     → doc (LLM)                          │  │
│  │   .pdf         → paper (citation mining)            │  │
│  │   .png/.jpg    → image (LLM vision)                 │  │
│  │   .mp4/.mp3    → video/audio (Whisper + LLM)        │  │
│  │                                                      │  │
│  │  URL types:                                         │  │
│  │   twitter.com  → TwitterUrl                         │  │
│  │   arxiv.org    → ArxivUrl                           │  │
│  │   .pdf         → PdfUrl                             │  │
│  │   .png/.jpg    → ImageUrl                           │  │
│  │   youtube.com  → YoutubeUrl                         │  │
│  │   other        → GenericWeb                          │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Extract (same methods as full pipeline)            │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Build single-file graph → Cluster (Leiden)         │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Optional: Generate embeddings (--embed)           │  │
│  │  → Local Ollama model (e.g. nomic-embed-text)      │  │
│  │  → Store in IngestIndex (index.json)               │  │
│  └──────────────┬───────────────────────────────────────┘  │
│                 │                                            │
│                 ▼                                            │
│  Export to graphos-out/ + update index                       │
└──────────────────────────────────────────────────────────────┘
```

---

## URL Auto-Detection

The ingest workflow detects URL type from the URL string:

| Pattern | Type | Processing |
|---------|------|-----------|
| `twitter.com` / `x.com` | TwitterUrl | Fetch + extract as document |
| `arxiv.org` | ArxivUrl | PDF download + citation mining |
| `.pdf` URL | PdfUrl | Direct PDF processing |
| `.png` / `.jpg` URL | ImageUrl | Download + LLM vision |
| `youtube.com` / `youtu.be` | YoutubeUrl | Transcribe + extract |
| Other | GenericWeb | Fetch HTML + extract as document |

---

## Embedding Generation

With `--embed`, Graphos generates vector embeddings for ingested nodes using a local Ollama model:

```
File → Extract nodes → For each node:
                         → Generate text embedding (Ollama API)
                         → Store in IngestIndex (index.json)
```

Embeddings enable semantic similarity search via cosine distance, complementing the keyword-based inverted index used by query and context selection.

---

## IngestIndex

The `IngestIndex` persists at `graphos-out/index.json`:

- **Lookup**: O(1) by NodeId
- **Search**: cosine similarity search when embeddings exist
- **Merge**: right-biased merge on NodeId collision
- **Persistence**: JSON file, loadable on subsequent runs

---

## When to Use

| Scenario | Use Full Pipeline | Use Ingest |
|----------|------------------|-----------|
| First run on a codebase | Yes | No |
| Add a single new file | No | Yes |
| Add a URL reference | No | Yes |
| Ingest a paper | No | Yes |
| Bulk file changes | Yes (--update) | No |

---

## Configuration

Ingest behavior is controlled by the `ingest:` section in `graphos.yaml`:

```yaml
ingest:
  embed: true                    # generate embeddings by default
  resolution: 0.8               # community resolution (higher = smaller communities)
  min_comm_size: 2              # minimum nodes per community
  max_leiden_iter: 15           # max Leiden iterations
  merge: true                   # merge with existing graph (false = standalone)
  deduplicate: true             # skip unchanged files (by SHA256)
  index_path: "graphos-out/index.json"
  url:
    timeout: 30                 # seconds
    retry: 3
    user_agent: "Graphos/0.1"
  categories:
    code:
      embed: true               # code files embed by default
      granularity: 0.5
    images:
      embed: false              # images don't embed by default
    videos:
      embed: false              # videos don't embed by default
    doc_files:
      embed: true
    paper_files:
      embed: true
```

### Resolution Order

Configuration is resolved in this priority order (higher overrides lower):

1. **Defaults** — `defaultIngestConfig` values
2. **Global config** — `~/.graphos/config.yaml` `ingest:` section
3. **Project config** — `graphos.yaml` `ingest:` section (always wins for scalars/booleans)
4. **CLI flags** — `--embed` / `--no-embed` override project-level `embed`

### Category-Level Overrides

Per-category settings in `ingest.categories.<type>` override top-level defaults:

```yaml
ingest:
  embed: false                  # default: no embeddings
  categories:
    code:
      embed: true               # but code files DO embed
```

### Deduplication

When `deduplicate: true`, Graphos computes the SHA256 hash of each file and checks `index.json` for a matching hash. If the hash matches, extraction is skipped entirely.

### Merge vs Standalone

- **`merge: true`** (default) — new nodes/edges are merged into the existing graph
- **`merge: false`** — creates a standalone subgraph for this file only