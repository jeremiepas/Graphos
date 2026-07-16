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

| Flag | Default | Description |
|------|---------|-------------|
| `--embed` | off | Generate vector embeddings via Ollama |