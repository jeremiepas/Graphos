## ADDED Requirements

### Requirement: Workflow 10 — single file/URL ingestion with embeddings
Module `Graphos.UseCase.Ingest` SHALL export: `ingestFile :: FilePath -> GraphosConfig -> IO IngestResult`. CLI: `graphos ingest <file>`. Auto-detect by extension/URL pattern: code (.hs/.py/.ts) → LSP/tree-sitter/stub; docs (.md/.txt) → LLM extraction; papers (.pdf) → citation mining + concepts; images (.png/.jpg) → LLM vision; video/audio (.mp4/.mp3) → Whisper transcription → LLM. URL patterns: twitter.com → TwitterUrl, arxiv.org → ArxivUrl, .pdf URL → PdfUrl, images → ImageUrl, youtube.com → YoutubeUrl, other → GenericWeb. With `--embed`: generate vector embeddings via local Ollama model, store in `IngestIndex` at `graphos-out/index.json`. Module `Graphos.UseCase.IngestIndex` manages embedding index: O(1) lookup by NodeId, cosine similarity search, right-biased merge on collision. (PRD §11, workflow 10)

#### Scenario: Ingest a markdown file
- **WHEN** `ingestFile` is called on a `.md` file
- **THEN** it SHALL extract headings + concepts + wiki-links as nodes and edges via LLM

#### Scenario: Ingest with embedding
- **WHEN** `--embed` is set
- **THEN** each extracted node SHALL have vector embedding stored in `graphos-out/index.json`

#### Scenario: Ingest a YouTube URL
- **WHEN** `ingestFile` is called on a youtube.com URL
- **THEN** it SHALL transcribe audio via Whisper, then extract concepts via LLM