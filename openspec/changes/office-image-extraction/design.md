## Context

Graphos extracts structure from code (LSP/tree-sitter) and text docs (markdown headers), producing a knowledge graph. Two large categories of enterprise knowledge are missing: office documents (DOCX/PPTX/XLSX) and images (PNG/JPG/WebP/GIF). Office documents are ZIP archives containing XML — parseable in pure Haskell. Images require a multimodal LLM to extract semantic content. The existing `OfficeConvert.hs` has stubs for DOCX/XLSX. The extraction pipeline (`Extract.hs`) already handles file routing via categories. The LLM client (`OpenAI.hs`) uses curl for API calls. The config system (`Config.hs`) supports provider/model/apiKey/baseUrl patterns.

## Goals / Non-Goals

**Goals:**
- Extract header/paragraph structure from DOCX/PPTX/XLSX as markdown-equivalent nodes (Title → `#`, Heading1 → `##`, etc.)
- Analyze standalone and embedded images via multimodal LLM (default: qwen3.6-moe) producing both free-text descriptions and structured entity nodes
- Support embedded image extraction from PPTX/DOCX, analyze them with the same vision pipeline
- Configure vision model independently from text labeling model, with inheritance of provider/apiKey
- Maintain memory efficiency: batch vision calls (5 per batch) with GC between batches
- Zero external tool dependencies (no pandoc, no python) — pure Haskell for office, curl for LLM

**Non-Goals:**
- Full-fidelity DOCX/PPTX rendering (tables, charts, animations, SmartArt)
- Legacy .doc/.ppt binary format parsing (stub with warning, recommend conversion to .docx)
- OCR of text within images (the vision model handles this natively)
- Video/audio analysis
- Training or fine-tuning custom vision models
- Streaming/chunked vision API calls (images are atomic)

## Decisions

### D1: Office extraction via ZIP+XML (not pandoc)

**Decision**: Parse DOCX/PPTX/XLSX as ZIP archives using `zip-archive` + `xml-conduit`, extract text content to markdown.

**Alternatives considered**:
- **Pandoc CLI**: `pandoc file.docx -t markdown` — excellent conversion quality but requires pandoc installed. Violates zero-external-dep goal.
- **Python mammoth/python-pptx**: Best quality for DOCX/PPTX but requires Python runtime. Same dependency concern.
- **Haskell pandoc library**: Would pull in the entire pandoc ecosystem (100+ transitive deps). Massive dependency footprint.

**Rationale**: ZIP+XML is self-contained, covers 80% of real-world office documents (headings, paragraphs, lists), and adds only 2 lightweight dependencies. The office XML schemas (OOXML) are well-documented. We don't need rendering — we need structure extraction (headers, paragraphs, slides), which maps directly to our existing markdown → node pipeline.

### D2: Image analysis via OpenAI Vision API with base64 encoding

**Decision**: Send images as base64-encoded data URLs in the OpenAI chat completions API `image_url` content type. Use the existing curl-based pattern from `OpenAI.hs`.

**Alternatives considered**:
- **Image URL hosting**: Upload images to a temporary URL, pass URL to API. Requires a file server. More complex.
- **Ollama local API**: Different request format, different endpoint. Would need a separate client.
- **Haskell HTTP client (http-conduit)**: More robust than curl but adds dependency complexity. Current curl pattern works well.

**Rationale**: Base64 data URLs are supported by all OpenAI-compatible APIs (OpenAI, Ollama, LiteLLM). The curl pattern is proven in `OpenAI.hs`. No new dependencies needed. Ollama supports `/v1/chat/completions` with vision content blocks.

### D3: VisionConfig inherits from LabelingConfig

**Decision**: `VisionConfig` mirrors `LabelingConfig` with an additional `enabled` field. Default model: `qwen3.6-moe`. Default baseUrl: `http://localhost:11434/v1`. Inheriting provider/apiKey from labeling config when not explicitly set.

**Alternatives considered**:
- **Single unified config**: Use LabelingConfig for both text and vision. Problem: text labeling uses cheap models (gpt-4o-mini), vision needs multimodal models.
- **Auto-detect model capabilities**: Query the API for vision support. Complex, error-prone, adds latency.
- **Separate config with no inheritance**: User must configure both separately. More friction.

**Rationale**: Option B from exploration. Users typically run the same provider for both, but need different models. Inheriting provider/apiKey/baseUrl avoids duplication while allowing model-specific selection.

### D4: Office files produce markdown-equivalent nodes, not semantic extraction

**Decision**: Convert office content to markdown text, then feed through existing `extractDocFile` pipeline. Headers become `#`, `##`, etc. No special node types for tables, lists, or formatting.

**Alternatives considered**:
- **Semantic office nodes**: Table nodes, list nodes, slide transition edges. Over-engineering for MVP.
- **Full OOXML parsing**: Extract every element type. Massive complexity, marginal graph value.
- **Per-slide nodes**: Each PPTX slide as a separate file-like node. Loses slide ordering context.

**Rationale**: The existing markdown → node pipeline already handles headers, tags, and wikilinks. Converting office docs to markdown reuses all that infrastructure. The graph value is in the structure (headers → nodes, references → edges), not in the formatting details.

### D5: Image entities use existing Relation types and nodeKind field

**Decision**: Image-extracted entities use `Contains` (image → entity) and `References` (cross-entity) relations. Entity types stored in `nodeKind` (e.g., "Person", "Skill", "Organization"). Full LLM response stored in `nodeExtra` as JSON.

**Alternatives considered**:
- **New Relation types** (HAS_SKILL, WORKS_AT, etc.): Adds enum variants, requires changes across export modules. YAGNI for MVP.
- **Flat description only**: Lose structured entity extraction. Reduces graph utility.
- **Separate entity type field**: Adds complexity. `nodeKind` already serves this purpose.

**Rationale**: The `nodeKind` field already exists and is used for "Module", "Header", "Tag", "File". Extending it to "Person", "Skill", etc. is natural. The `nodeExtra` field stores the full structured response for queries. No schema changes needed.

### D6: Architecture layers

**Decision**: Follow clean architecture strictly.
- **Domain**: `VisionConfig` type, `OfficeFile`/`ImageFile` FileType variants, `OfficeFiles` FileCategory
- **UseCase**: `extractOfficeFile`, `analyzeImage` orchestration, batch processing with GC
- **Infrastructure**: `OfficeConvert` (ZIP/XML parsing), `LLM.Vision` (curl-based API calls)

**Rationale**: Matches existing pattern (Domain types → UseCase orchestration → Infrastructure IO). No IO in Domain/UseCase.

## Risks / Trade-offs

- **[OOXML complexity]** DOCX/PPTX XML schemas are large. Initial implementation covers headings, paragraphs, slides, and text runs. Tables, nested content, and SmartArt → skipped gracefully → Mitigation: fallback to stub node with warning, recommend .docx conversion
- **[Vision API cost and latency]** Each image = 1 LLM call. 100 images = 100 calls at ~2-5s each → Mitigation: batch size of 5, concurrent processing, GC between batches, `enabled: false` by default
- **[Large images]** Base64 encoding ~1.33× size. 20MB image → 26MB base64 → exceeds some API limits → Mitigation: skip images > 20MB with warning log, future: client-side resize
- **[qwen3.6-moe availability]** Default model must be running locally (Ollama) or remotely → Mitigation: clear error message when model unavailable, `enabled: false` default, graceful degradation to stub node
- **[Legacy .doc/.ppt formats]** OLE2 binary format cannot be parsed in pure Haskell without massive effort → Mitigation: stub node with warning, recommend conversion to .docx/.pptx

## Verification Strategy (Check)

1. **Build**: `cabal build` passes with new dependencies (zip-archive, xml-conduit, base64-bytestring)
2. **Unit tests**: `cabal test` passes — existing 90 examples unchanged
3. **Office extraction test**: Create test .docx file with headings → verify header nodes (h1, h2, etc.) and Contains edges generated
4. **Image analysis test**: With qwen3.6-moe running, process a test image → verify ImageFile node + entity nodes with correct nodeKind values
5. **Embedded image test**: Create test .pptx with embedded image → verify both slide text nodes and image analysis nodes
6. **Memory test**: Process directory with 50+ images → verify peak memory stays under 3× final graph size
7. **Graceful degradation**: Process .doc file → verify stub node created with warning log, no crash
8. **Config test**: Verify vision config defaults, inheritance from labeling config, and YAML parsing

## Iteration & Rollback (Act)

**If Check fails:**
- OOXML parsing insufficient → fall back to stub nodes for unsupported elements, don't block release
- Vision API unreliable → disable by default (`vision.enabled: false`), document required setup
- Memory regression → reduce batch size, add more GC boundaries

**Standardization for next cycle:**
- Collect entity extraction quality metrics (precision/recall on resume test set)
- Consider diagram-specific extraction (flowcharts, architecture diagrams) based on user feedback
- Consider OCR fallback for images where vision model returns empty results
- Evaluate PPTX slide relationship extraction (next/previous, parent/child decks)