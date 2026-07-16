## Why

Graphos currently extracts structure from code files (via LSP/tree-sitter) and text documents (markdown via header parsing), but treats office documents (.docx, .pptx, .xlsx) and images (.png, .jpg, etc.) as invisible — they're detected and counted but produce no graph nodes. This leaves significant knowledge in organizations (resumes, presentations, diagrams, photos) disconnected from the codebase graph.

With multimodal LLMs now widely available (qwen3.6-moe, gpt-4o, etc.), we can extract structured knowledge from images. Office documents are ZIP archives containing XML — extractable in pure Haskell without external dependencies.

## What Changes

1. **Office document extraction**: Parse .docx, .pptx, .xlsx as ZIP archives, extract text content, convert to markdown-style headers (Title → #, Heading 1 → ##, etc.), and feed into the existing `extractDocFile` pipeline. Legacy .doc/.ppt formats get stub nodes with warnings.

2. **Image analysis via vision LLM**: Send images (standalone or embedded in PPTX/DOCX) to a multimodal model (default: qwen3.6-moe via Ollama). Extract structured entities (name, type, confidence) and free-text descriptions. Create typed nodes (Person, Skill, Organization) linked to the image with `Contains` edges.

3. **Vision configuration**: New `VisionConfig` in `GraphosConfig` mirroring `LabelingConfig` structure. Default model: qwen3.6-moe. Inherits provider/apiKey/baseUrl from labeling config. Configurable batch size for concurrent image processing.

4. **New file categories**: `OfficeFiles` category in `FileCategory` and `OfficeFile` in `FileType` enum. Office extensions (.docx, .pptx, .xlsx, .doc, .ppt) added to detection.

## Capabilities

### New Capabilities
- `office-extraction`: Parse DOCX/PPTX/XLSX files as ZIP archives, extract text to markdown, create graph nodes matching the existing header/tag pattern
- `image-analysis`: Analyze standalone and embedded images via multimodal LLM, extract structured entities and free-text descriptions, create typed nodes linked to the source image
- `vision-config`: Configuration for multimodal LLM calls, inheriting from labeling config with model override

### Modified Capabilities
- `file-detection`: Add OfficeFiles category and office/image file extensions to detection and config
- `extraction-pipeline`: Add office file and image extraction paths to extractAll, with batched GC boundaries for vision API calls

## Impact

- **New modules**: `Infrastructure.FileSystem.OfficeConvert` (upgrade from stub), `Infrastructure.LLM.Vision` (new), `UseCase.Extract.Office` (new), `UseCase.Extract.Image` (new)
- **Modified modules**: `Domain.Types` (FileCategory, FileType), `Domain.Config` (VisionConfig), `UseCase.Detect` (office extensions), `UseCase.Extract` (new extraction paths), `Domain.Types.Pipeline` (PipelineConfig for vision settings)
- **New dependencies**: `zip-archive`, `xml-conduit`, `base64-bytestring`
- **Config changes**: New `vision` section in graphos.yaml, new `office` and `vision` extension categories
- **No breaking changes**: Existing extraction pipelines unchanged, new file types fall through to stub if vision is disabled

## PDCA Cycle

- **Plan**: Office documents and images represent ~15-30% of enterprise knowledge assets. Adding extraction should increase node count by 10-40% on typical mixed-format directories, with image entity extraction providing typed Person/Skill/Organization nodes not available from code alone.
- **Do**: Implement office ZIP→XML→markdown extraction, vision LLM client with base64 image encoding, batch processing with GC, config-driven model selection.
- **Check**: Verify on a test directory containing .docx, .pptx, .png, .jpg files that: (1) office files produce header/tag nodes matching markdown pattern, (2) images produce typed entity nodes, (3) embedded PPTX images are analyzed, (4) memory stays under 3× final graph size with batching, (5) existing code/doc extraction unchanged.
- **Act**: Standardize the vision prompt across image types. Feed entity extraction quality metrics (precision/recall on resume test set) into next iteration. Consider adding diagram-specific extraction (flowcharts, architecture diagrams) in future cycle.