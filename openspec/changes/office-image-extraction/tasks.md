## 1. Add OfficeFile FileType and OfficeFiles FileCategory

- [ ] 1.P Plan: Add `OfficeFile` to `FileType` enum and `OfficeFiles` to `FileCategory` enum in Domain.Types.Pipeline. Add `fecOffice` field to `FileExtensionConfig` in Domain.Config. Update `allSupportedExtensions` in Detect.hs. Add ToJSON/FromJSON instances. Check criteria: `cabal build` passes, existing tests pass, `OfficeFiles` appears in FileCategory JSON serialization.
- [ ] 1.D Do: Add `OfficeFile` to FileType (Node.hs), `OfficeFiles` to FileCategory (Pipeline.hs), `fecOffice` to FileExtensionConfig (Config.hs), update defaultFileExtensions, allSupportedExtensions, and detectFilesWithExtensions routing. Add office extensions `[.docx, .pptx, .xlsx, .doc, .ppt]`.
- [ ] 1.C Check: `cabal build` succeeds, `cabal test` passes 90 examples, FileCategory JSON round-trips correctly.
- [ ] 1.A Act: If checks pass, move to task 2. If any test fails, fix and re-run.

### Attempt history (1)

## 2. Add VisionConfig to Domain.Config

- [ ] 2.P Plan: Create `VisionConfig` data type with fields `vcEnabled`, `vcModel`, `vcApiKey`, `vcBaseUrl`, `vcMaxTokens`, `vcBatchSize`. Add ToJSON/FromJSON instances with defaults (model="qwen3.6-moe", baseUrl="http://localhost:11434/v1", apiKey="${OPENAI_API_KEY}", maxTokens=1000, batchSize=5). Add `gcVision` field to `GraphosConfig`. Update `defaultGraphosConfig`, `mergeGraphosConfig`, and config loading in Infrastructure.Config. Check criteria: `cabal build` passes, VisionConfig JSON parsing works with partial overrides.
- [ ] 2.D Do: Implement VisionConfig in Domain.Config, add to GraphosConfig, update merge logic, update Infrastructure.Config loader to parse `vision:` section from YAML. Add `--vision`/`--no-vision` CLI flags in Main.hs that set `vcEnabled`.
- [ ] 2.C Check: `cabal build` succeeds, `cabal test` passes, VisionConfig FromJSON parses `{enabled: true, model: "gpt-4o"}` with defaults for missing fields.
- [ ] 2.A Act: If checks pass, move to task 3. If config parsing fails, debug and retry.

### Attempt history (1)

## 3. Implement DOCX extraction (OfficeConvert.hs)

- [ ] 3.P Plan: Replace stub `docxToMarkdown` with real ZIP+XML parsing. Add `zip-archive` and `xml-conduit` dependencies. Parse `word/document.xml`, extract `<w:p>` paragraphs with `<w:pStyle>` heading detection, produce markdown text. Check criteria: test DOCX with headings produces correct markdown, corrupt file produces Right with error message.
- [ ] 3.D Do: Implement `docxToMarkdown` in OfficeConvert.hs using `zip-archive` to open the ZIP and `xml-conduit` to parse `word/document.xml`. Map `<w:pStyle w:val="Heading1"/>` → `##`, Title → `#`, etc. Extract `<w:r><w:t>` text runs. Handle missing files with `Either Text` error. Add `docxExtractMediaPaths` and `docxExtractMediaFile` for embedded image support.
- [ ] 3.C Check: `cabal build` succeeds. Test with a manually-created .docx file: verify `# Title` and `## Heading 1` in output. Test with missing/corrupt file: verify `Left "..."` error.
- [ ] 3.A Act: If extraction is incomplete (missing table support, etc.), document limitation and move on. Core heading/paragraph extraction must work.

### Attempt history (1)

## 4. Implement PPTX and XLSX extraction (OfficeConvert.hs)

- [ ] 4.P Plan: Add `pptxToMarkdown` and `xlsxToMarkdown` to OfficeConvert.hs. PPTX: read `ppt/slides/slideN.xml`, extract `<a:t>` text, produce `## Slide N` headers. XLSX: read `xl/worksheets/sheet1.xml`, extract `<c><v>` cells, produce markdown tables. Check criteria: PPTX produces slide headers, XLSX produces table rows.
- [ ] 4.D Do: Implement `pptxToMarkdown` parsing slide XML, `xlsxToMarkdown` parsing worksheet XML. Add `pptxExtractMediaPaths` for embedded image extraction. Handle PPTX slide relationships to map images to slides.
- [ ] 4.C Check: `cabal build` succeeds. Test with minimal PPTX: verify `## Slide 1` output. Test with minimal XLSX: verify markdown table output.
- [ ] 4.A Act: If PPTX or XLSX parsing has edge cases (animations, complex layouts), document and defer. Core text extraction must work.

### Attempt history (1)

## 5. Implement legacy .doc/.ppt handling

- [ ] 5.P Plan: Add `docToMarkdown` and `pptToMarkdown` stubs that return a warning message recommending conversion. These are called by the office extraction pipeline when legacy formats are detected. Check criteria: .doc files produce a stub node with warning, no crash.
- [ ] 5.D Do: Implement simple stubs that return `Right "# Document: <path>\n\n[Legacy .doc format — convert to .docx for full extraction]"` and similar for .ppt. Add detection in extractOfficeFile to route by extension.
- [ ] 5.C Check: `cabal build` succeeds. Test with a .doc extension: verify warning message in output.
- [ ] 5.A Act: If edge cases found, document them.

### Attempt history (1)

## 6. Wire office extraction into Extract.hs pipeline

- [ ] 6.P Plan: Add `extractOfficeFile` function in `UseCase.Extract.Office` module that routes by extension to the correct OfficeConvert function, then feeds the markdown through `extractDocFile`. Wire `OfficeFiles` category into `extractAll` in `Extract.hs` with concurrent processing and GC boundaries. Check criteria: .docx file in test directory produces header nodes, .doc produces stub with warning.
- [ ] 6.D Do: Create `UseCase.Extract.Office` module with `extractOfficeFile :: PipelineConfig -> LogEnv -> FilePath -> IO Extraction`. Route by extension: .docx → docxToMarkdown → extractDocFile, .pptx → pptxToMarkdown → extractDocFile, .xlsx → xlsxToMarkdown → extractDocFile, .doc/.ppt → stub. Add office extraction branch to `extractAll` in Extract.hs, processing OfficeFiles concurrently with doc files.
- [ ] 6.C Check: `cabal build` succeeds. `cabal test` passes. Manual test: create a .docx with headings, run graphos, verify header nodes in graph.json.
- [ ] 6.A Act: If routing or node creation fails, fix extraction logic and re-test.

### Attempt history (1)

## 7. Implement LLM.Vision module (base64 + curl)

- [ ] 7.P Plan: Create `Infrastructure.LLM.Vision` module with `analyzeImage :: VisionConfig -> FilePath -> IO (Either Text ImageAnalysis)`. Implement base64 encoding, OpenAI Vision API call with curl (reuse pattern from OpenAI.hs), JSON response parsing. Define `ImageAnalysis` and `Entity` types. Check criteria: unit test with mocked response parses correctly, build succeeds.
- [ ] 7.D Do: Create module. Implement `encodeImageBase64`, build vision API payload with `image_url` content type containing `data:image/png;base64,...`. Parse response into `ImageAnalysis { iaDescription :: Text, iaEntities :: [Entity], iaKind :: ImageKind }`. Add `Entity { entityLabel :: Text, entityType :: Text, entityConfidence :: Double }` and `ImageKind` enum. Handle errors (connection failure, invalid JSON, rate limiting).
- [ ] 8.C Check: `cabal build` succeeds. Unit test: parse a mocked LLM JSON response into ImageAnalysis with entities.
- [ ] 7.A Act: If curl pattern needs adjustment for vision payloads, iterate.

### Attempt history (1)

## 8. Implement image extraction module (UseCase.Extract.Image)

- [ ] 8.P Plan: Create `UseCase.Extract.Image` module with `extractImageFile :: PipelineConfig -> LogEnv -> FilePath -> IO Extraction`. Convert ImageAnalysis to nodes (image node + entity nodes + Contains edges). Handle embedded images from PPTX/DOCX by accepting ByteString instead of FilePath. Check criteria: image produces ImageFile node with nodeKind and nodeExtra, entities produce typed nodes with Contains edges.
- [ ] 8.D Do: Create module. Implement node creation: image → `ImageFile` node with `nodeExtra = Just (object ["description" .= ..., "kind" .= ..., "entities" .= ...])`. Each entity → node with `nodeKind = Just entityType`. Create `Contains` edges from image to entities. Handle size limit (>15MB skip with stub). Handle embedded images via `extractImageFromBytes` variant that takes ByteString.
- [ ] 8.C Check: `cabal build` succeeds. Test: given a mock ImageAnalysis, verify node creation produces correct nodeId, nodeKind, nodeExtra, and edge relations.
- [ ] 8.A Act: If node ID generation has collisions, fix hashing scheme.

### Attempt history (1)

## 9. Wire image extraction into Extract.hs pipeline with batching

- [ ] 9.P Plan: Add `ImageFiles` handling to `extractAll` in Extract.hs. Process images in batches of `vcBatchSize` with `evaluate` + `performGC` between batches (matching existing chunk+GC pattern). Extract embedded images from PPTX/DOCX and analyze them alongside standalone images. Check criteria: images produce nodes, memory stays bounded, concurrent processing works.
- [ ] 9.D Do: Add image extraction branch to `extractAll`. Create `imageNodeMapRef` and `imageEdgeAccRef` IORefs. Batch processing: `chunkList vcBatchSize imageFiles`, mapM_ over chunks with `performGC` between batches. For each PPTX/DOCX, call `extractMediaPaths` then `extractImageFromBytes` on embedded media. Merge image extraction results with main extraction accumulator.
- [ ] 9.C Check: `cabal build` succeeds. `cabal test` passes. Manual test: directory with .png file produces ImageFile node and entity nodes in graph.json. Memory profile with +RTS -s shows bounded growth.
- [ ] 9.A Act: If batching or GC doesn't control memory, reduce batch size or add more frequent GC.

### Attempt history (1)

## 10. Integration testing and documentation

- [ ] 10.P Plan: Create integration test with a test directory containing .docx, .pptx, .png files. Verify end-to-end: office files produce header nodes, images produce entity nodes, embedded images are analyzed, pipeline completes without crash, graph.json is valid. Check criteria: all file types produce nodes, no crashes, memory under 3× graph size.
- [ ] 10.D Do: Create test directory with sample files. Write Hspec test that runs the extraction pipeline on the test directory. Verify node counts and edge relations. Update graphos.yaml example with vision section. Update README with office/image extraction documentation.
- [ ] 10.C Check: `cabal test` passes including new integration tests. Manual run with +RTS -s shows reasonable memory. graph.json contains office and image nodes.
- [ ] 10.A Act: Finalize. If any edge cases found, document as known limitations.

### Attempt history (1)