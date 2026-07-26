## MODIFIED Requirements

### Requirement: Config-driven extractor routing in detection

The detection pipeline SHALL pass `gcExtractors` from `GraphosConfig` through to extraction, ensuring that files with `mode: stub` in `graphos.yaml` are categorized correctly and routed to the stub extractor, not to LSP or tree-sitter.

Previously: `gcExtractors` was used in `UseCase.Extract` via `extractorForExt`, but detection had no awareness of extractor modes. Files with `mode: stub` were still detected and categorized as code files, then routed at extraction time.

- **Plan**: No detection change needed for extractor routing — this is already handled in `UseCase.Extract` via `extractorForExt`. The fix is ensuring `Pipeline.hs` passes the full `GraphosConfig` through so `extractorForExt` works correctly.
- **Do**: Verify that `PipelineConfig` already contains `cfgGraphosConfig :: GraphosConfig` which includes `gcExtractors`. No additional change needed beyond the pipeline fix.
- **Check**: Files with `mode: stub` in `graphos.yaml` are detected as code files but routed to the stub extractor.

#### Scenario: Stub mode files are detected but not parsed
- **WHEN** `graphos.yaml` has `extractors: { ".nix": { mode: stub } }`
- **AND** the project has `flake.nix`
- **THEN** `flake.nix` is detected as a code file
- **AND** extraction routes it to the stub extractor (creates one node per file, no parsing)

#### Scenario: Disabled LSP falls back correctly
- **WHEN** `graphos.yaml` has `lsp: { ".text": { command: "" } }`
- **AND** `extractors: { ".text": { mode: stub } }`
- **THEN** `.text` files are detected as doc files (per `file_extensions`)
- **AND** extraction uses the stub extractor for them