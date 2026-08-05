## Why

Graphos defaults to OpenAI for LLM labeling and vision, requiring an API key before any AI feature works. The default config is cloud-first, contradicting the local-first philosophy. Additionally, LLM clients only support `Authorization: Bearer` auth — some providers (custom gateways, LiteLLM proxies, enterprise APIs) need custom headers like `X-API-Key` or `X-Tenant-ID`. The embedding config lacks custom header support. The `init` command's generated YAML doesn't document all available parameters. Finally, extraction defaults currently prefer LSP for many languages (Haskell, Go, Rust, Python, C/C++), but tree-sitter is more reliable and zero-dependency — it should be the default, with LSP configs shown as commented-out alternatives.

## What Changes

- **BREAKING**: Change `LabelingConfig` defaults from `provider: openai` to `provider: ollama`, `model: gpt-4o-mini` to `model: llama3.2`, `apiKey` from `${OPENAI_API_KEY}` to `""`, `baseUrl` from `https://api.openai.com/v1` to `http://localhost:11434/v1`, `batchSize` from `10` to `20`
- **BREAKING**: Change `VisionConfig` defaults: `apiKey` from `${OPENAI_API_KEY}` to `""`
- Add `labelingHeaders :: Map String String` field to `LabelingConfig` for custom HTTP headers
- Add `embHeaders :: Map String String` field to `EmbeddingConfig` for custom HTTP headers
- Add `vcHeaders :: Map String String` field to `VisionConfig` for custom HTTP headers
- Update `callLLM`, `generateEmbedding`, and `analyzeImage` to include custom headers in curl calls
- Update auth logic: if `provider == "ollama"` and `apiKey == ""`, skip `Authorization` header entirely; custom headers override `Authorization` on collision
- **BREAKING**: Change `defaultExtractors` to use `ExtractTreeSitter` for all code languages, with LSP configs moved to commented-out alternatives in the init template
- Code extraction granularity defaults to `GranularityFunction` (stop at function/global variable level) — already the case, but enforce in init template comments
- Rewrite `initConfigFile` YAML template to document every parameter with its default value as a comment, including extraction configs with tree-sitter as default and LSP as commented-out alternatives

## Capabilities

### New Capabilities
- `custom-llm-headers`: Support for arbitrary HTTP headers on LLM, embedding, and vision API calls — enables auth patterns beyond `Authorization: Bearer`

### Modified Capabilities
- `llm-labeling`: Default provider changes from OpenAI to Ollama (local-first); new `headers` field; default model/apiKey/baseUrl/batchSize changes
- `embedding`: New `headers` field for custom auth headers on embedding API calls
- `vision-analysis`: Default apiKey changes to empty; new `headers` field; inherits from labeling when unset
- `extraction`: Default extractors change from LSP to tree-sitter for all code languages; init template shows tree-sitter as default with LSP as commented alternatives; granularity default is `function` (stop at function/global variable level)
- `extraction`: Default extractors change from LSP to tree-sitter for all code languages; init template shows tree-sitter as default with LSP as commented alternative; granularity default is `function` (stop at function/global variable level)

## Impact

- **Domain types**: `LabelingConfig`, `EmbeddingConfig`, `VisionConfig` gain `headers` field; default values change; `defaultExtractors` changes from LSP to tree-sitter for most languages
- **Infrastructure**: `OpenAI.callLLM`, `Embedding.generateEmbedding`, `Vision.analyzeImage` add custom headers to curl calls
- **Config loading**: `FromJSON`/`ToJSON` instances for all three configs updated; `mergeGraphosConfig` handles new fields
- **CLI**: `graphos init` generates comprehensive YAML with all params documented, tree-sitter as default extractor, LSP as commented alternatives
- **Breaking**: Existing `graphos.yaml` files that omit `provider` will get `ollama`; projects relying on LSP extractors by default will get tree-sitter; projects relying on OpenAI must explicitly set `provider: openai`
- **Backward compat**: Config files that explicitly set `provider: openai` or `mode: lsp` continue to work unchanged

## PDCA Cycle

- **Plan**: All AI features work out-of-the-box with local Ollama. Tree-sitter is the default extractor (zero-dependency). Custom header auth enables enterprise API gateways. Init template is comprehensive.
- **Do**: Change defaults to Ollama/local + tree-sitter, add `headers` fields, update curl calls, rewrite init YAML template.
- **Check**: `cabal test` passes; `graphos init` produces YAML with all params commented; `graphos . --label --embed` works with local Ollama; existing explicit configs still work.
- **Act**: If local-first or tree-sitter defaults cause confusion, improve error messages and init template documentation. Iterate based on user feedback.