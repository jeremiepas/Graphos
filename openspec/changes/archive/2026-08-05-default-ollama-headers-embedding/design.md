## Context

Graphos has three LLM client configurations (`LabelingConfig`, `EmbeddingConfig`, `VisionConfig`) in `Domain.Config.Vision`, and extractor configuration (`defaultExtractors`) in `Domain.Config.Extraction`. Currently:

- **Labeling** defaults to OpenAI (`provider: openai`, `model: gpt-4o-mini`, `apiKey: ${OPENAI_API_KEY}`), requiring an API key before any AI feature works
- **Embedding** defaults to local Ollama (`nomic-embed-text`), but lacks custom header support
- **Vision** defaults to Ollama for base URL but uses `${OPENAI_API_KEY}` as apiKey default
- **Extractors** default to LSP for many languages (Haskell, Go, Rust, Python, C/C++), which requires installing and running those language servers
- Auth is limited to `Authorization: Bearer <apiKey>` — no support for custom headers
- The `graphos init` YAML template doesn't document all parameters; Haskell defaults and YAML template are inconsistent

All three LLM clients live in Infrastructure (`OpenAI.callLLM`, `Embedding.generateEmbedding`, `Vision.analyzeImage`) and share a similar curl-based HTTP pattern.

## Goals / Non-Goals

**Goals:**
- Make Graphos work out-of-the-box with local Ollama (zero API keys, zero config)
- Make tree-sitter the default extractor for all code languages (zero server dependencies)
- Support arbitrary HTTP headers for auth on all LLM/embedding/vision API calls
- Align all defaults across code, config parsing, and init template
- Generate a comprehensive `graphos.yaml` via `graphos init` that documents every parameter, shows tree-sitter as default with LSP as commented alternatives, and shows PDF extraction defaults with custom examples in comments

**Non-Goals:**
- In-process embedding model execution (Graphos will continue to call Ollama via HTTP)
- Streaming/SSE support for LLM calls
- Changing the curl-based HTTP client pattern (no HTTP library migration)
- Supporting multiple auth strategies per-request (one set of headers per config section)
- Removing LSP extraction entirely (it remains available, just not default)
- Changing extraction granularity — `GranularityFunction` (stop at function/global variable level) remains the default

## Decisions

### D1: `headers` field type — `Map String String`

| Alternative | Pros | Cons |
|---|---|---|
| `Map String String` | Natural YAML mapping, deduplication, clear semantics | No ordering guarantee (irrelevant for headers) |
| `[(String, String)]` | Preserves order, allows duplicate header names | YAML representation awkward, no dedup |
| Per-field named headers | Type-safe, discoverable | Inflexible, every new header needs a code change |

**Decision**: `Map String String`. Headers don't need ordering, deduplication is desirable, and YAML maps map directly.

### D2: Auth header merge strategy

**Decision**: Three-tier auth, evaluated in order, last wins on collision:

1. If `provider /= "ollama"` and `apiKey /= ""`: add `Authorization: Bearer <apiKey>`
2. Custom `headers` map applied after: each entry becomes `-H "Key: Value"`
3. On collision between (1) and (2): custom `headers` override `Authorization`

### D3: Default provider change — Ollama

**Decision**: Default to Ollama. Users without Ollama get a clear error. The `graphos init` template will include commented-out OpenAI configuration. **Breaking change** for anyone relying on the implicit OpenAI default.

### D4: Where to add headers — Domain types, Infrastructure curl

**Decision**: Add `headers` fields to `LabelingConfig`, `EmbeddingConfig`, `VisionConfig` in `Domain.Config.Vision`. Infrastructure modules read the headers and pass them to curl. Preserves the Domain ← UseCase ← Infrastructure boundary.

### D5: Init template — comprehensive with all params, tree-sitter default, LSP in comments

**Decision**: Rewrite `defaultConfigYaml` in `Main.hs` to produce a YAML that:
- Lists every parameter with its default value as a comment
- Shows tree-sitter as the default extractor for all code languages
- Includes commented-out LSP alternatives for each language (so users can switch to LSP by uncommenting)
- Shows PDF extraction with default config and commented custom examples
- Documents all LLM/embedding/vision parameters with defaults
- Uses `granularity: function` as the default (stop at function/global variable level)

Example extractors section in the template:
```yaml
extractors:
  # TypeScript (default: tree-sitter; uncomment for LSP)
  ".ts":
    mode: tree-sitter
    grammar: typescript
    language_id: typescript
    # mode: lsp
    # language_id: typescript
  # Haskell (default: tree-sitter; uncomment for LSP)
  ".hs":
    mode: tree-sitter
    grammar: haskell
    language_id: haskell
    # mode: lsp
    # language_id: haskell
```

### D6: Default extractors — tree-sitter for all code languages

**Decision**: Change `defaultExtractors` in `Domain.Config.Extraction` from LSP to tree-sitter for all code languages. LSP remains available via config override. Tree-sitter is zero-dependency (no server to install/start) and more reliable for batch extraction.

Current defaults → new defaults:
```
.hs  : LSP → TreeSitter (grammar: haskell)
.lhs : LSP → TreeSitter (grammar: haskell)
.go  : LSP → TreeSitter (grammar: go)
.rs  : LSP → TreeSitter (grammar: rust)
.py  : LSP → TreeSitter (grammar: python)
.pyw : LSP → TreeSitter (grammar: python)
.c   : LSP → TreeSitter (grammar: c)
.cpp : LSP → TreeSitter (grammar: cpp)
.h   : LSP → TreeSitter (grammar: c)
.hpp : LSP → TreeSitter (grammar: cpp)
.nix : LSP → TreeSitter (grammar: nix)
.rb  : LSP → TreeSitter (grammar: ruby)
.java: LSP → TreeSitter (grammar: java)
```

Already tree-sitter (unchanged):
```
.ts/.tsx/.js/.jsx/.json/.md/.rst/.adoc
```

### D7: Extraction granularity — function level default

**Decision**: Keep `defaultGranularity = GranularityFunction`. This extracts module/structure nodes, API-surface definitions (functions, classes, types, fields, imports/exports), and module-level constants. Extraction stops at function bodies. This is already the default and aligns with "stop at function/global variable level."

## Risks / Trade-offs

- **[Breaking change: default provider]** Users with existing `graphos.yaml` that omit `provider` will get `ollama` instead of `openai`. → **Mitigation**: The `graphos init` template includes commented-out OpenAI section.
- **[Breaking change: default extractor mode]** Users relying on LSP extraction for `.hs`, `.go`, `.rs`, `.py`, `.c`, `.cpp`, etc. will get tree-sitter instead. Tree-sitter extracts less semantic info (no cross-file references, no hover), but is more reliable. → **Mitigation**: LSP is available via config override. Init template shows how to switch.
- **[Ollama not running]** `--label` or `--embed` will fail with a connection error. → **Mitigation**: Improve error message to suggest installing/starting Ollama.
- **[Header injection]** Custom headers are passed directly to curl without sanitization. → **Mitigation**: Local CLI tool, config is trusted input. Document env var expansion.

## Verification Strategy (Check)

1. `cabal build` compiles with no warnings
2. `cabal test` passes all existing tests
3. `graphos init` generates YAML with every parameter documented, tree-sitter as default, LSP in comments
4. Manual test: with Ollama running, `graphos . --label --embed` works with zero config changes
5. Manual test: with `provider: openai` and `api_key: "${OPENAI_API_KEY}"`, labeling still works
6. Manual test: custom headers in `graphos.yaml` appear in curl calls
7. Manual test: tree-sitter extraction works for `.hs`, `.go`, `.py`, `.rs`, `.c`, `.cpp`
8. Config round-trip: `graphos.yaml` with custom headers and extractor overrides → `loadConfig` → `mergeGraphosConfig` → preserved

## Iteration & Rollback (Act)

**If Check fails:**
- Breaking change too disruptive → add migration guide or `--extractor-legacy` flag
- Tree-sitter grammar not available for a language → fall back to `ExtractStub` for that language, document in init template
- Headers not merging correctly → add unit tests for header merge logic

**Rollback:** Revert to OpenAI defaults and LSP extractors. The `headers` field is purely additive (defaults to empty map), so it's safe to keep even on rollback.

**Standardization:** If local-first + tree-sitter defaults are well-received, document the philosophy in PRD.

## Migration Plan

1. Change `defaultLabelingConfig`, `defaultVisionConfig` defaults to Ollama
2. Add `headers` fields to all three config types
3. Update `FromJSON`/`ToJSON` instances with `.:? "headers" .!= Map.empty`
4. Update `mergeGraphosConfig` for new fields
5. Change `defaultExtractors` from LSP to tree-sitter for all code languages
6. Update `callLLM`, `generateEmbedding`, `analyzeImage` to pass custom headers
7. Rewrite `defaultConfigYaml` in `Main.hs` (comprehensive, tree-sitter default, LSP in comments, PDF examples)
8. Update project `graphos.yaml` to match new defaults
9. Build, test, verify