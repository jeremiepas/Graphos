## Why

Embedding a 109k-node graph takes ~2h30 because every node triggers a `curl` fork plus a fresh TCP connection (`Infrastructure/LLM/Embedding.hs`), texts are sent one per request, and no content deduplication or caching exists — the same `Promise ./path.ts` text is re-embedded thousands of times. The pipeline also races on a fixed temp file (`/tmp/graphos-embed-payload.json`), which is already a bug when two Graphos instances run concurrently.

## What Changes

- Replace the per-node `curl` process with a native Haskell HTTP client (`http-client`) using a shared connection `Manager` (keep-alive); payload goes in memory — no temp file, no fork, no data race.
- Add a batched embedding API: `generateEmbeddings :: EmbeddingConfig -> [Text] -> IO (Either Text [[Double]])` using the OpenAI-compatible `input: [texts…]` array. Chunks of 64–128 bring 109,023 requests down to ~1,000–1,700.
- Deduplicate texts before embedding: embed only unique texts, then redistribute vectors to every node sharing that text.
- Persist a content-addressed embedding cache in `graphos-out/cache/` keyed by `SHA256(model, text)` → vector. Subsequent runs (including `--fresh`) re-embed only what changed.
- `ieSourceHash` becomes a real content hash (SHA256 of model + text) instead of the source file path.
- Optional bounded concurrency (2–4 batches in flight) via `async`, defaulting to sequential to stay gentle on local Ollama.
- A Lean 4 model (`lean/EmbedPipeline.lean`, already drafted) proves the optimizations are semantics-preserving: batching + dedup + cache + concurrent completion ≡ the current sequential loop, under the single assumption that the server is a deterministic function of (model, text).

## Capabilities

### New Capabilities
- `embedding-throughput`: batched, deduplicated, cached, keep-alive embedding pipeline; concurrency; Lean 4 equivalence proof as the verification instrument.

### Modified Capabilities
- `embedding`: the curl-based single-text requirement is superseded by the native HTTP batched client; custom headers requirement carries over verbatim to the new transport.

## Impact

- **Infrastructure**: `LLM/Embedding.hs` rewritten on `http-client` (already a dependency); new shared `Manager`; batch request/response JSON; new cache module `LLM/EmbeddingCache.hs` (SHA256 via existing `cryptohash-sha256`).
- **UseCase**: `Pipeline/Core.hs` (`generateGraphEmbeddings`) and `Ingest.hs` (`generateEmbeddingsForNodes`) switch to dedup + batch + cache; `Port/LLMPort.hs` gains `lpGenerateEmbeddings` (plural), keeping `lpGenerateEmbedding` for compatibility.
- **Domain**: `IngestEmbedding.ieSourceHash` semantics change (path → SHA256 content hash); `EmbeddingConfig` gains `embBatchSize` and `embConcurrency` fields (defaults: 64, 1).
- **Config**: `graphos.yaml` `embedding:` section gains optional `batchSize` / `concurrency` keys.
- **No contract change** to `graph.json`, `embeddings.json` sidecar, or `index.json` formats beyond the documented meaning of `ieSourceHash`.