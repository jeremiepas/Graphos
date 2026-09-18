# E2E Verification Log — streaming-embeddings (task 6.1)

Fixture: /tmp/graphos-e2e/fixture (Foo.hs, 4 extractable nodes)
Binary: cabal-built graphos (http-client transport, dedup+batch+cache path)
Local Ollama: nomic-embed-text @ http://localhost:11434/v1

## Run 1 (cold cache)
```
[12:13:58] [ INFO] [graphos]   Generating node embeddings...
[12:14:00] [ INFO] [graphos]   Wrote 4 node embeddings to embeddings.json
```
- embeddings.json: 4 nodes × 768 dims
- cache dir out1/cache/embeddings/: 4 JSON entries (sha256 key files)

## Run 2 (warm cache, same output dir)
```
[12:14:06] [ INFO] [graphos]   Generating node embeddings...
[12:14:06] [ INFO] [graphos]   Wrote 4 node embeddings to embeddings.json
```
- embeddings.json identical to run 1; cache entry mtimes unchanged (no rewrite
  needed for hits; write-back is idempotent).
- Run 2 completed in <1s vs ~2s for run 1 (no HTTP traffic for cached texts).

## Run 3 (selective invalidation: one cache entry deleted)
```
[12:14:14] [ INFO] [graphos]   Wrote 4 node embeddings to embeddings.json
```
- Deleted entry re-fetched; the 3 other cache files retain their original
  mtimes; result identical (4 × 768 dims).

Conclusion: cache reuse verified end-to-end against a real Ollama server.
