## 1. Cache key fingerprinting (Infrastructure, parallel-safe)

- [ ] 1.1 Add config fingerprint to `Infrastructure.FileSystem.Cache`: extend `fileHash`-keyed entries so the cache key is `sha256(content <> fingerprint)` where the fingerprint serializes effective granularity, extractor mode per extension, and pdf extraction level. Unit test: same content under different granularity yields different keys; identical config yields identical keys. Verify with `cabal test`.
- [ ] 1.2 Test hit-correctness (property H, AVI-521 §1.2): a cached extraction round-trips through `saveCached`/`loadCached` and equals a fresh extraction of the same file. Verify with a Hspec round-trip test.

## 2. Eviction sweep (Infrastructure, parallel-safe with group 1)

- [ ] 2.1 Add `cache: {max_mb: 512}` to `GraphosConfig` parsing (`Domain.Config` type + `Infrastructure.Config` loader + `defaultConfigYaml` docs in `app/Main.hs`). Unit test: absent key defaults to 512; `0` parses as unlimited. Verify with `cabal test`.
- [ ] 2.2 Implement the LRU size-cap sweep in `Infrastructure.FileSystem.Cache`: readdir + stat `cache/` and `cache/embeddings/`, evict oldest-mtime entries until combined size <= cap, `0` skips, missing dirs are no-ops. Unit tests: cap exceeded evicts oldest first; `0` disables; empty cache dir is a no-op. Verify with `cabal test`.

## 3. Cache persistence across staged rebuild (UseCase + Infrastructure)

- [ ] 3.1 Move the `cache/` carry-over from post-body (`carryOverState`) to pre-body in `withStagedOutput` (`UseCase.Pipeline.Staging`): rename `out/cache` into staging before the body runs; add rename-back rollback on body failure so a failed rebuild leaves the old cache intact. Unit tests: swap succeeds → final dir contains old cache entries plus new; body fails → old cache back in place. Verify with `cabal test`.
- [ ] 3.2 Call the eviction sweep at the start of `runPipelineBody` (before detect), logging evicted-entry count at INFO. Unit test: sweep runs before extraction (observe via eviction log or test double). Verify with `cabal test`.

## 4. Wire the extraction cache into the full pipeline (UseCase)

- [ ] 4.1 Extend the FileSystem port (`UseCase.Port.FileSystemPort`) with cache consult/save operations implemented by `FileSystem.Cache` in `Infrastructure.Wiring` (keeps UseCase IO-free). Verify: `cabal build` passes with `-Wall -Werror`.
- [ ] 4.2 In `UseCase.Extract.Core.extractAll`, consult the cache per file before dispatching to tree-sitter/LSP/stub: hit → reuse cached `Extraction`; miss → extract and `saveCached`. Skip the consult when `cfgFresh` is set. Unit test: second run over unchanged files performs zero extractor invocations (assert via extractor-call counting or log). Verify with `cabal test`.
- [ ] 4.3 Log cache hit/miss counts at the end of the extract stage (e.g. `Extracted 5, reused 95 from cache`). Integration test: run pipeline twice on a fixture corpus; second run reports all-reused. Verify with `cabal test`.
- [ ] 4.4 Confluence test: run `--fresh` and `--update` over the same fixture tree; exported `graph.json` node and edge sets are equal. Add as an Hspec integration test. Verify with `cabal test`.

## 5. Watch mode: write-through + embeddings (UseCase)

- [ ] 5.1 Add cache write-through to `extractChangedFiles` (`UseCase.Extract.Core`): each changed file's extraction is saved under the fingerprinted key. Unit test: after `extractChangedFiles`, the cache contains the file's entry and a subsequent `loadCached` hits. Verify with `cabal test`.
- [ ] 5.2 In `runIncrementalPipeline` (`UseCase.Pipeline.Incremental`), when `cfgEmbed` is on: call `generateGraphEmbeddings` over the merged graph, write `embeddings.json` via `writeEmbeddingsSidecar`, and set `gEmbeddings`/`gEmbeddingsPath` before export. Unit test: incremental run with a fake embedding port produces sidecar with vectors for changed nodes; unchanged texts served from a pre-populated cache (zero API calls for them). Verify with `cabal test`.

## 6. End-to-end verification

- [ ] 6.1 Manual: `graphos . --embed` twice; second run logs embedding cache hits only (no re-embedding) and extraction reuse for unchanged files. `graphos . --fresh --embed` re-extracts and re-embeds everything. Record output in change notes.
- [ ] 6.2 Full suite: `cabal build && cabal test` green; verify `graphos . --update` output graph equals `graphos . --fresh` output (node/edge counts) on this repository.