# E2E_LOG.md — end-to-end verification for `lfm-embedding-optimization`

Date: 2026-09-22

## 6.3 End-to-end run (LFM2.5 default)

Corpus: small mixed corpus (one `.md` doc + one `.rs` file) at
`/tmp/opencode/e2e-corpus`, config `graphos.yaml` pointing `embedding` at the
standalone llama-server :8080 serving
`hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M` (the new default; the
`base_url` was set to `http://localhost:8080/v1` because the default Ollama
endpoint does not serve the GGUF reference).

```
$ graphos . --embed --no-cluster --no-viz        # run 1 (cold)
[ INFO]   Generating node embeddings...
[ INFO]   Wrote 2 node embeddings to embeddings.json
```

- **Zero HTTP 500s** (with `--verbose`/debug the embedding-failure log line is
  silent — no per-input failures).
- **`embeddings.json` sidecar written with 1024-dim vectors** via the
  streaming staged path (spot check):
  ```
  36915_doc_overview dim 1024
  36915_h1_Overview dim 1024
  ```
- **Cache populated:** 2 entry files under `graphos-out/cache/embeddings/`.
- **Second run warm:** zero API calls (all texts served from the cache), zero
  cache writes (`cache count before=2 after=2`), no re-name churn.
- **No staging leftovers** in the output directory (`ls graphos-out | grep -i
  staging` is empty) — the staged sidecar was renamed into place, per D9.

## 6.4 Full regression gate

```
$ cabal build --flag dev     → 0 errors (with -Wall -Werror dev flags)
$ cabal test                 → 1123 examples, 3 failures, 2 pending
```

The 3 failures are **pre-existing**, from other in-flight changes in the
working tree, not from this change:

1. `Graphos.Domain.Config — SemanticEdgesConfig serializes to snake_case keys`
   (aeson key-ordering; the tested encoding itself is unchanged since commit
   2aa3fff's spec-graph-verification work).
2. `Graphos.UseCase.Incremental — cluster+infer on the merged graph equals
   cluster+infer on the full build` (Leiden adjacency ordering;
   `wire-incremental-update` change).
3. `Graphos.UseCase.Staging — returns Left instead of throwing when the swap
   into place fails` (environmental: `chmod` on `/tmp` denied for a non-root
   user; `wire-incremental-update` change — the swap-failure path this
   change relies on for the prior-sidecar guarantee is covered separately by
   the streaming sidecar's staged-discard tests in `PipelineSpec`).

Every test added/extended by this change is green:

- `Graphos.Domain.EmbeddingSpec` (1.3) — 9 examples, 0 failures
- `Graphos.Domain.ConfigSpec` embedding blocks (1.2, 3.3, 6.1) — 20 examples, 0 failures
- `Graphos.Infrastructure.LLM.EmbeddingSpec` (2.1) — parser + `prepareEmbed` green
- `Graphos.Infrastructure.LLM.EmbeddingCacheSpec` (2.3) — prefix/model invalidation green
- `Graphos.UseCase.PipelineSpec` (2.2, 2.4, 3.4) — bisection + warm zero-writes + streaming green
- `Graphos.UseCase.IngestSpec` (2.3) — prefix participation + truncation convergence green

## Lean machine check

`lake clean && lake build` in `lean/` with pinned Lean 4.29.1 — exit 0, zero
errors/warnings; see `lean/VERIFICATION.md` (5.1/5.2).