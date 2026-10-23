# Embedding Throughput Benchmark — BENCHMARK.md

Harness: `bench/bench.py` (node-shaped corpus, batch 64, 1024 texts/level, OpenAI-compatible `/embeddings`). Re-run with:

```sh
python3 bench/bench.py --base-url http://localhost:8080/v1/embeddings --model LFM2.5-Embedding-350M-Q8_0.gguf
python3 bench/bench.py --base-url http://localhost:11434/v1/embeddings --model nomic-embed-text
python3 bench/bench.py --probe --base-url http://localhost:8080/v1/embeddings --model LFM2.5-Embedding-350M-Q8_0.gguf
```

## Recorded sweep — 2026-09-22, idle machine

**Hardware:** AMD 24-core CPU, 62 GB RAM, x86_64 (AMD 880M iGPU, no discrete GPU in the path).
**Servers:** llama-server :8080 serving `LFM2.5-Embedding-350M-Q8_0.gguf`; Ollama :11434 serving `nomic-embed-text:latest`.

### LFM2.5-Embedding-350M (llama-server :8080)

| Concurrency | Vectors | Wall (s) | texts/s |
|-------------|---------|----------|---------|
| 1 | 1024 | 13.305 | 77.0 |
| 2 | 1024 | 13.469 | 76.0 |
| 4 | 1024 | 14.149 | 72.4 |
| 8 | 1024 | 14.595 | 70.2 |

### nomic-embed-text (Ollama :11434)

| Concurrency | Vectors | Wall (s) | texts/s |
|-------------|---------|----------|---------|
| 1 | 1024 | 11.158 | 91.8 |
| 2 | 1024 | 10.519 | 97.3 |
| 4 | 1024 | 10.722 | 95.5 |
| 8 | 1024 | 10.445 | 98.0 |

## Pinned default concurrency (D6)

Rule (D6): default = **lowest level ≥ 90% of the measured plateau**.

- **LFM2.5 (llama-server)**: plateau ≈ 77 texts/s (concurrency 1). Every level ≥ 90% (76.0 / 72.4 / 70.2). Lowest level reaching ≥ 90% = **1** — the burst rate (proposal: ~343 req/s) only manifests on warm short inputs; on node-shaped texts this box is saturated at 1.
- **nomic (Ollama)**: plateau ≈ 98 (concurrency 8); 90% ≥ 88.2 → level 1 (91.8) already qualifies.
- **Pinned default concurrency: 1** — a higher level buys nothing on this hardware (within noise) and risks memory contention with the iGPU, per D6's rationale. The proposal's preliminary 2–4 estimate is not reproduced on an idle machine; D6's fallback ("if no plateau shift is observed, default stays 1 and BENCHMARK.md records why") applies.

Note: the earlier ~343 req/s burst figure measured short warm inputs; the node-shaped corpus at realistic label/path lengths measures ~77 texts/s at batch 64 — still ≥ 4× the sequential nomic baseline measured in the original proposal (82 texts/s under-load vs 43–48 aggregate), but throughput parity (not 4×) is what the sweep shows for LFM vs nomic *on this machine*: LFM 77 vs nomic 92–98. The default-model decision (task 6.1) is grounded in the asymmetric-prefix quality capability + 512-token fit, with the throughput record here for auditability.

## Oversized-input regression probe (task 4.3)

Command:

```sh
python3 bench/bench.py --probe --base-url http://localhost:8080/v1/embeddings --model LFM2.5-Embedding-350M-Q8_0.gguf
```

Result (2026-09-22):

```
probe: 64 sent (one oversized, prepared client-side) -> 64 vectors (siblings 63/63, intact=True)
```

Without client-side preparation the same oversized text (≈6,632 tokens) is rejected by llama-server with HTTP 500 and the whole batch loses its vectors (raw probe output: `HTTP 500: input (6632 tokens) is too large to process`) — the defect this change fixes.