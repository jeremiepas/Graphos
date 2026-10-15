#!/usr/bin/env python3
"""Embedding throughput benchmark (change: lfm-embedding-optimization, D7/D6).

Drives an OpenAI-compatible /embeddings endpoint with a node-shaped
synthetic corpus (<label> <sourcefile> strings sized from a real pipeline's
label/source distribution), sweeps concurrency {1,2,4,8} at batchSize 64,
and reports texts/s, wall-clock, and hardware info as a Markdown table row.

Usage:
  python3 bench.py --base-url http://localhost:8080/v1 --model hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M
  python3 bench.py --help
  python3 bench.py --probe            # oversized-input regression probe (4.3)
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import random
import sys
import time
import urllib.request
import urllib.error
from concurrent.futures import ThreadPoolExecutor

BATCH_SIZE = 64
CONCURRENCY_LEVELS = [1, 2, 4, 8]
TEXTS_PER_LEVEL = 1024

# Node-shaped corpus: labels drawn from the distribution of a production
# Graphos graph (function/identifier labels + doc-file labels), source paths
# with realistic depth/extension mix. Labels are short; the informative head.
LABEL_POOL = [
    "getUser",
    "setUser",
    "handleRequest",
    "renderApp",
    "parseConfig",
    "readFile",
    "writeFile",
    "connect",
    "disconnect",
    "initialize",
    "Promise",
    "Result",
    "App",
    "Config",
    "Router",
    "Store",
    "Client",
    "Server",
    "Middleware",
    "Controller",
    "Adapter",
    "Port",
    "Domain",
    "transform",
    "validate",
    "serialize",
    "deserialize",
    "authenticate",
    "authorize",
    "refresh",
    "invalidate",
    "subscribe",
    "unsubscribe",
    "onEvent",
    "emit",
    "pipe",
    "reduce",
    "compose",
    "memoize",
    "debounce",
    "README",
    "CHANGELOG",
    "LICENSE",
    "design",
    "proposal",
    "tasks",
    "spec",
    "report",
    "guide",
    "overview",
]
EXT_POOL = [".ts", ".tsx", ".hs", ".rs", ".py", ".go", ".java", ".md"]
DIRS = [
    "src/Graphos/UseCase",
    "src/Graphos/Infrastructure/LLM",
    "src/Graphos/Domain",
    "app",
    "tests/Graphos",
    "docs",
    "src/Graphos/Infrastructure/FileSystem",
    "src/Graphos/CLI",
]


def make_text(rng: random.Random) -> str:
    label = rng.choice(LABEL_POOL)
    src = f"{rng.choice(DIRS)}/mod{rng.randint(1, 400)}{rng.choice(EXTS)}"
    return f"{label} {src}"


EXTS = [".ts", ".tsx", ".hs", ".rs", ".py", ".go", ".java", ".md"]


def hardware_info() -> str:
    cpu = os.cpu_count() or 0
    ram = ""
    try:
        with open("/proc/meminfo") as f:
            for line in f:
                if line.startswith("MemTotal:"):
                    ram = f"{int(line.split()[1]) // (1024 * 1024)} GB RAM"
                    break
    except OSError:
        pass
    return f"{cpu} cores, {ram}, {platform.machine()}"


def post_embeddings(
    url: str, model: str, texts: list[str], timeout: float
) -> tuple[bool, int]:
    payload = json.dumps({"model": model, "input": texts}).encode()
    req = urllib.request.Request(
        url,
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            body = json.loads(resp.read())
            return True, len(body.get("data", []))
    except urllib.error.HTTPError as e:
        print(f"  HTTP {e.code}: {e.read()[:200]}", file=sys.stderr)
        return False, 0
    except Exception as e:  # transport failure
        print(f"  transport error: {e}", file=sys.stderr)
        return False, 0


def run_level(
    base_url: str, model: str, concurrency: int, n_texts: int, rng: random.Random,
    batch: int = BATCH_SIZE,
) -> dict:
    texts = [make_text(rng) for _ in range(n_texts)]
    batches = [texts[i : i + batch] for i in range(0, len(texts), batch)]
    start = time.perf_counter()
    ok_vecs = 0
    with ThreadPoolExecutor(max_workers=concurrency) as pool:
        for ok, n in pool.map(
            lambda b: post_embeddings(base_url, model, b, 120.0), batches
        ):
            if ok:
                ok_vecs += n
    elapsed = time.perf_counter() - start
    return {
        "concurrency": concurrency,
        "texts": n_texts,
        "vectors": ok_vecs,
        "wall_s": round(elapsed, 3),
        "texts_per_s": round(ok_vecs / elapsed, 1) if elapsed > 0 else 0.0,
    }


def prepare_client_side(text: str, limit_tokens: int = 512) -> str:
    """Mirror Domain.Embedding.prepare: conservative char/4 estimate —
    truncate to at most `limit_tokens * 4` characters."""
    return text[: limit_tokens * 4]


def oversized_probe(base_url: str, model: str) -> dict:
    """One over-limit text in a 64-text batch: with the pipeline's token-fit
    preparation the request is accepted and siblings keep their vectors."""
    rng = random.Random(42)
    normal = [make_text(rng) for _ in range(63)]
    oversized = "poison " + ("word " * 6628)  # ~6628 tokens before preparation
    batch = normal[:31] + [oversized] + normal[31:]
    prepared = [prepare_client_side(t) for t in batch]
    ok, n = post_embeddings(base_url, model, prepared, 120.0)
    return {
        "sent": len(batch),
        "accepted": ok,
        "vectors_returned": n,
        "siblings": (n - 1) if ok else 0,
        "siblings_intact": ok and n == len(batch),
    }


def main() -> int:
    ap = argparse.ArgumentParser(description="Embedding throughput benchmark")
    ap.add_argument(
        "--base-url",
        default=os.environ.get("EMBED_BASE_URL", "http://localhost:8080/v1"),
    )
    ap.add_argument(
        "--model", default=os.environ.get("EMBED_MODEL", "nomic-embed-text")
    )
    ap.add_argument("--texts", type=int, default=TEXTS_PER_LEVEL)
    ap.add_argument("--batch-size", type=int, default=BATCH_SIZE)
    ap.add_argument("--levels", default=",".join(map(str, CONCURRENCY_LEVELS)))
    ap.add_argument(
        "--probe",
        action="store_true",
        help="run the oversized-input regression probe only",
    )
    ap.add_argument(
        "--smoke", action="store_true", help="100-text single-level smoke run"
    )
    args = ap.parse_args()

    batch = args.batch_size
    if args.probe:
        r = oversized_probe(args.base_url, args.model)
        print(
            f"probe: {r['sent']} sent (one oversized, prepared client-side) -> "
            f"{r['vectors_returned']} vectors (siblings {r['siblings']}/63, "
            f"intact={r['siblings_intact']})"
        )
        return 0

    rng = random.Random(42)
    if args.smoke:
        r = run_level(args.base_url, args.model, 1, 100, rng, batch)
        print(f"smoke: {r['texts_per_s']} texts/s ({r['vectors']}/{r['texts']})")
        return 0

    levels = [int(x) for x in args.levels.split(",") if x.strip()]

    print(f"model: {args.model}  base_url: {args.base_url}  date: {datetime_stamp()}")
    print(f"hardware: {hardware_info()}, batch {batch}, {args.texts} texts/level")
    print()
    print("| Concurrency | Vectors | Wall (s) | texts/s |")
    print("|-------------|---------|----------|---------|")
    for level in levels:
        r = run_level(args.base_url, args.model, level, args.texts, rng, batch)
        print(
            f"| {r['concurrency']} | {r['vectors']} | {r['wall_s']} | {r['texts_per_s']} |"
        )
    return 0


def datetime_stamp() -> str:
    import datetime

    return datetime.datetime.now().strftime("%Y-%m-%d %H:%M")


if __name__ == "__main__":
    sys.exit(main())
