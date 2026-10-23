## Why

The 2026-09-22 kernel OOM (journalctl, boot -1) killed `graphos` at 30.9 GB RSS on a
62 GB no-swap machine while other processes (llama-server 4.3 GB, dev agents ~15 GB)
held the rest — the run had no heap budget, so nothing stopped it before the kernel did.
A `--max-heap` flag exists (fix-runtime-ram-crash task 1) but is opt-in; an uncapped run
grows until the OOM killer fires, taking down the run and stressing the whole desktop.
The active embedding path makes this worse: LFM2.5 emits 1024-dim vectors (~22.5 KB JSON
per node, ~40 KB boxed in memory per node), so a 150k-node corpus pushes the embedding
pass alone into multi-GB territory with no early warning. The pipeline needs a default
memory budget derived from the machine, not from user vigilance. (Workflow 01, PRD §16.1
scale targets.)

## What Changes

- **Default heap budget** — when `--max-heap` is absent, graphos derives a default RTS
  heap limit from the system's available memory (MemAvailable with a safety reserve),
  logs the chosen value, and re-executes with `-M`. No more uncapped runs.
- **Graceful heap-exhaustion failure** — when the `-M` cap trips mid-run, the pipeline
  SHALL fail with a clear stage-named error and a preserved checkpoint (resume via
  existing checkpoint-controls), instead of an opaque RTS abort or a kernel kill.
- **Pre-flight memory guard** — before the pipeline starts (and before the embedding
  pass specifically), graphos reads MemAvailable, compares it against the configured
  budget, and warns or aborts (`--fail-on-low-memory`) when the budget cannot be met.
- **Stage-aware budget enforcement** — the embedding pass checks its projected
  assignment footprint (node count × dims × 8 B unboxed, or measured RSS growth)
  against the remaining budget and logs the projection at INFO before starting.

## Capabilities

### New Capabilities
- `memory-budget-guard`: Machine-derived default heap budget, pre-flight
  memory checks, graceful heap-exhaustion failure, and stage-aware budget
  enforcement for the CLI pipeline path. (workflow 01; PRD §16.1 scale targets)

### Modified Capabilities
- `01-full-pipeline`: Pipeline startup gains the pre-flight memory guard and
  default heap budget (requirements extend Workflow 01 stage behavior).
- `embedding`: The embedding pass requirement is extended — before generating
  embeddings the pass SHALL log a projected memory footprint and refuse to
  start when the budget is already exceeded.
- `checkpoint-controls`: On heap-exhaustion failure the checkpoint state is
  preserved and resumable (extends resume requirements with the OOM case).

## Impact

- **Code**: `app/Main.hs` (`reexecWithRTS` — derive default `-M`, read
  `/proc/meminfo`), `src/Graphos/CLI/Parser.hs` (`--fail-on-low-memory`), new
  `Infrastructure.System.Memory` (MemAvailable reader, pure Domain policy in
  `Domain.Config`), `UseCase.Pipeline.Core` (pre-flight check + heap-overflow
  handler + embedding projection log), `UseCase.Ingest` (same guard for ingest).
- **API**: New CLI flag `--fail-on-low-memory[=BOOL]` (default off: warn only);
  stderr now includes a `[memory]` budget line at startup.
- **Dependencies**: None new (reads `/proc/meminfo` directly on Linux; other
  platforms fall back to "no default budget" + warning).
- **Performance**: One re-exec at startup when no explicit `--max-heap` is
  given (same mechanism as `--rts-profile` today); per-stage checks are O(1).
- **Compatibility**: Explicit `--max-heap` still wins over the derived default;
  JSON outputs and sidecar formats unchanged. Complements (does not supersede)
  the planned `bounded-embedding-memory` change, which reduces the footprint
  this guard polices.