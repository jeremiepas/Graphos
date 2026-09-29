## Context

Graphos runs a 7-stage pipeline (detect → extract → build → cluster → infer →
analyze → export) plus an optional embedding pass. `fix-runtime-ram-crash`
bounded extraction, observability, and Node representation, and added an
opt-in `--max-heap` (re-exec with RTS `-M`) plus `--rts-profile` in
`app/Main.hs` (`stripRTSFlags`/`reexecWithRTS`). The 2026-09-22 kernel OOM
(graphos killed at 30.9 GB RSS, 62 GB no-swap machine) happened on a run
without `--max-heap`: nothing derives or enforces a budget by default. The
embedding pass amplifies growth: LFM2.5-350M returns 1024-dim vectors
(~22.5 KB JSON per node on the wire, tens of KB boxed in memory), and the
planned `bounded-embedding-memory` change (not yet implemented) will shrink
that footprint — this change polices it independently. See proposal.md — Why.

Layer constraints (clean architecture): memory policy (budget derivation,
thresholds, projection arithmetic) is pure Domain logic; reading
`/proc/meminfo` and re-exec are Infrastructure; pre-flight and stage checks
are UseCase orchestration. Domain has zero IO; UseCase has zero IO
implementation.

## Goals / Non-Goals

**Goals:**
- Every `graphos run` operates under an explicit, logged memory budget by default
- Heap exhaustion produces a controlled, stage-named failure with preserved checkpoint
- Pre-flight check refuses work that cannot fit, under a `--fail-on-low-memory` flag
- Embedding pass projects its footprint before starting (specs/memory-budget-guard)

**Non-Goals:**
- Reducing the footprint itself (unboxed vectors, single-copy assignment) — that
  is `bounded-embedding-memory`
- Machine-wide process management (killing llama-server, cgroup limits)
- Platforms without `/proc/meminfo` getting a derived budget (they keep the
  current uncapped behavior plus a warning)
- Swap or cgroup configuration

## Decisions

### D1 — Budget source: `/proc/meminfo` MemAvailable, derived in Domain, read in Infrastructure

| Aspect | Choice |
|--------|--------|
| Domain (pure) | `MemoryPolicy` in `Domain.Config`: `deriveBudget :: MemInfo -> Bytes -> Maybe Bytes`, thresholds, clamps [512 MB, 32 GB], safety reserve default 4 GB |
| Infrastructure | `Infrastructure.System.Memory`: `readMemInfo :: IO (Maybe MemInfo)` parsing `/proc/meminfo` (`MemAvailable`), `Nothing` when unreadable |
| UseCase | `UseCase.Pipeline` startup composes: CLI/config budget → derived default → uncapped+warn |

Rationale: keeps the arithmetic testable without a filesystem; the only IO is
the edge read. Reuses the existing re-exec mechanism instead of new RTS plumbing.

**Alternatives considered:**
- **A: cgroup v2 limits (`/sys/fs/cgroup/memory.max`)** — more correct in containers; deferred: /proc covers the observed failure mode, cgroup read is a follow-up additive source.
- **B: `GHC.RTS.Flags.getRTSFlags` to read an existing `-M`** — insufficient: detects an existing cap but cannot set one; setting requires exec anyway (kernel-level RLIMIT_AS via `setResourceLimit` was rejected: RLIMIT_AS counts virtual, and GHC's 1 TB reserved address space defeats it).
- **C: total RAM instead of MemAvailable** — over-commits on busy desktops (the exact Sep-22 scenario); MemAvailable reflects reclaimable cache.

### D2 — Enforcement vehicle: existing re-exec with `-M`, extended to a default

`reexecWithRTS` gains a `BudgetSource = Explicit | Derived | None` input. When
no explicit `--max-heap` is present and `readMemInfo` succeeds, Main computes
the derived default and re-executes with `-M<derived>`; the re-exec logs the
value and source at INFO. Explicit flags win; `--no-memory-budget` (new
opt-out) yields the uncapped path with a WARN.

**Alternatives considered:**
- **A: set `RtsFlags.opts` in-process** — not supported by GHC API before runtime init; rejected (same finding as fix-runtime-ram-crash task 1).
- **B: always re-exec, even for explicit values** — current behavior re-execs only when flags are set; keeping that avoids a second exec for the common `--rts-profile` case. Unchanged.

### D3 — Graceful exhaustion: catch HeapOverflow at stage boundary

The RTS `-M` cap raises an async `HeapOverflow` exception. Each pipeline stage
runs under a handler that catches it, logs an ERROR naming the current stage,
the active budget, and "checkpoint preserved", and exits non-zero (exit code
2, distinct from timeout's 1). Checkpoint writing already happens after Build
(atomic writes); no new checkpoint logic is needed. The stage name comes from
the stage runner's context (each stage already knows its name for span events).

**Alternatives considered:**
- **A: catch-all in main** — loses stage attribution; rejected.
- **B: poll RSS from a watcher thread and abort early** — adds a thread and races; RTS `-M` is the authoritative signal; rejected for this change.

### D4 — Pre-flight check: compare MemAvailable to active budget before Detect

UseCase startup (after budget establishment): if `available < budget`, log
WARNING with both numbers; if `--fail-on-low-memory` is set, exit 1 before any
stage with an error naming budget, available, and the flag. The flag defaults
to off (warn-only) to avoid breaking scripted runs.

**Alternatives considered:**
- **A: always abort on low memory** — breaks legitimate small runs where the budget is generous relative to actual need; warn-only default chosen.
- **B: make the derived budget smaller instead of warning** — silently under-provisioning big runs invites mid-run exhaustion; explicit user decision beats silent adaptation.

### D5 — Embedding projection: node count × dims × 8 B, logged and gated

Before the first batch, the embedding pass computes
`projected = nodeCount × vectorDims × 8 B` (vector dims known from the first
response's array length; a conservative 1024 default when only config-known)
and logs it at INFO. If `projected > budget`, the pass aborts with a
stage-named error before the sidecar is created. This uses the unboxed-floor
number; the boxed reality is larger, which is acceptable for a gate — the
authoritative bound remains RTS `-M` (D3). Dims come from the first response
to avoid a model-config dependency.

**Alternatives considered:**
- **A: gate on measured RSS growth after N batches** — reactive, allows early batches to waste API spend; the cheap arithmetic gate runs first, measurement is left to `--rts-profile` users.
- **B: read dims from model metadata table** — couples the guard to `Domain.Embedding`'s token table; response-derived dims are authoritative and simpler.

## Risks / Trade-offs

- [Derived budget too small on busy machines → runs abort that would have fit] → Clamped floor 512 MB, reserve only 4 GB, WARNING names both numbers, `--max-heap` overrides; default warn-only pre-flight avoids surprise exits.
- [Re-exec doubles startup cost when no explicit flag is given] → One exec of a small binary (~ms), same mechanism users already accept for `--rts-profile`.
- [HeapOverflow catch is async-exception-sensitive] → Handler wraps whole stage, no cleanup-sensitive resources across the boundary; checkpoint writes are already atomic.
- [Projection underestimates boxed-list footprint] → The gate is a floor guard, not an upper bound; RTS `-M` remains the hard stop. `bounded-embedding-memory` closes the gap at the source.
- [Non-Linux regression] → `readMemInfo` returns `Nothing` → explicit uncapped+WARN path, identical to today's behavior; Hspec covers the Nothing branch.
- [meminfo format drift across kernels] → Parse only `MemAvailable` (stable since 3.14); unit tests on fixture text; unknown lines ignored.

## Migration Plan

1. Land `Infrastructure.System.Memory` + Domain policy + unit tests (no behavior change).
2. Wire derived default into `reexecWithRTS` behind the existing flag plumbing; default budget active.
3. Add pre-flight guard + `--fail-on-low-memory`; wire into run and cluster-only paths.
4. Add stage-level HeapOverflow handlers and embedding projection gate.
5. Rollback: revert to `--max-heap`-only behavior via `--no-memory-budget` (kept one release), then remove.

Verification: `cabal build` clean under `-Werror`; `cabal test` all green
including new MemoryPolicy/HeapOverflow/PreFlight specs; manual `+RTS -s`
check on the Graphos repo and the 50k synthetic corpus (see tasks).

## Open Questions

- None. Container memory-limit detection (cgroup v2) is deferred as an
  additive follow-up and does not affect specs or tasks here.