<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. Per-component timeout in SDK shutdownObservability

- [x] 1.P Plan: Wrap each sub-cleanup in `shutdownObservability` (SDK.hs) with independent 5s timeout + catch. Check criteria: (1) metrics server cancel wrapped in timeout+catch, (2) OTLP SDK flush wrapped in timeout+catch, (3) debug trace flush wrapped in timeout+catch, (4) OTLP logs flush wrapped in timeout+catch, (5) `cabal build` zero warnings. Affected: `Infrastructure.Observability.SDK`. Risk: `shutdownTracerProvider` signature — may return non-() that needs handling inside timeout.
- [x] 1.D Do: Implement per-component timeouts in SDK `shutdownObservability`. Each component gets `timeout 5000000` + `catch (\(e::SomeException) -> hPutStrLn stderr ...)`. Log warning on timeout/exception, continue to next.
- [x] 1.C Check: (1) All 4 sub-cleanups wrapped in timeout+catch, (2) each logs warning on timeout, (3) continues to next on timeout, (4) `cabal build` zero warnings.
- [x] 1.A Act: Standardize pattern — if it works, document as "timeout-guarded cleanup" convention for future Infrastructure modules.

### Attempt history (1)

<!-- empty -->

## 2. Per-component timeout in non-SDK shutdownObservability

- [x] 2.P Plan: Same pattern as Task 1 but for `Infrastructure.Observability` (custom impl). Check criteria: (1) metrics server cancel wrapped in timeout+catch, (2) OTLP trace export wrapped in timeout+catch, (3) OTLP metrics export wrapped in timeout+catch, (4) `cabal build` zero warnings. Affected: `Infrastructure.Observability`.
- [x] 2.D Do: Implement per-component timeouts in non-SDK `shutdownObservability`. Same pattern: `timeout 5000000` + `catch` for each sub-cleanup.
- [x] 2.C Check: (1) All sub-cleanups wrapped, (2) each logs warning on timeout, (3) continues to next, (4) `cabal build` zero warnings.
- [x] 2.A Act: If both modules use identical pattern, consider extracting a helper `timeoutGuarded :: String -> IO () -> IO ()` to avoid duplication in future.

### Attempt history (2)

<!-- empty -->

## 3. Add --otel-shutdown-timeout CLI flag and PipelineConfig field

- [x] 3.P Plan: Add `cfgOtelShutdownTimeout :: Int` field to `PipelineConfig` (default: 10). Add `--otel-shutdown-timeout INT` CLI flag. Pass value through config. Check criteria: (1) `PipelineConfig` has new field, (2) CLI flag parses correctly, (3) default is 10, (4) `cabal build` zero warnings, (5) `cabal test` passes. Affected: `Domain.Types.Pipeline`, `app.Main.hs`. Risk: `PipelineConfig` is large — must update all construction sites.
- [x] 3.D Do: Add field to `PipelineConfig`. Add optparse-applicative parser for `--otel-shutdown-timeout`. Update all PipelineConfig construction sites (Main.hs pipelineOpts, defaultConfig if any, runSingleFilePipeline, etc.) with `pure 10` or the parsed value.
- [x] 3.C Check: (1) Field exists in PipelineConfig, (2) `--otel-shutdown-timeout 2` parses, (3) default is 10, (4) `cabal build` zero warnings, (5) `cabal test` passes.
- [x] 3.A Act: If multiple config fields are added over time, consider a dedicated `OtelShutdownConfig` sub-record.

### Attempt history (3)

<!-- empty -->

## 4. Pipeline call-site shutdown timeout

- [x] 4.P Plan: Wrap `shutdownObservability obsEnv` at both call sites in `UseCase.Pipeline` with `timeout (cfgOtelShutdownTimeout config * 1000000)`. On `Nothing`, log warning. Check criteria: (1) `runPipeline` wraps shutdown in timeout, (2) `runSingleFilePipeline` wraps shutdown in timeout, (3) on timeout, logs warning but continues, (4) `cabal build` zero warnings. Affected: `UseCase.Pipeline`. Risk: `runIncrementalPipeline` does NOT call `shutdownObservability` — verify and add if needed.
- [x] 4.D Do: Add `System.Timeout.timeout` wrapper at both `shutdownObservability` call sites. On `Nothing`, log warning via `hPutStrLn stderr`. Also check `runIncrementalPipeline` — it creates `obsEnv` but never shuts down; add shutdown with timeout there too.
- [x] 4.C Check: (1) All pipeline functions that create `obsEnv` also shut it down with timeout, (2) timeout value comes from `cfgOtelShutdownTimeout`, (3) on timeout, warning logged, (4) `cabal build` zero warnings.
- [x] 4.A Act: If Main.hs also calls `initObservability` separately (watch mode), add shutdown timeout there too.

### Attempt history (4)

<!-- empty -->

## 5. Exit code handling in Main.hs

- [x] 5.P Plan: Ensure Main.hs exits 0 when pipeline succeeds (even if shutdown timed out). The current code already exits based on `runPipeline` result — verify shutdown timeout does not cause non-zero exit. Check criteria: (1) Normal mode: exit 0 on `Right res` regardless of shutdown outcome, (2) Watch mode: exit 1 only on initial pipeline failure, (3) `cabal build` zero warnings, (4) `cabal test` passes. Affected: `app.Main.hs`.
- [x] 5.D Do: Review Main.hs exit paths. Since `shutdownObservability` is now called inside `runPipeline` (wrapped in timeout that doesn't throw), the `Right result` path should already exit 0. But Main.hs's watch mode also calls `initObservability` separately — add shutdown with timeout there. Verify no exit code regression.
- [x] 5.C Check: (1) `runPipeline` returns `Right` even if shutdown times out (timeout returns `Nothing`, not exception), (2) Main.hs watch mode handles shutdown, (3) `cabal build` zero warnings, (4) `cabal test` passes.
- [x] 5.A Act: If MVar crash still occurs, investigate whether GHC RTS `+RTS -I0` (disable idle GC) or `unregisterMVar` workarounds are needed.

### Attempt history (5)

<!-- empty -->