## Context

Issue #119 reports compiled-in extractor bindings: ExtractorMode only lsp/tree-sitter/stub,
grammar table and LSP table baked in. Domain IO-free; Infrastructure holds IO.

## Goals / Non-Goals

Goals: plug extractors via config; three mechanisms; per-file error isolation; cache invariants.
Non-Goals: no dynamic Haskell plugin, no shell strings, no grammar download, ABI 9-11 only.

## Decisions

- D1: extend ExtractorMode with ExtractCommand; runtime grammar stays tree-sitter + optional path/queries. Alternatives: new mode (rejected), catch-all (rejected).
- D2: command is argv array with {file}/{files}, no shell. Alternatives: string (rejected).
- D3: output reuses graph.json Node/Edge types. Alternatives: dedicated dialect (rejected).
- D4: runner in Infrastructure/Extract/Command.hs, 60s default timeout. Alternatives: UseCase (rejected IO), repo-wide (rejected cache granularity).
- D5: runtime loader beside TreeSitter/Vendored.hs, ABI guard, reuse convert. Alternatives: dynamic runtime (rejected), compile-time bindings (status quo).
- D6: .scm queries via existing FFI; missing kind disables that kind. Alternatives: custom DSL (rejected).
- D7: ResolveConfig in Domain, resolvers in Infrastructure; config overrides built-in with warning. Alternatives: string templates (deferred), Domain resolution (rejected IO).
- D8: ExtractionError per file, never abort build. Alternatives: fail build (rejected by issue).
- D9: fingerprint adds command templates, grammar path/mtime, queries, resolve entries.

## Risks / Trade-offs

- ABI skew -> guard + warning + stub.
- Huge stdout -> size cap + timeout.
- Id collisions -> bucket prefix + tests.
- Cache gaps -> fingerprint + tests.
- Cross-platform loader -> POSIX first, document others.

## Migration Plan

Additive types -> loader -> dispatch/fingerprint/cache -> resolve wiring -> tests/docs.
No output migration. Rollback reverts wiring; configs stay valid.

## Open Questions

- stdin content instead of path? deferred v2.
- separate command concurrency (default 4).
- graphos init .scm templates? optional.
- hash mtime+size vs content (mtime+size proposed).
