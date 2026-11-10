# Proposal: Use Lean 4 to replace the critique part of Graphos

**Issue:** AVI-1677
**Approved plan revision:** `29f98e79-117e-4f97-9acd-fc29845c4563` (revision 4)
**Branch:** `AVI-1677-lean4`

## Why

Graphos builds a knowledge graph and produces a "critique/review reasoning" surface
(`src/Graphos/Domain/Analysis.hs`: `analyze`, `surprisingConnections`, `suggestQuestions`,
together with the usable-graph-for-code-review change D1-D7). That surface is currently
implemented in Haskell with no formal guarantees. We want Lean 4 to own this reasoning so
the critique path gains machine-checked invariants, while keeping Haskell as the host
application and runtime.

The bridge must not be a hidden/runtime-loaded plugin. It is a **compile-time
compilation bridge**: Lean is compiled to a C library and imported/linked by GHC at
build time, exposed through `foreign import ccall`.

## What Changes

- Allow Mathlib/external Lean 4 libraries.
- If a bridge is needed, create a **public open-source project** (`graphos-lean-critique`
  or similar) that produces a versioned C ABI library from Lean.
- Graphos links that library at build time (Cabal `extra-libraries` / `extra-lib-dirs` /
  `include-dirs`) and calls it via FFI.
- Keep the Haskell critique implementation as reference/optional fallback during rollout.
- Run parity tests between Lean and Haskell on pinned fixtures.

## Capabilities

### New Capabilities
- `lean-critique-bridge`: a versioned C ABI exposing the critique reasoning, consumed
  by Graphos through a compile-time FFI bridge.

### Modified Capabilities
- `usable-graph-for-code-review`: critique surface gradually migrates to Lean while
  preserving golden outputs.

## Impact

- New Infrastructure FFI/binding module in Graphos.
- Cabal configuration for external library linkage (NixOS `leancc` notes).
- New OSS repository for the Lean library (bootstrap, ABI, versioning, governance) — this is the **bridge** repository only. No new Graphos repository is created; Graphos stays in its existing repository and consumes the bridge as an external build dependency (AVI-1677-D7).
- Docs/PRD/workflow references updated to the OSS library.
- Non-gating: Lean verification stays report-only in CI.
