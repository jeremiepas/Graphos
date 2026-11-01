# Design: Lean 4 compile-time bridge for the critique path

## Reconciled strategy

1. Mathlib/external Lean is allowed.
2. If a bridge is required, build it as a public OSS project.
3. That project produces a versioned C ABI library from Lean.
4. Graphos uses a compilation bridge: GHC links the external Lean library at build time
   and Haskell FFI declares the foreign calls.
5. No dynamic runtime bridge/plugin loading. Dependency resolved at compile/link time,
   with version checks and fallback controlled by build flags.

## Repository topology (AVI-1677-D7)

- No new Graphos repository. Graphos remains in its existing repository; all Graphos-side work
  (FFI adapter, Cabal linkage, parity tests, docs) lands there.
- A separate repository is created only for the Lean-Haskell bridge, and only because the bridge
  needs an independently buildable Lean 4 project (toolchain pin, `lake` build, versioned C ABI).
  That repository is `graphos-lean-critique` and is consumed by Graphos as an external build
  dependency.

## Bridge

- OSS library exposes `graphos_lean_critique.h`, symbol naming/version macros, and a
  version query `graphos_lean_critique_version`.
- Haskell side: thin FFI module marshals Domain types to C arrays and back; backend
  selection is a Cabal flag or a runtime capability check against the linked library
  version.

## Stages

0. Feasibility spike: one critique function defined in Lean, compiled to C with
   `@[export]`, built in the OSS library, linked into a tiny Haskell consumer, invoked
   via `foreign import ccall`, matched to the Haskell result on a fixture.
1. Public OSS project bootstrap (toolchain pin, CI, docs, license, releases).
2. Contract inventory (decision IDs, sources, theorem obligations, ABI encodings,
   error codes, ownership, determinism).
3. Lean implementation (Mathlib where useful; ABI surface primitive/serializable;
   proof-only definitions noncomputable or excluded).
4. Compilation bridge design (headers, linker inputs, NixOS notes).
5. Graphos consumption and fallback (Infrastructure FFI adapter, parity, version checks).
6. Parity, performance, cutover (golden fixtures, leak/perf checks, cutover decision).
7. OSS governance (semver, ABI policy, deprecation).

## Risks

- Build coupling to an externally installed/vendored library.
- ABI compatibility requires semver discipline.
- Mathlib runtime size: keep extraction ABI small.
- NixOS toolchain: document `leancc` and library paths.
- Natural-language critique stays out of scope unless reduced to crisp predicates.
