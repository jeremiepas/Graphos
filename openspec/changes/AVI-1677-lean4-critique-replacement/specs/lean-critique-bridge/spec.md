# lean-critique-bridge

## Purpose

Define the contract for the Lean 4 critique library and its compile-time
consumption by Graphos.

## Requirements

### Requirement: Versioned C ABI
The OSS library SHALL export a stable C ABI with a version query
`graphos_lean_critique_version` and a public header `graphos_lean_critique.h`.

### Requirement: Compile-time linkage
Graphos SHALL link the OSS library at build time via Cabal `extra-libraries`,
`extra-lib-dirs`, and `include-dirs`; it SHALL NOT dynamically load a runtime
bridge.

### Requirement: Parity
Lean-side critique results SHALL match Haskell results on pinned golden fixtures
within the agreed tolerance.

### Requirement: Fallback
Graphos SHALL retain the Haskell critique implementation as a reference/optional
fallback during rollout, selectable by build flag or capability check.
