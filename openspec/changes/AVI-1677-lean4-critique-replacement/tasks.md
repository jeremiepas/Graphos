# Tasks: AVI-1677 lean4 critique replacement

## 0. Feasibility spike
- [x] 0.1 Pick one critique function (e.g. surpriseScore) and reduce it to a crisp predicate in Lean
- [x] 0.2 Compile it to C with `@[export]` in a standalone Lean project
- [x] 0.3 Link it into a minimal Haskell consumer (Cabal extra-libraries)
- [x] 0.4 Invoke via `foreign import ccall` and match the Haskell result on a fixture

## 1. Public OSS project bootstrap
- [ ] 1.1 Create the public repository (name, org, license decided)
- [ ] 1.2 Pin the Lean toolchain
- [ ] 1.3 Build Lean + selected libraries, export a stable C ABI, publish versioned releases
- [ ] 1.4 CI, docs, governance

## 2. Contract inventory
- [ ] 2.1 Freeze decision IDs, Haskell source references, theorem obligations
- [ ] 2.2 Freeze ABI input/output encodings, error codes, memory ownership
- [ ] 2.3 Freeze determinism/ordering guarantees

## 3. Lean implementation
- [ ] 3.1 Use Mathlib/external Lean where it improves proofs/algorithms
- [ ] 3.2 Keep the extracted ABI surface primitive and serializable
- [ ] 3.3 Exclude proof-only definitions from extraction
- [ ] 3.4 Maintain theorem gates per lean-proof-methodology

## 4. Compilation bridge
- [ ] 4.1 Expose `graphos_lean_critique.h`, symbol naming/version macros
- [ ] 4.2 Provide `graphos_lean_critique_version`
- [ ] 4.3 Document linker inputs and NixOS build notes

## 5. Graphos consumption
- [ ] 5.1 Add Infrastructure FFI adapter
- [ ] 5.2 Keep Haskell implementation as reference/optional fallback
- [ ] 5.3 Parity tests and version checks
- [ ] 5.4 Rollout via configuration and build flags

## 6. Parity, performance, cutover
- [ ] 6.1 Golden fixture parity
- [ ] 6.2 Memory/leak and performance checks
- [ ] 6.3 Decide cutover (delete Haskell path or keep fallback)
- [ ] 6.4 Update docs/PRD/workflow references

## 7. Governance
- [ ] 7.1 Semantic versioning, ABI compatibility policy
- [ ] 7.2 Public issue tracker and releases, deprecation process

## Verification
- [ ] V.1 OSS: `lake build`, zero errors/warnings/sorry, ABI smoke tests
- [ ] V.2 Graphos: Cabal build linking the OSS library, FFI round-trip, parity, fallback
- [ ] V.3 Cross-repo: pin OSS version in Graphos integration branch and test
