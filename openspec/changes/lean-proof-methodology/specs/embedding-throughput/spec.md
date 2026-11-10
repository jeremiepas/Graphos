# embedding-throughput (delta)

## ADDED Requirements

### Requirement: Reproducible machine check of the equivalence proof

The Lean 4 equivalence artifact SHALL compile with zero errors and zero warnings under a clean build of the pinned core toolchain (Lean 4.29.1, no Mathlib), and the exact verification command SHALL be recorded inside the change directory.

#### Scenario: Clean rebuild is green
- **WHEN** `lake clean && lake build` is run in `openspec/changes/streaming-embeddings/lean` with the Lean 4.29.1 toolchain
- **THEN** the command exits 0 and reports zero errors and zero warnings

#### Scenario: Command is recorded and portable
- **WHEN** a reviewer reads `lean/VERIFICATION.md`
- **THEN** it names the toolchain source (`nix run nixpkgs#lean4`), the exact binary path, and the exact commands, and warns that `nixpkgs#lean` provides Lean 3

### Requirement: Goal-guided invocation of cache-bearing theorems

Proofs in the artifact SHALL invoke master theorems that carry an auto-implicit cache binder (`runCached_fst`, `runCached_sound`) through goal-guided tactics or defeq-forcing `have` steps, and SHALL NOT pass the cache argument positionally as a literal proof term.

#### Scenario: Positional proof terms are rejected in review
- **WHEN** a proof passes `[]` (or any cache value) as a positional argument to `runCached_fst`/`runCached_sound`
- **THEN** the edit is rejected in review because Lean mis-routes the argument into `fb`, producing misleading instance errors

### Requirement: No decide over non-reducible statement shapes

`by decide` SHALL be used only where the statement kernel-reduces over concrete data; statements mentioning `runCached`, `List.lookup`, or `Option.getD` SHALL be proved by derivation from the master theorems and Boolean case analysis on the `beq` scrutinee instead.

#### Scenario: Lookup-bearing goals avoid decide
- **WHEN** a toy example's statement contains `runCached` or a `List.lookup` chain
- **THEN** its proof derives the result from `pipeline_eq_baseline` / `runCached_fst` / `runCached_sound` (or `rw`-chains over them) rather than `by decide`