## Why

The streaming-embeddings verification instrument (`openspec/changes/streaming-embeddings/lean/EmbedPipeline.lean`) did not actually compile under a fresh Lean 4.29.1 build: earlier "green" runs were stale-`lake`-cache illusions, and the file contained latent proof breakages in every `List.lookup`-induction lemma plus un-provable `by decide` toy examples. More importantly, the *method* used to invoke the master theorems (`runCached_fst`, `runCached_sound`) was fragile: positional proof terms mis-route through the auto-implicit `{cache}` binder, producing confusing `OfNat (List (Nat × String))` failures, and `by decide` cannot kernel-reduce `List.lookup`/`Option.getD` chains. Without a documented, reproducible proof style, every future edit of the artifact risks reintroducing multi-hour debugging sessions.

## What Changes

- Fix all latent proof breakages so `lake clean && lake build` exits 0 with **zero errors and zero warnings** (recorded in `lean/VERIFICATION.md`).
- Standardize the artifact's proof methodology on two rules:
  1. **Goal-guided invocation, not positional application.** Master theorems with auto-implicit `{cache}` are applied via tactics (`rw [theorem]`, `rw [theorem, ...]`) or via `have h' : <reduced statement> := by rw [theorem] at h; exact h` forcing steps — never as positional proof terms, because Lean's elaborator skips implicit binders when filling positional arguments (the literal `[]` lands in `fb : β`, yielding misleading `OfNat`/type-mismatch errors).
  2. **No `decide` over `List.lookup`.** `by decide` is admissible only when the statement kernel-reduces (`pipeline`-over-`Nat` shapes with concrete lists); any statement mentioning `runCached`/`lookup`/`Option.getD` must instead be derived from the master theorems (`pipeline_eq_baseline`, `runCached_fst`, `runCached_sound`) plus the `beq`-case-analysis pattern below.
- Document the `List.lookup` reduction pattern that Lean 4.29.1 core actually supports: `rcases hbeq : (a == k) with _ | _` (Bool constructor order `false | true`) followed by `rw [List.lookup, hbeq] at h` + `exact h` inside a `have h' : <reduced> := by …` defeq-forcing step; `simp`/`simp only` cannot rewrite into a `match (a == k)` scrutinee, and `beq_iff_eq`/`if_neg` chains do not fire on it.
- Make the inter-run cache-reuse toy proof self-contained: the double-`runCached` example rewrites with `runCached_fst`, then reduces the produced cache via `rw [runCached]` + `simp only [List.nil_append, batchedTable_eq_table]` + `exact sound_table f _` — expressing "the inner run builds a sound cache, so the outer run recomputes nothing" without ever passing `cache` positionally.
- Silence the `unusedSectionVars` linter file-wide (`set_option linter.unusedSectionVars false`) instead of per-theorem `omit` modifiers that break doc-string attachment.

## Capabilities

### Modified Capabilities
- `embedding-throughput`: the "Sequential-equivalence" requirement's verification clause now has a reproducible, warning-free machine check (`lake clean && lake build`, Lean 4.29.1 core only) and a documented proof methodology pinned in the change directory; the requirement text itself is unchanged.

## Impact

- **No functional impact** on the Haskell pipeline; this change touches only the verification instrument and its documentation.
- `lean/EmbedPipeline.lean`: rewritten induction lemmas (`mem_of_lookup_eq_some`, `lookup_append_of_none`, `lookup_append_of_some`, `lookup_table`), `runCached_fst`'s `some`-case (`Option.getD_some` step), and all section-7 toy examples now follow the standardized style.
- New `lean/VERIFICATION.md` records the exact toolchain (`nix run nixpkgs#lean4` → Lean 4.29.1 at `/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin`; `nixpkgs#lean` is Lean 3 and must not be used) and the exact verification command.
- Task 6.2 of streaming-embeddings ("rerun 1.2's exact command; zero errors") inherits a stable, reproducible command.
- Future edits to the artifact (e.g., modeling retries, new cache semantics) must follow the two methodology rules to stay maintainable.