## 1. Methodology fixes in the artifact

- [ ] 1.1 Confirm every induction lemma over `List.lookup` (`mem_of_lookup_eq_some`, `lookup_append_of_none`, `lookup_append_of_some`, `lookup_table`) uses the `rcases hbeq : (a == k) with _ | _` + `rw [List.lookup, hbeq] at h` + defeq-forcing `have h' := by …` pattern, with the Bool constructor order `false | true` respected. Check: `grep -n "rcases beq\|rcases hbeq" lean/EmbedPipeline.lean` finds the pattern in all four lemmas.
- [ ] 1.2 Confirm no proof term passes the cache positionally to `runCached_fst` / `runCached_sound` (search for `runCached_fst 1 f []`-style applications). Check: `grep -n "runCached_fst 1\|runCached_sound 1" lean/EmbedPipeline.lean` matches only inside `rw [` or `have` contexts, never as bare proof terms.
- [ ] 1.3 Confirm the section-7 toy examples contain no `by decide` on any statement mentioning `runCached` or `List.lookup`. Check: `grep -n "by decide" lean/EmbedPipeline.lean` matches only `pipeline`-over-`Nat` statements.

## 2. Reproducible verification

- [ ] 2.1 Re-run the recorded command from `lean/VERIFICATION.md` (`lake clean && lake build` with the pinned Lean 4.29.1 toolchain) and confirm exit 0, zero errors, zero warnings. Check: exit code captured in the run log section of `VERIFICATION.md`.
- [ ] 2.2 Confirm the artifact has no Mathlib dependency. Check: `cat lean/lakefile.toml` lists no `mathlib` requirement; the file's header states "Lean 4 core uniquement, pas de Mathlib".

## 3. Documentation carried by the change

- [ ] 3.1 Keep `lean/VERIFICATION.md` accurate: toolchain provenance (`nix run nixpkgs#lean4`, exact store path, the `nixpkgs#lean` = Lean 3 warning) and the exact commands. Check: the file's run log shows `Build completed successfully` with the date of the last green run.