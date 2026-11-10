# Verification command (lean-proof-methodology)

## Toolchain

| what | value |
|---|---|
| Source | `nix run nixpkgs#lean4` |
| Binary path | `/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin` |
| Version | Lean (version 4.29.1, commit v4.29.1, Release) — Lake version 5.0.0-src+v4.29.1 |
| Dependency | none: Lean 4 core only, **no Mathlib** (`lake-manifest.json` `"packages": []`) |

> ⚠️ `nixpkgs#lean` must NOT be used: it provides **Lean 3**, which cannot read
> this Lean 4 artifact. The nix store path above is the only sanctioned
> toolchain; `nix run nixpkgs#lean4` re-resolves to it.

## Exact commands

```bash
export PATH="/nix/store/cwilxazl2n8f59lksc4xihx560485ci8-lean4-4.29.1/bin:$PATH"
cd openspec/changes/streaming-embeddings/lean
lake clean && lake build
# Expected: exit 0, zero errors, zero warnings.

# Optional direct check of the single file:
lake env lean EmbedPipeline.lean
# Expected: exit 0, no output.
```

## Proof methodology (pinned for future edits)

Two rules, per `openspec/changes/lean-proof-methodology`:

1. **Goal-guided invocation, never positional proof terms.** Master theorems
   with an auto-implicit `{cache}` binder (`runCached_fst`, `runCached_sound`)
   are applied through tactics (`rw [theorem]`, `simp only [...]`,
   `exact h` / `have h' : <reduced> := by rw [...] at h; exact h` forcing).
   Never pass a cache value (e.g. `[]`) as a positional argument: Lean's
   elaborator skips implicit binders when filling positional arguments, so the
   literal lands in `fb : β` and yields misleading `OfNat (List (Nat × String))`
   failures.
2. **No `decide` over non-reducible statement shapes.** `by decide` is
   admissible only when the statement kernel-reduces over concrete data
   (`pipeline`-over-`Nat` shapes). Any statement mentioning `runCached`,
   `List.lookup`, or `Option.getD` is derived from the master theorems
   (`pipeline_eq_baseline`, `runCached_fst`, `runCached_sound`).

**`List.lookup` induction pattern** (Lean 4.29.1 core — the only reduction
pattern that kernel-checks here; `simp`/`simp only` cannot rewrite into the
`match (a == k)` scrutinee, and `beq_iff_eq`/`if_neg` chains do not fire):

```lean
rcases hbeq : (a == k) with _ | _        -- Bool: arm order false | true
· have h' : <reduced statement> := by
    rw [List.lookup, hbeq] at h; exact h  -- defeq-forcing via have
  ...
· ...                                     -- some (head value) arm
```

Used verbatim by `mem_of_lookup_eq_some`, `lookup_append_of_none`,
`lookup_append_of_some`, and (goal-guided variant, with the goal rewritten
instead of a hypothesis) `lookup_table`.

## Run log

Lean-proof-methodology re-verification (2026-10-27, all green after the
`lookup_table` rcases restatement and `hbeq` rename):

```
$ lake clean && lake build
✔ [2/3] Built EmbedPipeline (827ms)
Build completed successfully (3 jobs).
$ lake env lean EmbedPipeline.lean
LEAN_CHECK_EXIT=0
```

Exit 0, zero errors, zero warnings.