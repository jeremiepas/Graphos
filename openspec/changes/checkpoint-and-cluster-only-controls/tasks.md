## 1. CLI

- [ ] 1.1 Add `--fresh` / `--no-checkpoint` flag
- [ ] 1.2 Ensure `--cluster-only` parses and is mutually sensible with `--fresh`

## 2. Checkpoint decision

- [ ] 2.1 Centralize checkpoint discovery/selection logic
- [ ] 2.2 Bypass discovery when `--fresh` is set
- [ ] 2.3 Log `Resuming from checkpoint <path> (age ...)` vs `Full extraction` at INFO

## 3. Genuine cluster-only

- [ ] 3.1 Load nodes+edges from checkpoint and enter pipeline at Cluster stage
- [ ] 3.2 Skip Extract and Infer stages under `--cluster-only`
- [ ] 3.3 Error clearly when `--cluster-only` has no usable checkpoint
- [ ] 3.4 Minimal checkpoint provenance check; warn on input mismatch

## 4. Verification

- [ ] 4.1 `cabal build --flag dev` with `-Werror`
- [ ] 4.2 `cabal test` green (entry-point selection, flag behavior)
- [ ] 4.3 Smoke: `--cluster-only` runs with no extraction logs; `--fresh` performs full extraction
