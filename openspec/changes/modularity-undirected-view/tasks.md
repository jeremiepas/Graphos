## 1. Lean arm

- [x] 1.1 `Seam/UndirectedScoring.lean`: `RecordGraph`, `legacyNum`, `supportPairs`, `toEdgeGraph`, `fixed_range_bound`, `fixedM_pos`, `supportPairs_eq_self`, `mem_or_rev_mem_supportPairs`, witnesses `twoArcs` / `bothWays`. Check: `lake build` (pinned v4.14.0) exits 0 with no warnings; `#print axioms` lists only `propext`, `Quot.sound`
- [x] 1.2 Root import and README row
- [ ] 1.3 `Harness/CrossCheck.lean`: add the rows `twoArcs` (`Qnum 8 / 16`) and `bothWays` (`Qnum 8 / 16`); regenerate `crosscheck/q-lean.tsv` with `lake exe modularity-crosscheck --tsv`; `--check` green

## 2. Haskell fix

- [ ] 2.1 `Domain/Community.hs`: `supportPairs` (D2); `computeCommunityStats` derives degrees, `σ_tot`, `σ_in` and `m` from it, so `modularity` ignores `gDirected` (D1). Check: `ModularityCrossCheckSpec` matches all nine rows within `2^-24`
- [ ] 2.2 QuickCheck in `CommunitySpec`: for any graph and partition, `modularity g cm == modularity (undirectedView g) cm`; adding the reverse of an existing edge leaves `modularity` unchanged; `-0.5 ≤ modularity ≤ 1`
- [ ] 2.3 `ClusterSpec`: on a `--directed` fixture, the report's `cqModularity` is in `[-0.5, 1]` and equals `modularity` of the undirected projection; `finalComm` covers every node (D3)
- [ ] 2.4 Leiden unchanged: `DeterminismSpec` and `PermutationSpec` pass; `cluster-algorithm leiden` partitions stay byte-identical

## 3. Validation on solario-core

- [ ] 3.1 Rebuild solario-core `origin/main` with `--granularity function --directed`: the report prints `modularity ≈ 0.336` (Python reference 0.3360 from `graph.json`, previously 0.6485)
- [ ] 3.2 File-granularity build: the report's modularity is in `[-0.5, 1]` (previously 1.2149)
- [ ] 3.3 Refresh any recorded report baseline that quotes `modularity = ` (`docs/`, `openspec/`)

## 4. Close-out

- [ ] 4.1 `openspec validate modularity-undirected-view --strict`
- [ ] 4.2 Full `cabal test` green; archive the change
