## 1. Lean arm

- [x] 1.1 `Seam/UndirectedScoring.lean`: `RecordGraph`, `legacyNum`, `supportPairs`, `toEdgeGraph`, `fixed_range_bound`, `fixedM_pos`, `supportPairs_eq_self`, `mem_or_rev_mem_supportPairs`, witnesses `twoArcs` / `bothWays`. Check: `lake build` (pinned v4.14.0) exits 0 with no warnings; `#print axioms` lists only `propext`, `Quot.sound`
- [x] 1.2 Root import and README row
- [x] 1.3 `Harness/CrossCheck.lean`: add the rows `twoArcs` (`Qnum 8 / 16`) and `bothWays` (`Qnum 8 / 16`); regenerate `crosscheck/q-lean.tsv` with `lake exe modularity-crosscheck --tsv`; `--check` green
  - `allRows = crossCheckRows ++ seamRows` drives the TSV, the table and the range check; `lake build modularity-crosscheck` (v4.14.0) clean; `--check` reports the table current, 14 data lines; both rows `m = 2`, `qnum = 8`, `Q = 1/2`.

## 2. Haskell fix

- [x] 2.1 `Domain/Community.hs`: `supportPairs` (D2); `computeCommunityStats` derives degrees, `σ_tot`, `σ_in` and `m` from it, so `modularity` ignores `gDirected` (D1). Check: `ModularityCrossCheckSpec` matches all nine rows within `2^-24`
  - `supportPairs` (ascending `(min, max)` pairs from `gEdges` keys, `u /= v`, both endpoints present) feeds degrees, `m`, `σ_tot` and `σ_in`. `ModularityCrossCheckSpec` now has 13 rows (Haskell `twoArcs` built `--directed`, `bothWays` with both orientations) and compares `m` with `length (supportPairs g)`.
- [x] 2.2 QuickCheck in `CommunitySpec`: for any graph and partition, `modularity g cm == modularity (undirectedView g) cm`; adding the reverse of an existing edge leaves `modularity` unchanged; `-0.5 ≤ modularity ≤ 1`
  - `CommunitySpec` "modularity on the undirected support graph": three QuickCheck properties over random 2–8-node record graphs (any ordered pairs, random `gDirected`, random partition).
- [x] 2.3 `ClusterSpec`: on a `--directed` fixture, the report's `cqModularity` is in `[-0.5, 1]` and equals `modularity` of the undirected projection; `finalComm` covers every node (D3)
  - `ClusterSpec` "report modularity on a --directed build", for Leiden and Bunch: `runClustering` → `capPartition` → `partitionQuality`, as the pipeline does.
- [x] 2.4 Leiden unchanged: `DeterminismSpec` and `PermutationSpec` pass; `cluster-algorithm leiden` partitions stay byte-identical
  - Full suite green, `DeterminismSpec` / `PermutationSpec` included; no clustering code reads `computeCommunityStats`.

## 3. Validation on solario-core

- [x] 3.1 Rebuild solario-core `origin/main` with `--granularity function --directed`: the report prints `modularity ≈ 0.336` (Python reference 0.3360 from `graph.json`, previously 0.6485)
  - 2026-11-19, solario-core `14d86be28`, `--granularity function --directed`: 146,526 nodes, 666,966 edges, 656,428 support pairs (10,538 pairs stored both ways); report **modularity = 0.4042**, equal to the Python recomputation on the undirected support graph of the exported `graph.json` (0.4042). The 0.336 reference was taken on the 107,005-node graph before D4 import resolution and doc nodes; the check that holds is report = undirected recomputation. 976 s.
- [x] 3.2 File-granularity build: the report's modularity is in `[-0.5, 1]` (previously 1.2149)
  - `--granularity file --directed`: 42,572 nodes, 61,445 edges; report **modularity = 0.6699** (was 1.2149), Python recomputation 0.6699. 69 s.
- [x] 3.3 Refresh any recorded report baseline that quotes `modularity = ` (`docs/`, `openspec/`)
  - No live doc quotes a report modularity. `bunch-real-projects/tasks.md` quotes undirected Bunch builds: `src/cli` re-measured, unchanged (0.3675); the pc26 figure comes from an undirected build of the same kind and is left as recorded. Archived changes are historical and not edited.

## 4. Close-out

- [x] 4.1 `openspec validate modularity-undirected-view --strict`
  - Valid.
- [x] 4.2 Full `cabal test` green; archive the change
  - 2026-11-19: `cabal build --flag dev all --enable-tests` clean; full suite 1,730 examples, 0 failures, no skips.
