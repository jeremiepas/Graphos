# Check — 1.D Verify implementation of fidelity harness and subgraph subcommand.

## Verification Plan

1. **Compilation**
   - Run `cabal build --flag dev`.
   - Verify no errors with `-Werror`.

2. **Fidelity Harness: ImportEdgesSpec**
   - Run `cabal test --match ImportEdges`.
   - Verify it fails with a structured error on a zero-import graph.
   - Verify it passes on a correctly constructed graph.

3. **Fidelity Harness: GraphCoverageSpec**
   - Run `cabal test --match GraphCoverage`.
   - Verify it reports missing files correctly.
   - Verify it fails if coverage is below expected threshold.

4. **Subgraph CLI**
   - Run `cabal run graphos -- subgraph --help`.
   - Verify all flags (`--graph`, `--config`, `--out`, `--boundary-hops`, `--no-derive`) are recognized.
   - Run a sample subgraph extraction and verify the output JSON.

5. **Cleanup and Docs**
   - Verify `scripts/*.py` are gone.
   - Verify `README.md` reflects changes.

## Results

1. **Compilation** — PASS. `cabal build lib:graphos`, `cabal build exe:graphos` and
   `cabal build graphos-test` all succeed with `--flag dev` (`-Werror`). The full test suite
   runs 397 examples, 0 failures.

2. **ImportEdgesSpec** — PASS. `cabal test --match ImportEdges` runs 3 examples:
   - passes when the graph matches the on-disk imports (0 missing, 0 extra, recall ≥ 0.99);
   - fails when the graph is missing an import edge (structured `shouldBe`/`shouldNotBe`
     assertion failure, not an exception, with a `MISSING` listing in the report);
   - fails with recall 0.0 on a graph with zero imports edges.
   Baseline (Graphos repo): 3/3 pass.

3. **GraphCoverageSpec** — PASS. `cabal test --match GraphCoverage` runs 2 examples:
   - passes when every source file on disk is present in the graph;
   - groups missing files by cause (root-anchored build vs unexplained) and fails on the
     unexplained bucket (structured assertion failure).
   Baseline (Graphos repo): 2/2 pass.

4. **Subgraph CLI** — PASS. `graphos subgraph --help` lists the command; all five flags are
   recognized. Sample extraction against `graphos-out/graph.json` with a two-subsystem config
   produced `/tmp/subgraph-out.json` (152 nodes: 69 core / 10 boundary / 73 external; 2,758
   edges; every node carries `tier`/`subsystem`/`layer`, every edge carries
   `provenance: source`). The output loads under `graphos query`, `graphos explain` and
   `graphos neighbors --graph <out>` without schema errors. Missing `--config` exits 1.
   `--no-derive` produces the same output on this graph (no derived edges were in scope).

5. **Cleanup and Docs** — PASS with deviation. No harness Python scripts exist in `scripts/`
   (see do.md deviations), so there was nothing to remove. `README.md` documents all three
   components with purpose, invocation, flags, exit codes, and the `--config` schema.

## Notes / Deviations

- The plan's typescipt-repository baselines (203 import pairs, 86 missing files) are not reproducible
  in this repository; the Graphos-repo baselines above are the recorded "before" numbers for
  tasks 3, 5 and 8.
- `graphos subgraph` output carries real `imports` edges from the source graph; the derived-edge
  fallback (from `Import`-kind nodes) is exercised and unit-tested in `SubgraphSpec` (7/7
  passing) but produces no edges for this repo's graph because the Python `Import` nodes live
  outside the selected tiers.
