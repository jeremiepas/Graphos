# Do — 1.D Implement the pure Subgraph module, the two Hspec spec modules, the graphos subgraph CLI subcommand, update graphos.cabal, remove Python scripts, document in README.md, and record baseline numbers.

## What was done

1. **`src/Graphos/UseCase/Subgraph.hs`** (pure, no IO) — implemented `extractSubgraph`,
   `SubgraphConfig`, `SubsystemConfig`, `SubgraphTier`, `EdgeProvenance`, `architecturalLayer`.
   Core nodes are selected by `matchGlob` patterns (leading `./` stripped via `normalizePath`),
   boundary nodes via BFS over `imports` edges in both directions within `max_hops`, external
   nodes are import targets outside core/boundary. Every kept node carries `tier`,
   `subsystem` (core only) and `layer` metadata; every kept edge carries `provenance`
   (`source`/`derived`). Derivation (`deriveImports`) resolves `Import`-kind node labels to
   specifiers (quoted or bare), skips pairs already present as real `imports` edges
   (idempotent), resolves relative specifiers with `.js`→`.ts`/`.tsx` and `index.*` barrel
   rewrites, and materializes `ext:<pkg>` external nodes for package imports.

2. **`tests/Graphos/Fidelity/ImportEdgesSpec.hs`** — on-disk oracle: recursively scans a
   fixture tree, resolves quoted import/re-export specifiers against disk (probing source
   extensions), and compares with the `imports` edges of a `graph.json`. Reports
   ground-truth pair count, graph edge count, precision/recall (threshold 0.99) and explicit
   `MISSING`/`EXTRA` pair listings. Tests: perfect agreement passes; missing edge fails;
   zero-imports graph fails with recall 0.0 — all as structured Hspec failures, no exceptions.

3. **`tests/Graphos/Fidelity/GraphCoverageSpec.hs`** — disk-vs-graph coverage accounting:
   classifies missing files by the ignore-rule class that most plausibly explains them
   (root-anchored `build` output, depth-independent tooling, `.gitignore`, unexplained) and
   fails when any file is unexplained. Tests: full coverage passes; missing files grouped by
   cause with an unexplained bucket failing the gate.

4. **`graphos.cabal`** — no changes required: `Graphos.UseCase.Subgraph` was already in
   `exposed-modules`, both Fidelity specs and `SubgraphSpec` already in the test-suite
   `other-modules`, and `aeson`/`directory`/`filepath`/`hspec`/`temporary` already in
   `build-depends`.

5. **`graphos subgraph` CLI** — added `SubgraphCmd` to `src/Graphos/CLI/Parser.hs` with flags
   `--graph`, `--config`, `--out`/`-o`, `--boundary-hops`, `--no-derive`; wired the handler in
   `app/Main.hs` (`toLabeledGraph` converts the rich `Graph` to `LabeledGraph`), with
   `exportSubgraphJSON` added to `src/Graphos/Infrastructure/Export/JSON.hs` so the output is
   loadable via `--graph`. Missing `--config` exits 1. `renderCommandReference` updated and the
   two scaffold golden fixtures regenerated to match.

6. **Python scripts** — `scripts/validate_import_edges.py`, `scripts/graph_coverage.py` and
   `scripts/subgraph_from_patterns.py` referenced by the plan do **not exist** in this
   repository (the actual `scripts/` contains only `audit_graph.py`, `measure_html_payload.py`,
   `openspec-orch`, `otel-up.sh`, `otel-down.sh`). Nothing to remove — see deviations.

7. **`README.md`** — the Extraction Fidelity Harness section now documents all three components
   (purpose, invocation, flags, exit codes, no external interpreter) with runnable commands and
   the `--config` schema. Fixed the `--match` patterns (the `/Fidelity/...` slash form matches
   0 examples under hspec-discover; the bare `ImportEdges`/`GraphCoverage` forms are correct).

8. **Baseline numbers** — captured on the Graphos repository (see check.md):
   `ImportEdgesSpec` 3/3 pass; `GraphCoverageSpec` 2/2 pass; `SubgraphSpec` 7/7 pass;
   full suite 397 examples, 0 failures. Sample `graphos subgraph` run: 152 nodes
   (69 core / 10 boundary / 73 external), 2,758 edges, all `provenance: source` (the
   Python `Import` nodes that would derive edges are outside the selected tiers).

## Deviations

- `SubgraphCmd` config is `Maybe FilePath` because optparse-applicative 0.18.1.0 does not
  export `required`; missing `--config` is a runtime error + `ExitFailure 1`.
- `parseSpecifier` additionally accepts bare specifiers (Python `from pathlib import Path`)
  in addition to quoted JS/TS specifiers.
- The plan's Python harness scripts were never present in this repository; no deletion needed.
- The typescipt-repository baselines (203 pairs, 86 missing files) are not reproducible here; the
  Graphos-repo baselines above are recorded instead for later comparison.
- Fixed a pre-existing build breaker unrelated to this task: `src/Graphos/Domain/Graph/Core.hs`
  imported `Control.DeepSeq ()` but used `NFData` (`instance NFData Graph`) — now
  `import Control.DeepSeq (NFData(..))`.
