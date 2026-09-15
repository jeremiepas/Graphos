# Tasks: avi-573-edge-betweenness

<!-- Historical record of AVI-573 (commit bbb6209). Every task is complete and
     verified — this dir backfills the change record to close the AVI-738
     openspec-verifier adverse finding. Tasks are PDCA-per-task, foundational first. -->

## 1. Brandes edge betweenness replaces the biased per-pair esp heuristic (Domain)

- [x] 1.P Plan: Replace the biased per-pair `esp` heuristic in
  `Graphos.Domain.Graph.Analysis.edgeBetweenness` with standard Brandes
  (`BC(e) = Σ σ(s,t|e)/σ(s,t)`): exact `O(N·M)` all-pairs and sampled
  `O(s·(N+M))`. Check criteria: (a) exact routine reproduces the §8 golden
  fixture `{b,c}` + bridges + `BC(b–c) = 2/3`; (b) sampled estimator is unbiased
  (AC-4); (c) `cabal build -Werror` + suite green.
- [x] 1.D Do: Brandes forward/backward pass implemented; exact + sampled paths;
  biased `esp` code removed (`Graph.hs`, `Analysis.hs`).
- [x] 1.C Check: `cabal test` PASS — `StructuralSpec.hs` AC-1..AC-4 golden fixtures.
- [x] 1.A Act: Semantics recorded in `bounded-edge-inference` + `fgl-adapter` specs
  and the `Analysis.hs` Haddock surface.

## 2. Undirected reverse-embedding for FGL (AF-3)

- [x] 2.P Plan: `toCachedFGL` SHALL embed every edge `(u,v)` of an undirected graph as
  both `(u,v)` and `(v,u)` (self-loops not duplicated) so FGL single-pass traversals
  see the support graph `G_s`. Check criteria: (a) `articulationPoints` on path
  `a–b–c–d` returns `{b,c}`; (b) undirected `edgeBetweenness` sums unordered pairs;
  (c) `cabal build -Werror` + suite green.
- [x] 2.D Do: reverse-embedding in `toCachedFGL`; unordered-pair normalization
  (`srcIdx < tgtIdx`) so `BC(b–c) = 2/3` on the 4-node path.
- [x] 2.C Check: `cabal test` PASS — undirected `ap`/`edgeBetweenness` scenarios in
  `StructuralSpec.hs`.
- [x] 2.A Act: AF-3 recorded in `fgl-adapter` spec (Requirement + 2 scenarios).

## 3. Complexity assertions for ap/bcc/dominators (AC-1)

- [x] 3.P Plan: Document/assert worst-case bounds in the `(N, M)` model — `ap`/`bcc`
  `O(N+M)`, `dom` reducible `O(N+M)` / irreducible `O(N·M)`, `edgeBetweenness`
  `O(N·M)` exact / `O(s·(N+M))` sampled. Check criteria: (a) cost model asserted in
  `fgl-adapter` spec and `Analysis.hs` Haddock; (b) golden fixtures verify the
  semantic results; (c) `cabal build -Werror` + suite green.
- [x] 3.D Do: Theorem references (2.1/2.2/2.3, CHW 1982, Tarjan 1972, Brandes 2001)
  added to the spec and Haddock; reducible-class justification for `dom`.
- [x] 3.C Check: `cabal test` PASS — `ap`/`bcc`/`dom` golden fixtures in
  `StructuralSpec.hs`.
- [x] 3.A Act: Complexity-honesty bound recorded; irreducible worst case documented.

## 4. Complexity-honesty scale guards SG-1/SG-2/SG-3 (Domain + CLI)

- [x] 4.P Plan: Sampled-source cap (`cfgMaxSampledSources`, default 500,
  `--max-sampled-sources`) with `N/|S|` rescale (SG-1); exact all-pairs bypass above
  `cfgExactBetweennessNodeCap` (default 10000, `--exact-betweenness-node-cap`) (SG-2);
  per-source `O(N+M)` memory over `CachedFGL` (SG-3). Check criteria: (a) exact pass
  not executed above the cap (AC-3); (b) `s = N` rescale is identity → unbiased (AC-4);
  (c) relative error ≤ ε on sparse synthetic graph (AC-2); (d) CLI flags wired into
  `Parser.hs` / `Pipeline.hs`.
- [x] 4.D Do: SG-1/SG-2/SG-3 wired through config + CLI; rescale applied in sampled
  estimator; bounds documented in `bounded-edge-inference` spec.
- [x] 4.C Check: `cabal test` PASS — AC-2/AC-3/AC-4 scenarios in `StructuralSpec.hs`.
- [x] 4.A Act: Scale-guard Requirements recorded in `bounded-edge-inference` spec with
  scenarios AC-2/AC-3/AC-4.

## 5. Verification & reconciliation (AVI-738 remediation)

- [x] 5.P Plan: Backfill the change-dir record for AVI-573 and archive it, closing the
  AVI-738 openspec-verifier adverse finding. Reconcile so canonical specs are not
  duplicated (they already contain the AVI-573 requirements verbatim via bbb6209).
  Check criteria: (a) `openspec validate --json` passes for the new change dir;
  (b) after archive, the dir lives under `openspec/changes/archive/`; (c) `git diff`
  shows `openspec/specs/bounded-edge-inference/spec.md` and `openspec/specs/fgl-adapter/
  spec.md` untouched by this commit.
- [x] 5.D Do: Change dir created under `openspec/changes/`, archived with
  `openspec archive --yes --skip-specs` (canonical specs left intact), committed on
  `AVI-512-add-math-in-graphos-project`, pushed to origin.
- [x] 5.C Check: `openspec validate --json` green; canonical spec files unchanged in
  the commit; branch pushed.
- [x] 5.A Act: Historical record archived; AVI-738 finding closed by reconciliation.
