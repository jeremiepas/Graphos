## ADDED Requirements

### Requirement: Edge betweenness scale guards (AVI-534 SG-1/SG-2/SG-3)

Edge betweenness computation SHALL respect the AVI-534 §4 scale guards, grounded in `docs/math-requirements/AVI-534-structural-analysis-complexity.md`:

- **SG-1 (sampled-source cap).** Sampled betweenness draws at most `cfgMaxSampledSources` sources (default 500, CLI `--max-sampled-sources`), in a deterministic seedable order (ascending sequential FGL index, §9). The estimator MUST rescale per-source contributions by `N/|S|` so the cap does not bias the mean (Lemma 3.1).
- **SG-2 (exact all-pairs cap).** Exact O(N·M) all-pairs betweenness MUST NOT be invoked when `N > cfgExactBetweennessNodeCap` (default 10000, CLI `--exact-betweenness-node-cap`); the sampled estimator (SG-1) is used instead. This is a complexity-honesty constraint, not a product decision.
- **SG-3 (per-source memory).** The Brandes forward/backward pass per source MUST allocate O(N + M) working memory over the shared `CachedFGL` and MUST NOT build a fresh structure per source beyond that.

#### Scenario: exact all-pairs bypassed above the cap (AC-3)

- **WHEN** edge betweenness is requested for a graph with `N > cfgExactBetweennessNodeCap`
- **THEN** the sampled estimator runs over at most `cfgMaxSampledSources` sources with the `N/|S|` rescale applied; the exact O(N·M) pass is not executed (unit-testable with a small synthetic cap)

#### Scenario: sampled estimator is unbiased (AC-4)

- **WHEN** the sampled estimator is run with `s = N` (all sources, cap ≥ N)
- **THEN** `ŷ_V` equals the exact all-pairs Brandes result exactly (relative difference 0), because the `N/s` rescale with `s = N` is the identity

#### Scenario: sampled relative error within ε on sparse graphs (AC-2)

- **WHEN** exact `BC` and sampled `ŷ_S` are computed on a sparse synthetic graph (`N ∈ {500, 2000}`, `M ≈ 5N`, deterministic construction) with a deterministic sample of `s` sources
- **THEN** `‖ŷ_S − BC‖₂ / ‖BC‖₂ ≤ ε` for the configurable `ε` (default 0.1), verified in the AVI-573 analysis spec
