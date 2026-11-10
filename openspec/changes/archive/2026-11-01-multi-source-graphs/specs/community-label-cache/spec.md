## Purpose

Persist LLM-generated community labels between runs and reuse them when Leiden leaves a community recognizably intact, cutting labeling cost on update runs and source additions.

## ADDED Requirements

### Requirement: Label cache persistence

The pipeline SHALL persist community labels in `<output>/cache/label-cache.json`. Each entry SHALL record the labeling model, a fingerprint of the community's sorted member NodeIds, the member NodeId list, and the label text. The cache SHALL be a last-run snapshot: after each labeling pass the file SHALL contain exactly the current run's communities' entries (previous entries not matching any current community are discarded). The cache file SHALL survive staged-rebuild directory swaps (carried with `cache/` state). Labeling SHALL consult only entries whose recorded model equals the current labeling model; a model change invalidates all entries. (PRD §5, workflow 11)

#### Scenario: Cache written after labeling

- **WHEN** a pipeline run with labeling completes
- **THEN** `<output>/cache/label-cache.json` SHALL contain one entry per labeled community with model, member-set fingerprint, member list, and label

#### Scenario: Cache survives staged rebuild

- **WHEN** a second run completes via the staged-rebuild swap
- **THEN** the label cache SHALL be present in the new output directory with the current run's entries

#### Scenario: Model change invalidates entries

- **WHEN** the labeling model changes between two runs
- **THEN** no label from the previous run's cache SHALL be reused for the second run

### Requirement: Fuzzy v1 label matching — bidirectional containment

For each community needing a label, the pipeline SHALL look up the cache as follows. An exact fingerprint match SHALL reuse the cached label without an LLM call. When no exact match exists, a cached entry `old` is a fuzzy match for the new community `new` iff `|new ∩ old| / |old| ≥ 0.8` AND `|new ∩ old| / |new| ≥ 0.8` (bidirectional containment: old survives in new and new is mostly old). Among multiple fuzzy candidates the pipeline SHALL choose the one with the largest intersection size, tie-broken by the lowest community id. Fuzzy matches SHALL reuse the cached label without an LLM call. Communities with no exact or fuzzy match SHALL be labeled by the LLM and written to the cache. (PRD §5, workflow 11)

#### Scenario: Exact member-set match reuses label

- **WHEN** a new community's sorted member set equals a cached entry's member set for the same model
- **THEN** the cached label SHALL be reused and zero LLM labeling calls SHALL be made for it

#### Scenario: Boundary drift reuses label

- **WHEN** a cached community of 100 members persists and its new counterpart differs by 5 members in and 5 members out (ratios 0.95 both ways)
- **THEN** the cached label SHALL be inherited without an LLM call

#### Scenario: Split communities re-label

- **WHEN** a cached community splits into two communities of roughly half its members each (containment from old ≥ 0.8 fails against each half)
- **THEN** both new communities SHALL be LLM-labeled

#### Scenario: Merged community re-labels

- **WHEN** two cached communities merge into one new community (new-side containment of either old community < 0.8)
- **THEN** the merged community SHALL be LLM-labeled rather than inheriting either label

#### Scenario: Merge does not shadow exact-match lookups

- **WHEN** one new community exactly matches cached entry X while another new community merely contains all of cached entry X (new-side ratio 0.5)
- **THEN** the exact-match community SHALL inherit X's label and the other community SHALL NOT inherit X's label

#### Scenario: Tie-break by intersection then community id

- **WHEN** two cached entries fuzzy-match a new community with equal intersection sizes
- **THEN** the entry associated with the lower community id SHALL win

#### Scenario: Majority-drift community re-labels

- **WHEN** a new community shares fewer than 80 percent of its members with any cached entry
- **THEN** the community SHALL be LLM-labeled and its label written to the cache

### Requirement: Cache consult ordering and cost accounting

The labeling stage SHALL report (log line at completion) the count of labels served from the cache (exact and fuzzy separately) and the count made via LLM calls. On a run where no community changed and the model is unchanged, the labeling stage SHALL make zero LLM calls. (PRD §5, workflow 11; PRD §10 observability)

#### Scenario: No-change update makes zero LLM calls

- **WHEN** `graphos --update` runs over an unchanged source set and model
- **THEN** the labeling stage SHALL make zero LLM labeling calls and every community label SHALL come from the cache

#### Scenario: Cost accounting reported

- **WHEN** labeling completes with mixed hits and misses
- **THEN** the completion log SHALL state the counts of exact hits, fuzzy hits, and LLM-labeled communities