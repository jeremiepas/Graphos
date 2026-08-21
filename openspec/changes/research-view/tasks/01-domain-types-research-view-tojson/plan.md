# Task 1 — Domain types: ResearchView + ToJSON — PLAN

**Task slug**: `01-domain-types-research-view-tojson`
**Attempt**: 1
**Status**: pending

## Summary

Define the `ResearchView`, `ResearchNode`, `ResearchEdge`, `ResearchCommunity`, `ResearchMetadata` domain types in `src/Graphos/Domain/Query/Research.hs` with `ToJSON` instances, and implement the deterministic term color assignment helper `assignTermColors`.

## Detail

### Scope

- **New module**: `src/Graphos/Domain/Query/Research.hs`
- **New record types** per design Decision 1:
  - `ResearchNode` — wraps a full `Node` from `gNodes`, plus `rnDiscoveredBy :: [Text]` (ordered by input term), `rnBestScore :: Double`, `rnScores :: [(Text, Double)]` (per-term, 0 if term did not match)
  - `ResearchEdge` — minimal edge representation with `source`, `target`, `type`, `confidence`
  - `ResearchCommunity` — `rcLabel :: Maybe Text`, `rcComposition :: Maybe CommunityComposition` (null when `gCompositions` absent), `rcMemberCount :: Int`
  - `ResearchMetadata` — `rmGeneratedAt :: UTCTime`, `rmGraphHash :: Text`, `rmNodeCount :: Int`, `rmEdgeCount :: Int`
  - `ResearchView` — `rvTerms`, `rvNodes`, `rvEdges`, `rvCommunities`, `rvMetadata`
- **`ToJSON` instances** for all records with explicit field names matching the spec:
  - `terms`, `nodes` (with `id`, `label`, `source_file`, `community`, `discovered_by`, `best_score`, `scores`), `edges` (`source`, `target`, `type`, `confidence`), `communities` (keyed by id, each with `label`, `composition`, `member_count`), `metadata` (`generated_at`, `graph_hash`, `node_count`, `edge_count`)
  - `composition` serializes as `null` when `gCompositions` is absent (legacy graph compatibility)
- **`assignTermColors :: [Text] -> Map Text HexColor`** — deterministic palette keyed by term (D3 schemeCategory10 or equivalent, cycles for > 10 terms)

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in `src/Graphos/Domain/Query/ResearchSpec.hs`
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: legacy graph composition is null` (spec `research-view` § "legacy graph composition is null")

**PASS conditions**:
- Module compiles with GHC 9.10, no warnings under `-Wall -Werror`
- All top-level definitions have explicit type signatures
- All modules have explicit exports
- JSON shape test: serializing a `ResearchView` produces the exact field names specified; nested objects use the specified keys (e.g., `scores` is an array of `{term, score}` objects)
- Round-trip test: `decode (encode rv) == Just rv` for a representative `ResearchView` value
- Null composition: a `ResearchCommunity` with `rcComposition = Nothing` serializes `composition` as JSON `null`, not omitted
- Term color determinism: `assignTermColors ["phase", "work"] == assignTermColors ["phase", "work"]`; `assignTermColors ["phase"] /= assignTermColors ["work"]` (distinct terms → distinct colors)
- Palette cycling: `assignTermColors` with > 10 terms cycles through the palette without error
- No IO dependencies in Domain module (strict rule compliance)

**FAIL boundaries**:
- If `ToJSON` uses default Aeson generic derivation with record field names that don't match the spec's snake_case (e.g., `rn_discovered_by` vs `discovered_by`), the test fails — field names must be explicit
- If `assignTermColors` returns different colors for the same input terms across calls (non-determinism), the test fails
- If the module imports any IO types or infrastructure modules, the test fails (domain purity violation)

### Affected modules

- **New**: `src/Graphos/Domain/Query/Research.hs`
- **New**: `test/Graphos/Domain/Query/ResearchSpec.hs`
- **Imports from**: `src/Graphos/Domain/Graph/Core.hs` (Node, Edge types), `src/Graphos/Domain/Community.hs` (CommunityId, CommunityComposition)

### Prerequisites

- Existing domain types (`Node`, `Edge`, `CommunityId`, `CommunityComposition`, `HexColor`) are available in the codebase
- `lsp` and `fgl` context knowledge available for type definitions

### Risks

- **Low**: Type definitions are straightforward; risk is mostly in matching the exact JSON field names specified by the spec
- **Medium**: `HexColor` type must be imported from the correct module; if it doesn't exist yet, it may need to be created in Domain first
