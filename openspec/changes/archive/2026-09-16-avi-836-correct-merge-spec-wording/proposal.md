## Why

`openspec/specs/09-merge` §3.2 (workflow-09 requirement + scenario) states the merge is
"last-write from B wins", but `Domain.Graph.Core.mergeGraphs` (`Core.hs:127-132`) uses
`Data.Map.union` / `gNodes old <> gNodes new`, which is **left-biased**: view A (the left
operand) keeps its content at conflicting NodeIds and B's exclusive keys are unioned in. The
property test `tests/Graphos/Domain/Graph/ColimitPropertySpec.hs` (AC-5) and
`tests/Graphos/Domain/GraphSpec.hs:242` already assert old-wins and pass, so code + tests agree;
only the spec prose is wrong. This change corrects the prose to old-wins and aligns the matching
workflow doc.

## What Changes

- Correct `openspec/specs/09-merge` workflow-09 requirement: "last-write from B wins" → old-wins
  (`Data.Map.union` left bias; view A wins at conflicting keys; B's exclusive keys unioned).
- Correct the matching `#### Scenario:` bullet: "duplicate NodeIds merged (B wins)" → old-wins.
- Align `docs/workflows/09-merge.md` ("last-write wins from graph B" / "last-write on collision").
- Leave the immutable `openspec/changes/archive/*` snapshots untouched (historical record).

## Capabilities

### Modified Capabilities
- `09-merge`: documentation-only correction of conflict-resolution direction; no behavior change.

## Impact

- **Docs only.** `src/`, `tests/`, and the build are unaffected. Follows the openspec workflow
  (change dir + `openspec archive`) so the `openspec-archived` / `openspec validate` CI stays green.
