---
description: "Task 1 — CommunityComposition record + computation"
---
---
description: "Plan: define CommunityComposition record, ToJSON/FromJSON, computeCompositions, and tests"
---

# Task 1 — CommunityComposition record + computation — PLAN

**Task slug**: `01-community-composition-record-computation`
**Attempt**: 1
**Status**: completed

## Summary

Define the `CommunityComposition` domain record, implement `computeCompositions`, and add Hspec tests covering pure-code, balanced, paper-as-doc, round-trip JSON, membership-based counting, cross-type edge filtering, and dominant-kind filtering of Nothing.

## Detail

### Scope

- **Add record** in `src/Graphos/Domain/Community.hs` (existing module — no new file):
  `CommunityComposition { ccCodeCount, ccDocCount, ccOtherCount, ccDominantKind, ccMixedRatio, ccCodeDocEdges }`
- **Add ToJSON / FromJSON** with snake_case field names (`code`, `doc`, `other`, `dominant_kind`, `mixed_ratio`, `code_doc_edges`)
- **Add `computeCompositions`** function:
  `computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition`
- **Add Hspec tests** in `test/` (or appropriate test module for Domain/Community)

### Spec Scenarios (from specs/cluster-composition/spec.md)

| Spec ID | Scenario | Task Coverage |
|---|---|---|
| SC-1.1 | Pure-code community: 10 CodeFile, 0 doc → ccMixedRatio = 0.0 | Record field correctness, ratio formula |
| SC-1.2 | Balanced mixed community: 8 CodeFile + 8 DocFile → ccMixedRatio = 1.0 | Ratio boundary |
| SC-1.3 | Paper counted as doc: 6 CodeFile + 3 PaperFile → ccDocCount = 3, ccMixedRatio = 0.5 | Doc-like classification |
| SC-2.1 | Composition counts match membership: 12+4+0, 3 cross References edges → full JSON record | computeCompositions correctness |
| SC-2.2 | Cross-type edge count excludes non-References: 3 References + 5 contains → ccCodeDocEdges = 3 | Edge-type filtering |
| SC-2.3 | Dominant kind ignores Nothing: 5 function, 3 Nothing, 2 module → Just "function" | Kind aggregation |

### Check Criteria (defined BEFORE code)

**Tests to run:**
```bash
cabal test --test-options="-unit Task1"   # or the specific test module
# Full suite:
cabal test
```

**Spec scenario gates:**

| ID | Test name pattern | PASS condition | FAIL condition |
|---|---|---|---|
| SC-1.1 | `pure-code.*ratio.*zero` or equivalent | `ccMixedRatio == 0.0`, `ccCodeCount == 10`, `ccDocCount == 0`, `ccOtherCount == 0` | Any assertion fails or ratio != 0.0 |
| SC-1.2 | `balanced.*mixed.*community` | `ccMixedRatio == 1.0` | `ccMixedRatio /= 1.0` |
| SC-1.3 | `paper.*doc` or `paper.*counted` | `ccDocCount == 3`, `ccMixedRatio == 0.5` | `ccDocCount /= 3` or ratio != 0.5 |
| SC-2.1 | `composition.*counts.*match` | Full JSON record matches: code=12, doc=4, other=0, dominant_kind=Just "function", mixed_ratio=0.33, code_doc_edges=3 | Any field mismatch |
| SC-2.2 | `cross-type.*edge.*count.*excludes` | `ccCodeDocEdges == 3` (not 8) | `ccCodeDocEdges /= 3` |
| SC-2.3 | `dominant.*kind.*ignores.*nothing` | `ccDominantKind == Just "function"` | `ccDominantKind /= Just "function"` |

**JSON round-trip gate:**
- Parse `ToJSON` output through `FromJSON` and assert equality of the original `CommunityComposition` value
- PASS: `parsed == original`
- FAIL: `parsed /= original` or decode error

**JSON field name gate:**
- Parse the JSON output and verify keys are snake_case: `code`, `doc`, `other`, `dominant_kind`, `mixed_ratio`, `code_doc_edges`
- PASS: all keys present with correct snake_case names
- FAIL: missing key or incorrect name (e.g., camelCase)

**Exact FAIL boundaries:**
- If `ccMixedRatio` formula is implemented as `min/max` but doesn't handle `max == 0` → ratio should be 0, not `NaN`/`DivideByZero`
- If `ccDocCount` excludes `PaperFile` or `OfficeFile` → partial doc count
- If `ccCodeDocEdges` counts all edge types (not just References) → inflated cross edges
- If `ccDominantKind` returns `Nothing` when all kinds are `Nothing` → should return `Nothing` (this is actually correct), but if it returns a wrong kind when `Nothing` nodes exist → FAIL

### Affected Modules

- `src/Graphos/Domain/Community.hs` — add `CommunityComposition` record + `computeCompositions`
- Test module under `test/` (create or extend existing Community tests)

### Prerequisites

- Existing `Graph` type with `gNodes :: Map NodeId Node` and `gEdges :: Map EdgeId Edge`
- Existing `Node` type with `nodeFileType :: FileType` and `nodeKind :: Maybe Text`
- Existing `Edge` type with edge type field (`type :: Text` or `edgeType`)
- Existing `CommunityMap :: Map CommunityId [NodeId]`
- Existing `FileType` type with `CodeFile`, `DocFile`, `PaperFile`, `OfficeFile`, `ImageFile`, `VideoFile`, `AudioFile` constructors

### Risks

- **Risk**: Circular imports — `Community.hs` must not import `Graph/Core.hs`. Design: `computeCompositions` takes `Graph` as a parameter, no module-level dependencies.
- **Risk**: `nodeKind :: Maybe Text` — must handle `Nothing` correctly in dominant kind calculation.
- **Risk**: Edge type comparison — edge type representation (Text vs custom type) must match existing `References` constant.
- **Low risk**: Adding fields to an existing module is low-impact; no structural changes.

## Result

All subtasks completed. `CommunityComposition` record added to `src/Graphos/Domain/Community.hs` with `computeCompositions` function. JSON instances use snake_case field names. Build passes with `-Wall -Werror`. Tests pass (633 examples, 0 failures).
