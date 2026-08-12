<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 5 — Add writeCommunityAggregates to IncrementalJSON.hs — DO

**Task slug**: `05-add-write-community-aggregates`
**Attempt**: 1
**Status**: pending

## Summary

Add `writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()` to `src/Graphos/Infrastructure/Export/IncrementalJSON.hs`. Mirrors the existing `writeGodNodes` pattern — uses `writeKey` + `BSL.hPut` + `encode`.

## Detail

### Concrete Changes

**File: `src/Graphos/Infrastructure/Export/IncrementalJSON.hs`**

Add the function and export it:
```haskell
writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()
writeCommunityAggregates iw aggregates = do
  writeKey iw "community_aggregates"
  BSL.hPut (iwHandle iw) (encode aggregates)
```

This is a straightforward addition. The function:
1. Writes the JSON key `"community_aggregates"` (with comma handling via `writeKey`)
2. Serializes the list via `encode` (Aeson) and streams it to the handle

**File: `tests/IncrementalJSONSpec.hs`** (or extend existing tests)

Add a round-trip test:
```haskell
it "writes community_aggregates to a temp file with correct JSON" $
  withTempFile $ \path -> do
    iw <- openWriter path
    writeCommunityAggregates iw [agg1, agg2]
    closeWriter iw
    content <- readFile path
    -- Parse and verify
    let parsed = decode content :: Maybe [Value]
    parsed `shouldBe` Just [toJSON agg1, toJSON agg2]
```

### Key Decisions

- **Placement in pipeline**: Called after `writeGodNodes` and before `writeAnalysisTail`. The JSON structure requires aggregates to appear in this position.
- **No custom encoding**: Uses Aeson's default `encode` for `[CommunityAggregate]` — the custom `ToJSON` instance from Task 1 handles the `inter_community_edges` shape.
- **Test helper pattern**: The temp-file round-trip pattern is reusable for other incremental writer tests. Consider extracting to a shared test helper module.

### Dependencies

- Requires: Task 1 completed (`CommunityAggregate` with correct `ToJSON`)
- Reads: `tasks/05-add-write-community-aggregates/plan.md`
- Unlocks: `tasks/05-add-write-community-aggregates/check.md`
