<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Add CommunityAggregate Domain type — DO

**Task slug**: `01-add-community-aggregate-type`
**Attempt**: 1
**Status**: pending

## Summary

Fix the `CommunityAggregate` Domain type in `src/Graphos/Domain/Types/Analysis.hs`: change `caInterCommunityEdges` from scalar `!Int` to `![(CommunityId, Int)]` and update the Aeson instances to serialize/deserialize as `[{"target": <cid>, "count": <n>}, ...]`.

## Detail

### Concrete Changes

**File: `src/Graphos/Domain/Types/Analysis.hs`**

1. Change the record field (line 93):
   ```haskell
   -- Before:
   , caInterCommunityEdges    :: !Int
   -- After:
   , caInterCommunityEdges    :: ![(CommunityId, Int)]
   ```

2. Replace the default `ToJSON`/`FromJSON` instances (derived from `Generic`) with custom instances that produce the `{"target":..,"count":..}` object shape:
   ```haskell
   instance ToJSON CommunityAggregate where
     toJSON ca = object
       [ "id"                      .= caId ca
       , "member_count"            .= caMemberCount ca
       , "cohesion"                .= caCohesion ca
       , "bridge_count"            .= caBridgeCount ca
       , "color"                   .= caColor ca
       , "label"                   .= caLabel ca
       , "representative_labels"   .= caRepresentativeLabels ca
       , "inter_community_edges"   .= ceToJSON (caInterCommunityEdges ca)
       ]
     where
       ceToJSON :: [(CommunityId, Int)] -> Value
       ceToJSON = Array . V.fromList . map (\(t, c) -> object ["target" .= (t :: Text), "count" .= c])

   instance FromJSON CommunityAggregate where
     parseJSON = withObject "CommunityAggregate" $ \v -> CommunityAggregate
       <$> v .: "id"
       <*> v .: "member_count"
       <*> v .: "cohesion"
       <*> v .: "bridge_count"
       <*> v .: "color"
       <*> v .: "label"
       <*> v .: "representative_labels"
       <*> v .: "inter_community_edges"
   ```

3. Add `import qualified Data.Vector as V` if needed for the `ceToJSON` helper.

### Key Decisions

- **Custom Aeson instances**: The `Generic`-derived instances would serialize `[(Int, Int)]` as flat arrays `[4, 5, 8, 2]`. The spec requires `[{"target":4,"count":5},{"target":8,"count":2}]`. Custom instances are the only way to achieve this with Aeson.
- **Field type uses `CommunityId` (which is `Int`)**: No newtype wrapper needed since `CommunityId` is already `Int` (from `Domain.Types.Graph`). The `ToJSON` instance for `CommunityId` inherits from `Int`.

### Dependencies

- Reads: `tasks/01-add-community-aggregate-type/plan.md`
- Unlocks: `tasks/01-add-community-aggregate-type/check.md`
