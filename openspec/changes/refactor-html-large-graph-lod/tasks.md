<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass — record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P → D → C → A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

## 1. Add `CommunityAggregate` Domain type

- [ ] 1.P Plan: Add a new Domain type `CommunityAggregate` in `src/Graphos/Domain/Types/Analysis.hs` (or a new `Domain/Types/CommunityAggregate.hs`) with fields `caId :: Int`, `caMemberCount :: Int`, `caCohesion :: Double`, `caBridgeCount :: Int`, `caColor :: Text`, `caLabel :: Text`, `caRepresentativeLabels :: [Text]` (max 3), `caInterCommunityEdges :: [(Int, Int)]` (target community id, edge count). Add `ToJSON`/`FromJSON` instances (Aeson, field-naming snake_case to match `graph.json` convention: `id`, `member_count`, `cohesion`, `bridge_count`, `color`, `label`, `representative_labels`, `inter_community_edges`). Export from `Graphos.Domain.Types`. Affected areas: `Domain.Types` only. Risks: introducing the type in the wrong layer (must be Domain — pure, no IO). Check criteria: (a) `cabal build` passes with `-Werror`; (b) `ToJSON` output keys match the snake_case list exactly; (c) type lives under `src/Graphos/Domain/`; (d) no IO imports in the module.
- [ ] 1.D Do: Implement the `CommunityAggregate` type + instances + re-export.
- [ ] 1.C Check: (a) `cabal build` → PASS/FAIL. (b) Write a QuickCheck property or Hspec case asserting `toJSON` produces the 8 expected keys with snake_case names; record PASS/FAIL. (c) Confirm module path under `src/Graphos/Domain/` via `ls`; PASS/FAIL. (d) `grep` for `IO` in the new module → no hits; PASS/FAIL.
- [ ] 1.A Act: If all criteria pass, standardize the type as the canonical aggregate shape for future consumers (HTML, Obsidian, SVG). If any FAIL, fix before moving on; record the failure in Attempt history.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Implement `joinCommunitiesToNodes` UseCase function

- [ ] 2.P Plan: Add a pure function `joinCommunitiesToNodes :: Graph -> CommunityMap -> Graph` in `src/Graphos/UseCase/Cluster.hs` (or a new `UseCase.Join.hs`) that maps over `gNodes`, looks up each `NodeId` in the inverted `CommunityMap`, and sets `nodeCommunityId = Just cid` when found (leaves `Nothing` when not). Must be pure (no IO). Affected areas: `UseCase` only. Risks: O(N) memory for the inverted lookup map — acceptable at 78K (a `Map NodeId CommunityId` of ~78K entries is ~10MB). Check criteria: (a) `cabal build` passes; (b) Hspec test: a graph with 2 nodes in community 4 and 1 isolated node → after join, the 2 have `community_id = Just 4`, the isolated stays `Nothing`; (c) QuickCheck property: `length (filter isJust (map nodeCommunityId (gNodes (joinCommunitiesToNodes g cm)))) == countNodesInCommunities cm`; (d) function has a type signature and no `IO`.
- [ ] 2.D Do: Implement `joinCommunitiesToNodes` + inverted-map builder + tests in `tests/`.
- [ ] 2.C Check: (a) `cabal build` → PASS/FAIL. (b) `cabal test --match "joinCommunities"` → PASS/FAIL. (c) `cabal test --quickcheck-replay=0` for the property → PASS/FAIL. (d) `grep "IO" src/Graphos/UseCase/Cluster.hs | grep joinCommunities` → no hits; PASS/FAIL.
- [ ] 2.A Act: Standardize the inverted-map pattern (useful for the aggregate computation too). If FAIL, record and retry.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Implement `computeCommunityAggregates` UseCase function

- [ ] 3.P Plan: Add `computeCommunityAggregates :: Graph -> CommunityMap -> CohesionMap -> [NodeId] -> Maybe (Map CommunityId Text) -> [CommunityAggregate]` in `src/Graphos/UseCase/Cluster.hs` (or `UseCase.Join.hs`). For each community: `member_count = length members`, `cohesion = cohesionScore g members` (or read from `CohesionMap`), `bridge_count = count members in articulationPoints`, `color = colorForCommunity cid` (lift the palette from `HTML.hs` into a pure Domain/UseCase helper or duplicate it), `label = Map.findWithDefault ("Community " ++ show cid) cid labels`, `representative_labels = take 3 (map nodeLabel members)`, `inter_community_edges = fold edges crossing community boundary grouped by target community`. Pure, O(N+E). Affected: `UseCase`. Risks: color palette currently lives in `HTML.hs` (Infrastructure) — must be lifted to a pure location to avoid UseCase→Infrastructure dependency. Check criteria: (a) `cabal build`; (b) Hspec: a 2-community fixture produces 2 aggregates with correct counts and 1 inter-community edge entry; (c) QuickCheck: `length (computeCommunityAggregates g cm cohom ap labels) == Map.size cm`; (d) no `IO` in the function; (e) no import of `Infrastructure` in `UseCase`.
- [ ] 3.D Do: Lift the color palette to a pure module (e.g. `Domain.Community.Color` or keep a copy in `UseCase`). Implement `computeCommunityAggregates` + tests.
- [ ] 3.C Check: (a) `cabal build` → PASS/FAIL. (b) `cabal test --match "aggregates"` → PASS/FAIL. (c) QuickCheck property → PASS/FAIL. (d) `grep "IO" …` → no hits; PASS/FAIL. (e) `grep "Infrastructure" src/Graphos/UseCase/Cluster.hs` → no hits; PASS/FAIL.
- [ ] 3.A Act: If the palette lift is clean, standardize it as the single source of truth for community colors (HTML.hs imports from it). If FAIL, record and retry.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Wire join + aggregates into Pipeline.hs

- [ ] 4.P Plan: In `src/Graphos/UseCase/Pipeline.hs`, after the re-cluster step (line ~263) and before `epWriteNodes` (line ~269): (1) call `joinCommunitiesToNodes enrichedGraph' finalComm` to get `enrichedGraph''` with community_ids set; (2) after `epWriteGodNodes`, compute `computeCommunityAggregates enrichedGraph'' finalComm finalCohes artPoints mLabels` and write via a new `epWriteCommunityAggregates` port method. Use `enrichedGraph''` (joined) for all downstream export (`exportAll`, Neo4j, Obsidian). Affected: `UseCase.Pipeline`, `UseCase.Port.ExportPort` (add `epWriteCommunityAggregates`), `Infrastructure.Wiring` (implement the port). Risks: the `unsafeCoerce` pattern in `Wiring.hs` for the incremental writer must be extended carefully; the join must not force the full graph into memory prematurely (use `deepseq` at the same boundary as today). Check criteria: (a) `cabal build`; (b) `cabal test` (full suite) passes; (c) a small `cabal run graphos -- tests/fixtures/small` produces `graph.json` with non-null `community_id` on community members and a populated `community_aggregates` key; (d) node/edge/community counts unchanged vs. baseline on the same fixture.
- [ ] 4.D Do: Add `epWriteCommunityAggregates` to `ExportPort`, implement in `Wiring.hs`, add `writeCommunityAggregates` to `IncrementalJSON.hs`, wire the join + aggregate calls into `Pipeline.hs`.
- [ ] 4.C Check: (a) `cabal build` → PASS/FAIL. (b) `cabal test` → PASS/FAIL. (c) Run on small fixture, `python3 -c` parse `graph.json` → `nodes[].community_id` non-null for community members; `community_aggregates` present and non-empty; PASS/FAIL. (d) Compare counts vs. a baseline run (before this change) → identical; PASS/FAIL.
- [ ] 4.A Act: Standardize the "join before write" pipeline invariant. If the `unsafeCoerce` pattern causes issues, document it. If FAIL, record and retry.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Add `writeCommunityAggregates` to IncrementalJSON.hs

- [ ] 5.P Plan: In `src/Graphos/Infrastructure/Export/IncrementalJSON.hs`, add `writeCommunityAggregates :: IncrementalWriter -> [CommunityAggregate] -> IO ()` mirroring `writeGodNodes` (uses `writeKey` + `BSL.hPut` + `encode`). Export it. This task may run in parallel with Task 4's port wiring but the function itself is a prerequisite. Affected: `Infrastructure.Export.IncrementalJSON`. Risks: ordering — must be called after `writeGodNodes` and before `writeAnalysisTail`. Check criteria: (a) `cabal build`; (b) Hspec or manual: an `IncrementalWriter` opened on a temp file, `writeCommunityAggregates` called with a 2-element list, then `closeWriter` → the file contains `"community_aggregates": [...]` with valid JSON; (c) the key name is exactly `community_aggregates`.
- [ ] 5.D Do: Implement `writeCommunityAggregates` + a small test.
- [ ] 5.C Check: (a) `cabal build` → PASS/FAIL. (b) Temp-file round-trip test → PASS/FAIL. (c) `grep "community_aggregates" src/Graphos/Infrastructure/Export/IncrementalJSON.hs` → present; PASS/FAIL.
- [ ] 5.A Act: If the round-trip test pattern is useful, keep it as a reusable IncrementalJSON test helper. If FAIL, record and retry.

### Attempt history (5)

<!-- empty unless a retry is needed -->

## 6. Rewrite HTML.hs viewer as two-phase LOD

- [x] 6.P Plan: Rewrite the embedded JS in `src/Graphos/Infrastructure/Export/HTML.hs` to a two-phase LOD viewer. Phase 1 (overview): dataset = `community_aggregates`, render 1 dot per community, `forceAtlas2Based` + `hideEdgesOnDrag: true`, size ∝ member_count, color = community palette, click → Phase 2. Phase 2 (drill-down): filter inline `nodes` by `community_id == clicked`, render member dots + internal edges + dashed bridge edges to other community dots, "Back to overview" button. Keep the streaming-to-handle write (header → `community_aggregates` JSON → `nodes` JSON → `edges` JSON → JS body). Keep the sidebar (search, communities list, legend). Affected: `Infrastructure.Export.HTML` only (no UseCase/Domain change). Risks: the JS is a large string in Haskell — keep it maintainable; the streaming write order must embed aggregates before nodes so the overview can render before the full node array is parsed (progressive render). Check criteria: (a) `cabal build`; (b) on a small fixture, the generated `graph.html` opens (via `file://`) and shows community dots in overview, click expands a community, "Back" returns; (c) the HTML contains `forceAtlas2Based` and `hideEdgesOnDrag`; (d) the HTML contains `community_aggregates` inline; (e) no `barnesHut` in the generated HTML.
- [x] 6.D Do: Rewrite `htmlHeader`/`htmlBody` + the streaming write in `exportHTML`. Add `community_aggregates` to the inline data block.
- [x] 6.C Check: (a) `cabal build` → PASS (fixed lexical error at line 245, removed unused `communitiesToHTML` function). (b) Open generated `graph.html` on small fixture in a browser → overview renders community dots, drill-down works; (c) `grep "forceAtlas2Based" graph.html` → present; (d) `grep "community_aggregates" graph.html` → present; (e) `grep "barnesHut" graph.html` → absent; (f) Fixed `nodeCommunityId` → `community_id` JSON field name consistency across VisNode and JavaScript code.
- [x] 6.A Act: Standardized the two-phase JS structure. All Check criteria pass.

### Attempt history (6)

<!-- empty unless a retry is needed -->

## 7. 78K-node benchmark on solario

- [ ] 7.P Plan: Run `cabal run graphos -- <solario-path>` (or reuse the existing `../../solario/graphos-out/graph.json` if regeneration is too slow, but it must be re-exported through the new pipeline to get `community_id` + aggregates). Then `graphos serve --dir graphos-out --port 8080` and verify in a browser. This is the Check gate for the whole change. Affected: none (validation only). Risks: regeneration may take > 5 min on 78K nodes (PRD §16.1 target); if so, use the existing graph.json but run a minimal "re-export" path if one exists, or accept the wait. Check criteria: (a) `graph.json` `nodes[].community_id` non-null for all 78,529 nodes; (b) `community_aggregates` has 8,519 entries; (c) initial overview load < 3s; (d) drill-down < 500ms; (e) pan/zoom > 30fps; (f) browser tab memory < 1GB; (g) node/edge/community counts unchanged vs. the baseline `../../solario/graphos-out/graph.json`.
- [ ] 7.D Do: Regenerate or re-export the solario graph; serve; measure.
- [ ] 7.C Check: (a) `python3` parse → 78,529 non-null community_ids; PASS/FAIL. (b) `community_aggregates` length == 8519; PASS/FAIL. (c) DevTools/stopwatch initial load < 3s; PASS/FAIL. (d) Drill-down < 500ms; PASS/FAIL. (e) Pan/zoom > 30fps (DevTools Performance); PASS/FAIL. (f) Tab memory < 1GB (DevTools Memory); PASS/FAIL. (g) Counts vs. baseline; PASS/FAIL.
- [ ] 7.A Act: If all pass, the change is validated — close the PDCA cycle. If latency FAILs at 8.5K community dots, open the next PDCA cycle for Fork A (sigma.js). If memory FAILs, investigate JS DataSet retention. Record findings; if FAIL, retry with targeted fixes.

### Attempt history (7)

<!-- empty unless a retry is needed -->