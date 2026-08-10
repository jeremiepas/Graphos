## Why

Community labels produced by `--label` (LLM labeling, PRD §5, `llm-labeling` spec) never reach the `graph.html` viewer. Two breaks in the pipeline cause this:

1. **HTML export signature drops labels.** `ExportPort.epExportHTML :: Graph -> Analysis -> FilePath -> IO ()` (ExportPort.hs:31) has no labels parameter, and `UseCase.Export.exportAll` (Export.hs:37) calls `epExportHTML ep g analysis hPath` without passing the `mLabels` it receives. Inside `Infrastructure/Export/HTML.hs`, `communityAggregatesToJSON` (line 843) hardcodes `vcaLabel = T.pack ("Community " ++ show cid)` regardless of any LLM work. So even the full `graphos . --label` run writes `community_labels` to `graph.json` (IncrementalJSON.hs:103) but the HTML viewer — which embeds its own copy of the data, not `graph.json` — shows only `Community <id>`.
2. **`graphos ingest` has no labeling path.** `ingestOpts` (Parser.hs:173) only exposes `FILE`, `--embed`, `--output` — no `--label`. `runSingleFilePipeline` (Incremental.hs:189-197) passes `Nothing` for labels to `epExportAll` and never calls `labelCommunities`. So a user who runs `graphos ingest <file> --label` (the user's scenario) gets no labeling at all, and even the `graph.json` lacks `community_labels`.

The user-visible symptom: after `graphos ingest <file> --label`, the frontend shows `Community 3` instead of the LLM-generated label (e.g., `Authentication Module`).

## What Changes

- **BREAKING (port signature)**: Add a labels parameter to `epExportHTML` and `exportHTML`: `epExportHTML :: Graph -> Analysis -> Maybe (Map CommunityId Text) -> FilePath -> IO ()`. Wire it in `Wiring.hs` and `UseCase.Export.exportAll` (pass `mLabels` through). All call sites updated.
- Modify `communityAggregatesToJSON` in `HTML.hs` to accept a `Maybe (Map CommunityId Text)` and use the LLM label (falling back to `"Community <id>"` when absent or when the community id is not in the map) for `vcaLabel`. Also surface the label in the overview dot `title` and the sidebar community list.
- Add a `--label` flag to `graphos ingest` (`ingestOpts` in Parser.hs; `IngestCmd` constructor in the `Command` type). When set, `runSingleFilePipeline` invokes `labelCommunities` after clustering and passes the resulting labels to `epExportAll` (so both `graph.json` and `graph.html` carry them). The flag mirrors the existing `--label` on the full pipeline (Parser.hs:56).
- No change to the `llm-labeling` config/defaults (already correct), no change to `graph.json` schema (the `community_labels` key already exists), no change to the HTML viewer JS structure (it already reads `c.label` from the embedded aggregates — the fix is in the Haskell that *produces* `c.label`).

## Capabilities

### New Capabilities
<!-- None — this is a fix to existing capabilities. -->

### Modified Capabilities
- `llm-labeling`: The `--label` flag SHALL also be available on `graphos ingest`, and the resulting labels SHALL flow to both `graph.json` and `graph.html` (previously only the full pipeline produced labels, and only to `graph.json`).
- `html-lod-viewer`: The community-dot overview and the sidebar community list SHALL display LLM-generated community labels (from the `community_labels` passed to `exportHTML`) instead of the hardcoded `"Community <id>"` fallback, falling back to `"Community <id>"` only when no label is available for that community. (Note: `html-lod-viewer` is currently in the unarchived `refactor-html-large-graph-lod` change; this delta targets the requirement that the overview renders community dots with labels.)

## Impact

**Code**:
- `src/Graphos/UseCase/Port/ExportPort.hs` — add `Maybe (Map CommunityId Text)` param to `epExportHTML`.
- `src/Graphos/Infrastructure/Export/HTML.hs` — `exportHTML` and `communityAggregatesToJSON` accept labels; use them for `vcaLabel` and the dot `title`.
- `src/Graphos/Infrastructure/Wiring.hs` — update the `epExportHTML` wiring to pass labels through.
- `src/Graphos/UseCase/Export.hs` — `exportAll` passes `mLabels` to `epExportHTML`.
- `src/Graphos/CLI/Parser.hs` — add `--label` to `ingestOpts`; extend `IngestCmd` constructor.
- `src/Graphos/Domain/Types.hs` (or wherever `Command`/`IngestCmd` lives) — extend the `IngestCmd` variant with a `Bool` (or reuse `cfgLabel`).
- `src/Graphos/UseCase/Pipeline/Incremental.hs` — `runSingleFilePipeline` invokes `labelCommunities` when `cfgLabel` is set, passes labels to `epExportAll`.
- `app/Main.hs` — `IngestCmd` handler threads the `--label` flag into `cfgLabel`.

**APIs/Dependencies**: No new Haskell or JS dependencies. The `ExportPort` signature change is internal (Haskell record-of-functions). `graph.json` schema unchanged (the `community_labels` key already exists). No downstream consumer breaks because the HTML is self-contained.

**Systems**: `graphos serve` (Static.hs) unchanged — it serves the regenerated `graph.html`. No config change; `--label` is a CLI flag, and labeling config (`gcLabeling`) is already loaded from `graphos.yaml`.

**Tests**: New Hspec tests for (a) `communityAggregatesToJSON` using a label when present and falling back when absent; (b) `exportAll` passing `mLabels` to `epExportHTML` (use a stub `ExportPort` that records the argument). The `--label` ingest path is verified by a unit test on `runSingleFilePipeline` with a stub `LLMPort` returning canned labels.

## PDCA Cycle

- **Plan**: Hypothesis — threading the existing `mLabels` from `exportAll` into `exportHTML`, and adding `--label` to `graphos ingest`, makes the frontend show LLM labels instead of `Community <id>`. Success measured against PRD §12 (HTML export) and the `llm-labeling` spec: (1) `graphos . --label` produces a `graph.html` whose embedded `_communityAggregatesData` contains the LLM labels in `label`; (2) `graphos ingest <file> --label` does the same and writes `community_labels` to `graph.json`; (3) no regression when labels are absent (fallback to `Community <id>`); (4) `cabal build` zero warnings, `cabal test` green.
- **Do**: Extend the `epExportHTML` signature, thread labels through `exportAll` and `runSingleFilePipeline`, use labels in `communityAggregatesToJSON`, add `--label` to `ingestOpts` and the `IngestCmd` handler.
- **Check**: Run `cabal run graphos -- example/ --label` (with a working LLM endpoint) and `cabal run graphos -- ingest <file> --label`; open the resulting `graph.html` files and verify the sidebar community list and overview dot tooltips show the LLM labels, not `Community <id>`. Run `cabal test` for the new unit tests. Run `cabal build` for zero warnings.
- **Act**: If the LLM endpoint is unavailable in CI, the unit tests use a stub `LLMPort` so they run offline. If labels are present for some communities but not all (partial labeling), the fallback handles it — document the partial-label behavior in the `llm-labeling` spec. If the `ExportPort` signature change ripples further than expected, split the change into a port-shape refactor + the label fix.