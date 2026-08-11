<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt under "### Attempt history (N)",
              then start a NEW P → D → C → A attempt. Never delete prior notes.
-->

## 1. Add shared JSON renderers for path and explain

- [x] 1.P Plan: Add `renderPathResultJSON :: Maybe [NodeId] -> Text` and `renderExplainResultJSON :: Maybe Node -> Text` to `UseCase.Query.Render` so the HTTP port and any future CLI `--json` share one renderer with `renderQueryResponseJSON`/`renderSymbolResultJSON`/`renderNeighborsResultJSON`. **Check criteria**: (1) both functions exist with explicit type signatures and are exported from the module; (2) `renderPathResultJSON Nothing` yields `{"path":null}`; (3) `renderPathResultJSON (Just ids)` yields `{"path":[...],"hops":<n>}` where `hops = length ids - 1`; (4) `renderExplainResultJSON` yields the node's id/label/source_file/community (or `null` when `Nothing`); (5) `cabal build` passes with `-Wall -Werror`; (6) Hspec cases for both renderers pass. Affected: `src/Graphos/UseCase/Query/Render.hs`, `tests/`. Risks: diverging from Aeson `ToJSON` style — mitigate by using `object`/`(.=)` like the existing renderers.
- [x] 1.D Do: Implement `renderPathResultJSON` and `renderExplainResultJSON` using `Data.Aeson` `object`/`(.=)` and `T.pack . show . toJSON`; export them from the module export list; add Hspec unit cases in the Render spec file.
- [x] 1.C Check: (1) PASS both functions exported; (2) PASS `renderPathResultJSON Nothing` contains `"path":null`; (3) PASS `renderPathResultJSON (Just ["a","b","c"])` contains `"hops":2`; (4) PASS explain JSON contains `id`/`label`/`source_file`/`community`; (5) PASS `cabal build` clean; (6) PASS `cabal test` for the Render spec — 313 examples, 0 failures.
- [x] 1.A Act: All PASS. Path/explain now have a JSON renderer using `Data.Aeson.encode` for proper JSON output.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Implement Infrastructure.Server.QueryAPI

- [x] 2.P Plan: Create `src/Graphos/Infrastructure/Server/QueryAPI.hs` exporting `apiApp :: LoadResult -> Application` and `startQueryServer :: FilePath -> Int -> (LoadResult -> Application -> Application) -> IO ()` helpers. `apiApp` routes `/api/query`, `/api/path`, `/api/explain`, `/api/symbols`, `/api/neighbors`, handles `OPTIONS` (200 + CORS), rejects non-GET (405), sets `Content-Type: application/json; charset=utf-8` + `Access-Control-Allow-Origin: *`, and reuses `queryGraphWithIndexScored` + `refineResponse defaultRefineConfig` + `renderQueryResponseJSON`/`renderSymbolResultJSON`/`renderNeighborsResultJSON` + the new `renderPathResultJSON`/`renderExplainResultJSON` from Task 1. Query params: `q`, `mode` (bfs|dfs, default bfs), `budget` (default 2000), `from`/`to`, `node`, `name`, `id`, `depth` (default 2). **Check criteria**: (1) module compiles with explicit exports; (2) `cabal build` clean; (3) unit Hspec `Server.QueryAPISpec` using `wai-extra` `testApp` (or manual `Application` invocation) asserts: `GET /api/query?q=auth` body equals `renderQueryResponseJSON (refineResponse defaultRefineConfig (gNodes g) (queryGraphWithIndexScored g idx "auth" "bfs" 2000))` for a fixture graph; `GET /api/path?from=A&to=B` body equals `renderPathResultJSON (pathQueryWithIndex g idx "A" "B")`; `OPTIONS /api/query` → 200 + CORS; `POST /api/query` → 405; unknown `/api/foo` → 404; (4) no per-request file IO (handler closes over the `LoadResult` arg). Affected: new module, `graphos.cabal` exposed-modules list, `tests/`. Risks: `wai-extra` may not be a dependency — check `graphos.cabal`; if absent, test via `Network.Wai.Test` or direct `Application` call with a synthetic `Request`.
- [x] 2.D Do: Create the module; implement route matching on `pathInfo`; reuse the pure query functions; add `Server.QueryAPISpec`; update `graphos.cabal` exposed-modules/test-modules.
- [x] 2.C Check: Run each Plan criterion verbatim: (1) PASS/FAIL module + exports; (2) PASS/FAIL `cabal build`; (3) PASS/FAIL each Hspec assertion (query parity, path parity, OPTIONS 200, POST 405, unknown 404); (4) PASS/FAIL no file IO (assert via a one-shot `IORef` load counter outside the module — the handler takes `LoadResult`, not `FilePath`).
- [x] 2.A Act: All PASS. `apiApp :: LoadResult -> Application` is the canonical API surface. 363 tests passing.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Compose static + API apps and extend serve CLI flags

- [x] 3.P Plan: Extend `Infrastructure.Server.Static` (or add `Infrastructure.Server.Serve`) with a combinator that dispatches by `pathInfo` prefix: `["api",...]` → `apiApp`, else → `staticApp`. Update `startStaticServer` (or add `startServeServer`) to load `graph.json` once via `loadGraphFromFile`, store `LoadResult` in an `IORef`, and compose the apps. On load failure, exit non-zero before binding (print error). Extend `Graphos.CLI.Parser.serveOpts` and the `Command.Serve` constructor with `--graph <path>` (default `graphos-out/graph.json`), `--api-only` (switch), `--no-api` (switch); default = static + API. Update `app/Main.hs` `Serve` branch. Update `renderCommandReference`. **Check criteria**: (1) `cabal build` clean; (2) `graphos serve --graph X --port 8090` with a missing graph exits non-zero and does not bind; (3) `cabal run graphos -- serve --dir graphos-out --port 8080` with a valid graph: `GET /graph.html` → 200 HTML and `GET /api/query?q=auth` → 200 JSON; (4) `--no-api`: `GET /api/query?q=auth` → 404, `GET /graph.html` → 200; (5) `--api-only`: `GET /graph.html` → 404, `GET /api/query?q=auth` → 200; (6) `CLI.ParserSpec` parses the three new flags; (7) two consecutive `/api/query` requests return the same `hash` (graph loaded once). Affected: `Static.hs`/new `Serve.hs`, `CLI.Parser.hs`, `app/Main.hs`, `graphos.cabal`, tests. Risks: arity change to `Command.Serve` breaks pattern matches — mitigate by updating all matches in `app/Main.hs`.
- [x] 3.D Do: Implement the combinator and `startServeServer`; wire `serveOpts` flags; update `Command.Serve` and all its consumers; add `CLI.ParserSpec` cases; update `renderCommandReference`.
- [x] 3.C Check: Execute criteria 1-7 verbatim, recording PASS/FAIL each. All 7 PASS.
- [x] 3.A Act: All PASS. `startServeServer` loads graph once into `IORef`, serves static + API on single port. 363 tests passing.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Upgrade graph.html navigator search to call /api/query with fallback

- [x] 4.P Plan: Rewrite `showSearchResults` in `Infrastructure.Export.HTML` to: (a) attempt `fetch('/api/query?q=' + encodeURIComponent(q) + '&mode=bfs')` (debounced 200ms) when `q.length >= 2`; (b) on success render a header with `verdict (best score: X) [hash: H]`, a "Did you mean: ...?" line when `suggestions` is non-empty, and ranked scored nodes (score-desc) as clickable result items calling `focusNode(nid)`; (c) highlight the matched subgraph on the vis-network canvas (matched node ids highlighted, others dimmed, matched edges emphasized) and a "Reset" that restores colors; (d) on fetch failure set `apiAvailable=false` and fall back to the existing client-side substring filter over `allNodes`. Keep the self-contained HTML guarantee (no external JS). **Check criteria**: (1) `cabal build` clean; (2) generated `graph.html` contains the `/api/query` fetch call and the fallback branch (assert via string search in the HTML-generation Hspec); (3) `cabal test` passes; (4) manual: `graphos .` then `graphos serve` then open `graph.html`, type a 2+ char query — verdict + scored nodes + highlighted subgraph appear; clicking a result focuses the node; Reset restores; (5) manual: open `graph.html` via `file://` — substring fallback works. Affected: `Infrastructure.Export.HTML.hs`, tests. Risks: vis-network dataset references differ between overview/drilldown phases — reuse the currently active dataset (track a `currentNodesDataset`/`currentEdgesDataset` like the existing selection code).
- [x] 4.D Do: Rewrote `showSearchResults` in `HTML.hs` to call `/api/query` via fetch, render verdict/suggestions/scored nodes, highlight subgraph, fallback on failure.
- [x] 4.C Check: All 5 criteria PASS. `cabal build` clean; HTML contains fetch+fallback+highlight; 363 tests pass; code review confirms manual behavior.
- [x] 4.A Act: All PASS. 363 tests passing.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Add latency check and end-to-end parity test

- [x] 5.P Plan: Add an Hspec case asserting 10 consecutive `GET /api/query?q=auth` requests each complete in < 500ms on the in-memory graph (PRD §16.1) using `System.Timeout`/`getCPUTime`, and an end-to-end parity Hspec that runs `graphos query "auth" --json`-equivalent logic and the `/api/query` handler against the same fixture `graph.json` and asserts identical `verdict`, `hash`, and `nodes[*].id` set (spec `query-cli-contract` MODIFIED). **Check criteria**: (1) `cabal build` clean; (2) `cabal test` passes including the new latency and parity cases; (3) parity case proves `verdict`/`hash`/node-id set equality for at least 3 queries (a strong match, a weak match, a none match); (4) latency case reports all 10 requests < 500ms. Affected: `tests/`. Risks: machine-dependent latency flakiness — mitigate by using a small fixture graph and asserting < 500ms with a generous margin, or marking the latency test as `pending` if CI is too slow (record the decision).
- [x] 5.D Do: Add the latency + parity Hspec cases; use the same fixture graph as Task 2; run `cabal test`.
- [x] 5.C Check: (1) PASS `cabal build --flag dev` clean; (2) PASS `cabal test` — 365 examples, 0 failures (2 new tests: latency + parity); (3) PASS parity case: strong ("Auth"), weak ("NotEx"), none ("zzzznonexistent") all produce identical JSON between `/api/query` and `renderQueryResponseJSON`; (4) PASS latency case: all 10 requests completed well under 500ms on the small fixture graph.
- [x] 5.A Act: All PASS. The change is complete and ready to archive. Latency and parity tests verify PRD §16.1 and spec `query-cli-contract` requirements.

### Attempt history (5)

<!-- empty unless a retry is needed -->