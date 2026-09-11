# consumable-graph-outputs

## Why

Source: field feedback from real use (`note/feedback/graphos-feedback.md`, 2026-09-11 — the
SOL-1508 plan.md → plan.json migration: sub-graph extraction, branch comparison, Lean 4
certification). Extraction worked (87K nodes / 150K edges, in-process cypher, `--update`);
**consuming the output outside graphos did not**. The user ended up writing three Python
scripts — a 300-line ijson streaming extractor, a two-graph comparator, and a canvas
generator — to do things graphos half-implements already:

1. **`graphos subgraph` was unusable.** Every invocation the user guessed (`--path`,
   positional patterns, seed lists) failed with "Invalid argument", and `graphos subgraph
   --help` shows the *top-level* help. Root causes, verified in source: the command only
   accepts a JSON `--config` of named subsystems (`CLI/Parser.hs:282`), and its `info` — like
   `cypher`, `push`, `merge`, `ingest`, `serve`, `init` — is registered **without
   `<**> helper`** (`CLI/Parser.hs:290-308`), so no subcommand help exists to say so.
2. **No `graphos diff`.** Comparing main vs branch meant a second full extraction plus a
   Python streamer over 4.3 GB + 1.5 GB. Yet `Graphos.Domain.Graph.Diff.graphDiff` already
   computes added/removed nodes and edges — it has no CLI, no changed-node detection, no
   report.
3. **`cypher` output is TSV-only in practice.** `renderCypherResultJSON` exists and is
   unit-tested (`RenderSpec.hs:54`), and `--json` parses via `commonQueryOptsP` — but the
   success path never calls the JSON renderer, and with no `cypher --help` the flag was
   undiscoverable. The user parsed TSV.
4. **No focused `.canvas` export.** `--obsidian` writes a whole vault (and does generate a
   `graph.canvas` — `Export/Obsidian.hs:33`); there is no way to get a canvas for a
   sub-graph.

The acceptance bar for this change is the feedback's own: **all three workaround scripts
become unnecessary.**

## What Changes

- **`graphos subgraph` becomes usable and documented**: repeatable `--path` patterns, a
  `--seeds FILE` list, stdin seeds, `--hops N` expansion — alongside the existing `--config`
  subsystem mode — emitting a compact contract-shaped sub-graph.json plus optional
  `--graphml` and `--canvas` outputs.
- **New `graphos diff OLD NEW`**: built on the existing Domain `graphDiff`, extended with
  changed-symbol detection; per-file rollup of added/removed/changed symbols; `--scope`
  glob; markdown report to stdout, `--json` for structured output, optional `--canvas`.
- **`cypher` honors `--json`** (wiring the existing renderer into the success path) and
  gains `--format json|csv|tsv`; single document on stdout, errors on stderr.
- **Every subcommand gets `--help`**: `<**> helper` on all `info` registrations, with usage
  examples in each `progDesc`/footer.
- **BREAKING**: none. `subgraph --config` mode, default TSV cypher output, and all existing
  flags keep working.

## Capabilities

### New Capabilities

- `subgraph-extraction-cli`: seed-driven sub-graph extraction (paths, seed files, stdin,
  hops) with compact contract-shaped output and optional graphml/canvas artifacts.
- `graph-diff-cli`: two-graph comparison with per-file symbol rollups, scope filtering, and
  markdown/JSON/canvas reports.
- `canvas-export`: focused Obsidian `.canvas` generation from sub-graph or diff results —
  file-level nodes colored by category, dependency edges, deterministic layout.
- `cli-subcommand-help`: every subcommand answers `--help` with its own flags and at least
  one usage example.

### Modified Capabilities

- `cypher-query`: adds a structured-output requirement — `--json` honored on the success
  path, `--format json|csv|tsv`, stdout/stderr discipline per `query-cli-contract`.

## Impact

- **Code**: `CLI/Parser.hs` (subgraph/diff opts, `<**> helper` everywhere),
  `app/Main.hs` (diff command, cypher render dispatch), `Domain/Graph/Diff.hs` (changed-node
  detection via id-intersection), new `UseCase/Diff.hs` (rollups, scope), new
  `Infrastructure/Export/Canvas.hs` (extracted from Obsidian's canvas writer),
  `UseCase/Query/Render.hs` (csv renderer).
- **APIs**: none — CLI only.
- **Dependencies**: none new.
- **Out of scope** (deferred with rationale in design.md): a binary/indexed on-disk graph
  format (feedback priority 3). Seed-driven `subgraph` makes small consumable JSON one
  command away, and the serving path is `progressive-graph-interface`'s slice API; revisit
  only if 4 GB pain persists after both land.
