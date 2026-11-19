# Verification notes — usable-graph-for-code-review (tasks 6.1, 6.2)

Measured 2026-11-18. Binary: branch `feature/usable-graph-for-code-review-close`
(`main` `f8fe023` + this change's close-out) merged locally with PR #264
(`fix/bunch-real-projects-validation`: atomic extraction merge, `./` import targets), so
that extraction is deterministic. "Before" figures are the 2026-10-08 bug report
(`note/personal/graphos/bug-report-2026-10-08-solario-core-pr1357.md`, binary `87b4f1a`) and,
for this repository, the 2026-09-29 build kept in `graphos-out.staging-20260929T101551287007131000-318660/`.

## 6.1 — builds, sizes, `symbols` wall time

| Corpus | Command | Before | After |
|---|---|---|---|
| `example/ts-lsp-test` | `graphos . --fresh --no-viz --no-observability` | 111 nodes; `symbols dbGetById` → "No symbol found" | 70 nodes, 76 edges (61 `contains`, 11 `imports`, 4 `depends_on`), `graph.json` 61,535 B, longest id 45 chars; `symbols dbGetById` → `15372_db_Function:dbGetById` in 20 ms, `symbols service.ts` → the module in 32 ms |
| this repository | `graphos . --fresh --no-viz --no-cluster` | 17,021 nodes, 417,939 edges, 116.6 MB (clustered build with inferred edges — not like for like); `symbols resolveNodeArg` 4.9 s | 21,820 nodes, 25,449 edges, 16.9 MB, built in 6 s; `symbols resolveNodeArg` 0.74 s; build drops: 24 unknown endpoints, 9 collapsed pairs |

`cabal build --flag dev all --enable-tests` and the full `cabal test` (1,724 examples, 0
failures, no skips) are recorded in `tasks.md`.

## 6.2 — bug-report commands on solario-core (`worktrees/solario-core-pr1357`, `defb4024a`)

Same worktree, `.graphosignore` and project `graphos.yaml` (`granularity: file`) as the report.

Build as in the report — `graphos . --directed --no-viz --no-cluster --fresh --no-observability`:
11,530 nodes, 8,848 edges, `graph.json` 12.0 MB. The project's `granularity: file` is now
honoured (B6), so every code file is one node and **no `imports` edges are emitted at file
granularity** (the requirement "File-level extraction emits one node per file … and no
children" says nothing about imports; a file-level import graph is a follow-up).

For the read commands the build was repeated with `--granularity function` (the CLI level wins,
task 2.1): 89,543 nodes, 98,744 edges, `graph.json` 84.7 MB, 33 s. The report's 129,839 nodes
are not reached because the fixes themselves remove nodes (no source-text duplicates,
suppressed wrappers); it is the same repository the task names.

| # | Report (2026-10-08) | Now |
|---|---|---|
| B1 | ids embed source text; `graph.json` 2.77 GB; `0_external_## Examples by kind …` nodes | ids `<dirHash>_<stem>_<kind>:<identifier>` (longest 260 chars, p99 112 — relative import specifiers), 84.7 MB, no `## Examples` node |
| B2 | `symbols createManagementWorkerEnvironment` → "No symbol found" (40.8 s) | `8033_management-worker-environment_Function:createManagementWorkerEnvironment`, score 1.0, 4.1 s; `symbols workflow-task-worker.ts` → `59942_workflow-task-worker_Module:workflow-task-worker.ts`, 4.2 s. `query "workflow-task-worker"` still answers `Verdict: none` (tokenized query is outside this change) |
| B3 | every `WHERE` → no rows; `MATCH p = …` → positional parse error | `source_file =`, `source_file CONTAINS`, `label =` each return the worker's nodes at the default budget; `MATCH p = …` → "named path variables are not supported: use `graphos path <start> <end>` for reachability" |
| B4 | `path` worker → `executeSharedRunCommand`: no path; imports end on stems | the report's chain `workflow-task-worker.ts → local-task-tool-session-factory.ts → shared-tool-executor.ts → shared-run-command.ts` exists as `imports` edges; `graphos path workflow-task-worker.ts shared-run-command.ts` answers **yes** (2 hops, the shortest over all relations: `imports` to `result.ts`, then a `references` edge) |
| B5 | `explain <id>` answers on an unrelated `CodeFile` | `explain executeSharedRunCommand` → `15996_shared-run-command_Function:executeSharedRunCommand`, `shared-run-command.ts` L357–559 |
| B6 | `granularity: file` ignored (129,839 nodes) | honoured: 11,530 nodes, one per file |
| B7 | `traces/trace_1.jsonl` written in the worktree despite `--no-observability` | nothing written in the worktree, with or without the flag; without it, traces go to `<out>/traces` |
| B8 | `GRAPH_REPORT.md` 29.6 MB, 18,894 articulation rows | 7,051 B; bridge table capped at 50 rows, "6949 articulation points, 6899 omitted" |
| B9 | ~40 s and several GB per read command | 2–4 s per command on 84.7 MB; the command reference and generated skills point repeated queries to `graphos serve` |
| B10 | 151,689 → 136,340 edges, unexplained; `symbols --help` read as a name | build log: "Extracted 91192 nodes, 100865 edges" → "Graph: 89543 nodes, 98744 edges", "Build drops: 329 edges with unknown endpoints, 1784 collapsed duplicate (source,target) keys"; `symbols --help` prints usage |

Review question — "does `workflow-task-worker.ts` reach `shared-run-command.ts`?": answered by
`graphos path workflow-task-worker.ts shared-run-command.ts` (yes), and the full import chain
is present.

Observations outside this change: `--no-observability` also silences INFO logging (the B10 line
needs a run without it); `query` is still tokenized and misses hyphenated file stems.
