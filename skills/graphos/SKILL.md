---
name: graphos
description: Build and query Graphos code knowledge graphs (graph.json) — choosing build flags, reusing existing builds, cypher/query/symbols/neighbors/path usage and their pitfalls, reading graph.json directly for exact counts and file-level import edges, and pushing to Neo4j. Use when the user mentions graphos, graph.json, graphos-out, a code/dependency/import graph, file in/out degrees, a dependency subgraph, or asks to explore a codebase "with graphos".
---

# Graphos

[Graphos](https://github.com/jeremiepas/Graphos) turns a source tree (code, docs, specs) into a knowledge graph using language servers, and writes it to `<output>/graph.json` (default `graphos-out/`). This skill covers building that graph and getting answers from it that you can trust.

## Setup check

```bash
command -v graphos && graphos lservers    # there is no --version flag
```

If `graphos` is missing, run `install.sh` from this skill's directory (`~/.claude/skills/graphos/install.sh`). It downloads the release binary, verifies its sha256, adapts it to the distro, and installs the shared global config. Don't overwrite an existing `~/.config/graphos/graphos.yaml` without asking the user (`--force-config` backs it up first).

Configuration is layered, later wins: built-in defaults → `~/.config/graphos/graphos.yaml` → project `graphos.yaml` → CLI flags. **Before creating or editing any `graphos.yaml` or `.graphosignore`, read [references/config.md](references/config.md)**: unknown keys are silently ignored, and the `neo4j`/`labeling`/`embedding`/`vision`/`observability` sections only accept camelCase keys. Templates are in `config/`: `graphos.global.yaml`, `graphos.project.yaml` and `graphosignore.example`.

Check for a project `graphos.yaml` before building: it may set `granularity: file`, which drops symbol nodes. Pass `--granularity function` when you need symbols.

Startup lines such as `[memory] budget…` and `[graphos] Re-executing with RTS flags…` are normal: graphos sizes its own heap. Override with `--max-heap 8G` or `--no-memory-budget`.

## 1. Reuse before rebuilding

A build reads every file and can take minutes on large repos. Look for an existing `graph.json` first and check which commit it was built from (folder name, `build-info.json` if present, or the file's mtime against `git log`). Rebuild only when the code you care about changed since.

## 2. Build

Recommended flags for analysis (no HTML view, no clustering, clean extraction):

```bash
graphos <path> -o <out-dir> --directed --no-cluster --no-viz --fresh \
  --no-semantic-edges --no-observability --granularity function
```

| Flag | Why |
|---|---|
| `--directed` | keeps edge direction (who imports whom) |
| `--no-cluster` | community detection is slow on 100k+ nodes and not needed for dependency questions |
| `--no-viz` | skips the HTML visualisation |
| `--fresh` (= `--no-checkpoint`) | ignores a stale checkpoint from an earlier build |
| `--no-semantic-edges` | only literal code↔doc links, no inferred ones |
| `--update` | incremental refresh instead of a full build |

- Keep the output directory **outside** the repository being analysed (or git-ignored). graph.json can reach several GB on a large monorepo, plus a checkpoint of similar size.
- To analyse a single commit or branch, build from a detached `git worktree` and remove it afterwards.
- To analyse a curated subset, copy those files into a staging directory keeping their relative paths (plus `graphos.yaml` / `.graphosignore`) and build that.
- Hidden directories and anything in `.graphosignore` are not ingested. Stage a copy if you need them.

## 3. Query

Each query command **reloads the whole graph.json**, which takes seconds to tens of seconds on a big graph, so batch your questions. Always pass an absolute `--graph` path.

| Need | Command |
|---|---|
| Free-text search | `graphos query "<question>" --graph $G` |
| Exact symbol | `graphos symbols <Name> --graph $G [--path 'src/x/**']` |
| Neighbourhood of a node | `graphos neighbors <node> --depth 2 --graph $G` |
| Shortest path | `graphos path <from> <to> --graph $G` |
| Detail on a node | `graphos explain <node> --graph $G` |
| openCypher | `graphos cypher "<query>" --graph $G --budget 5000000 --max-nodes 0 [--json]` |
| Multi-term research view | `graphos research <term>... --graph $G --json` |
| Subgraph extraction | `graphos subgraph --graph $G --config <cfg> -o <file>` |
| OpenSpec checks | `graphos speccheck --specs <openspec-root> --json` |
| Spec / change gates | `graphos intentcheck --specs specs.json --graph $G` · `graphos impact --before a.json --after b.json --gate` |

Add `--json` whenever the result feeds another step.

### Pitfalls

1. **`--budget` limits how many nodes are scanned, not just how much is printed.** With the default budget, a whole-graph query only looks at the first few thousand nodes and quietly returns too little. A count that comes back as a suspiciously round number (e.g. exactly 2000) is the sign. For whole-graph queries pass `--budget 5000000 --max-nodes 0`, or narrow the scope with `--path GLOB`.
2. **In cypher, `kind` is a node label**: write `MATCH (n:Module)`, not `WHERE n.kind = 'Module'`.
3. **Cypher grammar is partial**: flat `OR` chains work, but `(a OR b) AND (c OR d)` fails with "unexpected keyword: AND". Split the query or filter afterwards.
4. **`neighbors`/`path`/`explain` take a node id or display name, not a file path.** Find the id first with `symbols` or cypher. Ids look like `<n>_<stem>_<symbol>`, where the number groups nodes by directory.
5. **A node's `label` is often the full definition text** (`parseFoo(x: string): Result<…>`), not the bare name, so `WHERE n.label = 'parseFoo'` finds nothing. Use `symbols`, or `STARTS WITH` / `CONTAINS`.
6. **`source_file` varies by node type**: `./src/…` on definitions, bare `src/…` on `External` (reference) nodes, and sometimes code text on inferred nodes. Filter on a prefix (e.g. `STARTS WITH './src'`) to keep only real files.
7. **Markdown files in one directory that share a name prefix before the first dot can merge into a single node.** List such files from the filesystem and use the graph only for their edges.
8. **What graphos can't see**: dynamic imports (`await import(…)`), references built from strings or file paths, reflection/DI wiring, and anything outside the ingested corpus. Check with grep before calling a file unused or dead.
9. **`speccheck` coverage**: when it reports `spec-relation edges: 0`, every requirement shows as unimplemented. Treat that list as a checklist, not as evidence of gaps.

## 4. Exact numbers: read graph.json directly

For anything quantitative (counts, in/out degrees, full edge lists), read the file instead of relying on CLI output limited by the budget. Current format:

```json
{"graph": {
  "nodes": { "<id>": {"id": "...", "label": "...", "metadata": {"kind": "Module", "source_file": "./src/a.ts", "file_type": "code"}} },
  "edges": [ {"source": "<id>", "target": "<id>", "relation": "imports", "directed": true,
              "metadata": {"confidence": 1.0, "weight": 1, "id": "..."}} ]
}}
```

`nodes` is an **object keyed by id**, not an array. Older builds use a legacy layout: top-level `schema_version: "1"`, `nodes`/`edges` as arrays, properties directly on each node. Check with `head -c 200 graph.json`, and upgrade with `graphos migrate-graph <graph.json>` (in place unless `--output`).

**File-to-file import edges**: for each edge with `relation == "imports"`, the source is a `Module` node (its `source_file` is the importing file) and the target is an `External` node whose `metadata.source_file` is either the resolved imported path or `external:<package>` for third-party imports. Strip the leading `./`, drop `external:` targets, and deduplicate. `contains` edges only link a file to its own symbols.

```bash
jq -r '.graph as $g | $g.edges[] | select(.relation=="imports")
  | [$g.nodes[.source].metadata.source_file, $g.nodes[.target].metadata.source_file] | @tsv' "$G" \
  | sed 's#^\./##; s#\t\./#\t#' \
  | awk -F'\t' '$1!="" && $2!="" && $2 !~ /^external:/' | sort -u > file-edges.tsv
```

Import targets are not always resolved to a file: they can come back without an extension (`src/a` for `./a`) or as a directory (`index.ts` imports). Map each target to an existing file (try `.ts`, `.tsx`, `.js`, `/index.ts`…) before computing degrees.

From there, a file's out-degree is the number of lines where it appears in column 1, and its in-degree the number of lines where it appears in column 2. On multi-GB graphs, a script that loads the JSON once is faster than repeated CLI calls.

## 5. Reporting

Back every claim with the graph: cite `file:line`, give `in=`/`out=` degrees and node/edge counts, and say which commit and build the graph came from. Next to any "unused" or "nothing depends on this" conclusion, state the blind spots from pitfall 8.

## Neo4j

Read [references/neo4j.md](references/neo4j.md) before pushing a graph to Neo4j: the built-in push only runs under certain flags, scales badly on large graphs, and can create duplicates.
