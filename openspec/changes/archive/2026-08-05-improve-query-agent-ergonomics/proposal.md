# Proposal: improve-query-agent-ergonomics

## Why

A field postmortem (2026-08-04, `solario-core`, 78k-node graph) showed that an AI agent
using `graphos query` burned ~40 tool calls and **never answered** the question it was
asked. The failure was not graph quality alone — the query-family CLI (`query`, `path`,
`explain`; PRD §13.1) actively misled the caller:

1. **Silent degenerate fallback**: four unrelated queries returned a byte-identical,
   irrelevant node set formatted exactly like a genuine hit. No score, no "weak match"
   signal. The agent could not distinguish good answers from junk, so it looped.
2. **Output ordering/truncation works against the reader**: results are emitted in
   edge-map order (effectively alphabetical), so external truncation destroys the
   highest-value content first. Raising `--budget` made results worse.
3. **Noise drowns signal**: 200–400-char raw-source labels, `contains` edges to trivia
   nodes (`undefined`, `Promise`, `Result`), and near-duplicate symbol nodes consume the
   budget of dozens of useful lines.
4. **No machine-readable output, inconsistent flags**: `--budget` works on `query` but
   hard-errors on `explain`; no `--json` anywhere in the query family, forcing fragile
   text-scraping.
5. **No exact-lookup or path-scoped entry points**: agents must go through fuzzy scoring
   even when they know the exact identifier or directory.

Graphos exists to *save* agent tokens (PRD §1, §7). A query surface that cannot signal
its own failure does the opposite. This change makes failure legible, output
relevance-ordered and noise-free, and the CLI contract uniform and machine-readable.

## What Changes

- `graphos query` prints a match verdict header (`strong | weak | none` with best score),
  per-result scores, and results in **descending relevance order**. Weak/no-match queries
  emit did-you-mean suggestions from the graph's own vocabulary instead of a fallback
  node set. A result-set hash lets callers detect "no new information".
- Output noise controls: `--edges semantic|all` (default `semantic`, drops structural
  edges to trivia targets), `--label-width N` (default 120, elides long labels but keeps
  node ids), self-referential edge collapse, and duplicate-declaration deduplication at
  render time.
- Query semantics: multi-term queries degrade gracefully (OR scoring surfaced in the
  verdict, never silent zero), and a `--path <glob>` filter restricts results by source
  file; source paths become searchable.
- New subcommand `graphos symbols <name>`: exact identifier lookup bypassing fuzzy
  scoring (workflow doc 14).
- New subcommand `graphos neighbors <node-id> [--depth N] [--edges semantic|all]`:
  cheap foothold expansion from a known node (workflow doc 15).
- Uniform flag surface: `--json` and `--budget` accepted by `query`, `path`, `explain`,
  `symbols`, `neighbors`; `--help` works on every subcommand.
- Budget-aware tail truncation inside the tool: the head (highest-ranked) is always kept.

## Capabilities

### New Capabilities
- `query-legibility`: match verdict, per-result scores, did-you-mean on weak/no match,
  result-set hash, relevance-ordered budget-aware output for `graphos query`
  (workflow doc 04-query).
- `query-noise-control`: semantic edge filtering, label elision, self-edge collapse,
  duplicate-symbol deduplication across the query family (workflow docs 04-query,
  05-path, 06-explain).
- `query-scoping`: `--path <glob>` source-path filtering and path-indexed search for
  `graphos query` (workflow doc 04-query).
- `symbol-lookup`: `graphos symbols <name>` exact identifier lookup (new workflow
  doc 14-symbols).
- `neighbor-expansion`: `graphos neighbors <node-id>` depth-bounded neighborhood dump
  (new workflow doc 15-neighbors).
- `query-cli-contract`: uniform `--json` / `--budget` / `--help` across the query-family
  subcommands (workflow docs 04-query, 05-path, 06-explain).

### Modified Capabilities
<!-- none — no existing spec in openspec/specs covers the query CLI -->

## Impact

- **Code**: `app/Main.hs` (CLI parsers, renderers), `src/Graphos/UseCase/Query.hs`
  (scored results, verdict), `src/Graphos/Domain/Graph/Index.hs` (path index,
  did-you-meaning vocabulary, exact-symbol lookup), new pure render/JSON module(s) in
  UseCase; Hspec coverage in `tests/Graphos/UseCase/QuerySpec.hs` and new spec files.
- **Architecture**: all new logic is pure (Domain/UseCase); only stdout rendering stays
  in `app/Main.hs` — preserves the zero-IO rule (PRD §4.2).
- **APIs**: CLI output format of `query`/`explain`/`path` changes (verdict header,
  ordering, elided labels). MCP `query_graph` tool (PRD §8.2) can adopt the same scored
  result type later; not changed in this iteration.
- **Docs**: PRD §13 CLI reference and workflow docs 04/05/06 updated; new docs 14, 15.
- **Dependencies**: none new (aeson, optparse-applicative already present).

## PDCA Cycle

- **Plan**: Hypothesis — an agent can reach a correct verdict-or-bailout in ≤ 5 query
  calls when failure is legible. Success measured against PRD §16.1: query response
  stays < 500 ms on a 100k-node graph, and against acceptance scenarios: (a) a
  nonsense query returns verdict `none` + suggestions and zero fabricated results;
  (b) repeated identical queries return an identical result-set hash; (c) top-ranked
  result appears first and survives any truncation.
- **Do**: Implement scored query results, verdict thresholds, did-you-mean, noise
  filters, `symbols`/`neighbors` subcommands, and the uniform `--json`/`--budget` flag
  surface per design.md and tasks.md.
- **Check**: Hspec unit tests for scoring/verdict/dedup/path-filter (pure, PRD §15.3);
  golden tests for text and JSON renderings; manual re-run of the postmortem's failing
  queries (rows 3–7) asserting verdict `none`/`weak` instead of fabricated hits;
  benchmark query latency against the < 500 ms budget (PRD §16.1).
- **Act**: Fold verdict thresholds and edge-noise lists into config defaults if field
  use confirms them; feed the scored-result type into the MCP `query_graph` tool as the
  next PDCA cycle; update the graphos agent skill protocol to consume verdicts and
  result-set hashes.
