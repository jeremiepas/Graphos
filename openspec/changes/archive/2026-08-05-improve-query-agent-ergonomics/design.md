# Design: improve-query-agent-ergonomics

## Context

The query-family CLI (`query`, `path`, `explain`) is the primary agent interface to a
built graph. Current state:

| Component | Location | Current behavior |
|---|---|---|
| CLI parsers | `app/Main.hs` (`queryOpts`, `pathOpts`, `explain` inline) | `--budget`/`--graph` only on `query`; no `--json` anywhere; `explain` rejects `--budget` |
| Term matching | `Graphos.Domain.Graph.Index.findMatchingNodes` | OR-count scoring over inverted label index; returns `[(NodeId, Int)]`; score never surfaced to output |
| Query traversal | `Graphos.UseCase.Query.queryGraphWithIndex` | takes top-5 matches, BFS depth 3 (or DFS 6), returns `QueryResult` with unscored node/edge lists; `_budget` parameter is **ignored** |
| Rendering | `app/Main.hs` `QueryCmd`/`PathCmd`/`ExplainCmd` branches | raw `putStrLn` of full labels in `Map.toList` (key-sorted) order; no scores, no verdict, no truncation |
| Index | `GraphIndex` | label tokens + full labels only; source paths and node kinds not indexed |

Postmortem findings map to these gaps: match scores exist internally but are discarded
before output; BFS expansion from a weak top match *is* the "degenerate fallback" (the
subgraph around an irrelevant node looks identical to a real hit); ordering is map-key
order so external truncation removes the best content; labels are raw source text.

## Goals / Non-Goals

**Goals:**
- Every query response self-reports its quality: verdict (`strong|weak|none`), per-result
  score, result-set hash.
- No fabricated results: below-threshold matches produce suggestions, not a subgraph.
- Relevance-descending output with in-tool tail truncation honoring `--budget`.
- Semantic-noise controls: edge filtering, label elision, self-edge collapse, duplicate
  symbol dedup — all pure and shared across `query`/`path`/`explain`/`neighbors`.
- Path-scoped search (`--path <glob>`) backed by a source-path index.
- Exact lookup (`symbols`) and foothold expansion (`neighbors`) subcommands.
- Uniform CLI contract: `--json`, `--budget`, `--graph`, `--help` on all five
  query-family subcommands.
- Query latency stays < 500 ms on 100k-node graphs (PRD §16.1).

**Non-Goals:**
- No changes to graph build/extraction (granularity, LLM labeling, test/spec exclusion —
  postmortem §5 is a separate, config-side concern).
- No MCP server changes in this cycle (the scored result type is designed to be reusable
  there next cycle).
- No embedding/semantic-vector search; scoring stays lexical over the inverted index.
- No breaking change to `graph.json` schema.

## Decisions

### D1 — Scored results as a Domain type, verdict in UseCase

New pure types carry scoring end-to-end instead of discarding it at the UseCase boundary.

| Type | Layer | Fields (schema) |
|---|---|---|
| `MatchVerdict` | Domain | `Strong \| Weak \| NoMatch` |
| `ScoredNode` | Domain | node id, label, score (Double, normalized 0–1), source path, community |
| `QueryResponse` | UseCase | verdict, best score, result-set hash, ranked `[ScoredNode]`, filtered edges, suggestions `[Text]` |

Verdict thresholds: normalized best score ≥ 0.5 → `Strong`; > 0 but < 0.5 → `Weak`;
0 → `NoMatch`. Normalization = matched-term count ÷ query-term count, boosted by exact
full-label hit. Thresholds live as named constants in Domain with the rationale documented.

- *Alternatives considered*: (a) keep `QueryResult` and bolt scores onto the renderer —
  rejected: `path`/`explain`/MCP could not reuse it, and the fallback bug would persist
  internally; (b) TF-IDF weighting — rejected for this cycle: more tuning surface, not
  needed to make failure legible; revisit in Act.
- *Layering*: types in Domain (pure data), assembly in UseCase, rendering in
  `app/Main.hs` — Infrastructure remains untouched, honoring the zero-IO rule.

### D2 — No-fallback + did-you-mean from index vocabulary

When verdict is `NoMatch` (and for `Weak`, alongside results), the response carries up to
10 suggestions: nearest tokens from `giLabelIndex` keys by restricted Damerau-Levenshtein
distance (bounded ≤ 2) plus shared-prefix ranking. BFS expansion is **skipped entirely**
for `NoMatch` — this deletes the degenerate-fallback path rather than papering over it.

- *Alternatives*: (a) always expand but flag weakly — rejected: agents demonstrably
  cannot resist plausible-looking output; (b) n-gram similarity index — rejected: extra
  memory on 500k-node graphs for marginal gain over edit distance on the existing
  token set.

### D3 — Result-set hash for loop detection

`QueryResponse` includes a short hex hash (FNV-1a or SHA256-truncated) over the ordered
list of result node ids. Printed in the header (`results: 14 [hash a3f29c01]`) and in
JSON. Identical query → identical hash → caller detects "no new information".

- *Alternatives*: query-level caching with "seen before" warnings — rejected: requires
  state across invocations; a hash keeps the CLI stateless and lets the caller decide.

### D4 — Relevance ordering + in-tool tail truncation

Nodes are emitted in descending score; edges are emitted grouped under their
highest-ranked endpoint. The renderer accounts a token estimate per line (chars ÷ 4)
against `--budget` and stops emitting at the limit with a trailing
`… truncated: N more nodes, M more edges (raise --budget)` footer. Head is never dropped.

- *Alternatives*: rely on the shell/agent to truncate — rejected: that is exactly the
  postmortem failure mode 2.2; the tool must own its budget.

### D5 — Noise controls as one pure post-processing pass

A new pure UseCase module (`Graphos.UseCase.Query.Refine`) applies, in order:

| Step | Rule | Default |
|---|---|---|
| Edge-class filter | `--edges semantic` drops `contains` edges whose target label is in the trivia set (`undefined`, `unknown`, `null`, primitive/`Promise`/`Result` wrappers, single-token type parameters) or whose target is a leaf with degree 1 and label length > 200 | `semantic` |
| Self-edge collapse | drop edges where source id == target id | always |
| Duplicate-symbol dedup | group nodes whose labels differ only by declaration-prefix (`export const X…` / `const X…` / `X`) and identical source file+line; keep the shortest label, merge edges | always |
| Label elision | `--label-width N`: labels longer than N are elided at a word boundary with `…` ; node id always printed in full | 120 |

- *Alternatives*: fix duplicates at build time — correct long-term but requires re-running
  extraction on existing graphs; render-time dedup works on already-built graphs
  immediately. Build-time dedup is recorded as an Act follow-up.
- *Layering*: entirely pure; trivia set is a Domain constant, overridable later via
  config (Act).

### D6 — Source-path index + `--path <glob>` filter

`GraphIndex` gains `giPathIndex :: Map Text [NodeId]` (lowercased path segments →
nodes), built in `buildIndex` from `nodeSourceFile`. `--path <glob>` filters candidate
matches before traversal; bare path-like query terms (containing `/`) also consult this
index so `query "src/cli/commands"` matches.

- *Alternatives*: post-filter results by path after traversal — rejected: wastes the
  candidate budget on nodes that will be discarded and still misses path-only queries.
- *Cost*: index build stays O(N); memory adds one map keyed by path segments (bounded by
  file count, far smaller than the label index).

### D7 — `symbols` and `neighbors` subcommands

| Command | Semantics | Data path |
|---|---|---|
| `graphos symbols <name>` | exact (case-sensitive, then case-insensitive) match on the identifier token and full label; no fuzzy scoring, no BFS; prints each hit as an `explain`-style card (id, file, line, kind, degree, community) | `giLabelIndex` full-label entries + new exact-token lookup |
| `graphos neighbors <node-id> [--depth N]` | node id (not fuzzy term) → BFS to depth N (default 2) over `giAdj`; output through the same Refine pass and scored renderer (score = 1/(1+distance)) | `bfsFrom` + `Refine` |

- *Alternatives*: fold both into `query` flags (`--exact`, `--from-node`) — rejected:
  distinct verbs are self-documenting for agents and keep `query`'s contract simple.

### D8 — Uniform CLI contract via a shared options record

A shared `CommonQueryOpts` parser (graph path, budget, json, label-width, edges) is
composed into all five subcommand parsers in `app/Main.hs`, replacing the current
copy-pasted per-command options. `--json` renders `QueryResponse` (and
`explain`/`symbols`/`neighbors` cards) via Aeson with stable field names; text and JSON
renderers consume the same pure response value, so content can never diverge.

- *Alternatives*: per-command incremental flag additions — rejected: that is how the
  current inconsistency arose (postmortem 2.8).

## Risks / Trade-offs

- [Threshold miscalibration: verdicts could mark genuine hits `weak`] → thresholds are
  named Domain constants; unit tests pin behavior on representative fixtures; Act step
  re-tunes from field data before considering config exposure.
- [Render-time dedup guesses wrong and merges distinct symbols] → dedup requires
  identical source file + line, not just label similarity; property tests assert no
  merge across differing locations.
- [Output format change breaks existing consumers (scripts, the graphos agent skill)] →
  text output keeps the `NODE:`/`Connections:` vocabulary where possible; `--json` gives
  a stable contract; skill docs updated in the same change.
- [Edit-distance did-you-mean over a 500k-key token index is slow] → restrict candidate
  set by first-character buckets and length window (±2) before computing distances;
  benchmark gate < 100 ms for the suggestion step.
- [Larger `GraphIndex` memory footprint (path index)] → StrictData, path segments only
  (no full-path duplication); measured against the 100k-node fixture in Check.
- [Aeson instances on Domain types could tempt IO/serialization coupling in Domain] →
  JSON instances live next to the UseCase response type, not on raw Domain graph types.

## Verification Strategy (Check)

- **Build gate**: `cabal build` clean under `-Wall -Werror` (dev flag), inside
  `nix-shell shell.nix`.
- **Unit tests** (`cabal test`, Hspec + QuickCheck, all pure per PRD §15.3):
  - scoring/verdict: fixtures reproducing postmortem rows 3–7 assert `NoMatch`/`Weak`
    with suggestions and zero traversal output; row-8-style exact phrase asserts `Strong`.
  - determinism: same graph + query ⇒ identical result-set hash (property).
  - ordering: rendered node sequence is score-descending (property).
  - Refine: trivia-edge filter, self-edge collapse, dedup-only-on-same-location
    (property), label elision preserves node id.
  - path index: `--path` glob inclusion/exclusion; path-like bare terms match.
  - symbols/neighbors: exact-hit semantics, depth bound respected.
- **Golden tests**: text and `--json` renderings for a small fixture graph committed
  under `tests/`; JSON schema field names pinned.
- **CLI contract test**: every query-family subcommand accepts
  `--help`, `--json`, `--budget`, `--graph` (parser-level Hspec, no process spawning).
- **Performance check**: query + suggestion path on the 100k-node benchmark fixture
  < 500 ms (PRD §16.1); suggestion step alone < 100 ms.
- **Manual scenario**: rebuild nothing; run the postmortem's exact failing commands
  against an existing `graphos-out/graph.json` and record verdicts in the change's
  `check.md`.

## Iteration & Rollback (Act)

- **If Check fails on thresholds** (real hits flagged weak): retune the two constants,
  rerun the fixture suite; thresholds are isolated so no structural rework is needed.
- **If output changes break the agent skill**: the skill is updated in lockstep; if
  third-party consumers surface, `--format legacy` can be added behind the shared
  options record without touching response assembly.
- **Rollback**: change is additive at the type level; reverting the `app/Main.hs`
  renderer commit restores prior output while keeping pure improvements — no data or
  schema migration to unwind.
- **Standardize learnings**:
  - fold the trivia set and verdict thresholds into `graphos.yaml` config once field-
    validated;
  - port `QueryResponse` (verdict + hash) into the MCP `query_graph` tool as the next
    PDCA cycle;
  - open a follow-up change for build-time symbol dedup and test/spec path tagging
    (postmortem §5);
  - update workflow docs 04/05/06 and add 14-symbols / 15-neighbors, keeping the
    capability↔workflow 1:1 mapping.
