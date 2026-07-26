# Design: fix-edge-inference-blowup

## Context

Edge inference (UseCase layer, PRD §3 infer stage) runs between the first clustering and re-clustering, inside the pipeline's "Step 4" span. Three constructions scale quadratically:

| Hotspot | Location | Cost today | Observed |
|---|---|---|---|
| Full centroid mesh | `inferCommunityBridges` | O(C²) edges materialized | 314 comms → 48,112 edges on this repo; ~10⁸ edges projected at 10–15k comms → 47 GB |
| List dedup | `nubBy` in `inferTransitiveDeps`, `inferCodeDocEdges` | O(k²) comparisons | 10¹²-scale on doc-heavy corpora |
| Doc-code fan-out | `nameAlignEdges` in `inferCodeDocEdges` | O(docs × matches) unbounded | 1,271 doc files × generic labels → millions of candidate edges |
| Surprises dedup | `nubBy` in `crossCommunitySurprises` (Domain) | O(k²) over cross-community edges | grows with inferred edges |

## Goals / Non-Goals

**Goals:**
- Inference cost proportional to real graph size: O(E) bridge candidates, O(k log k) dedup, bounded fan-out.
- Same edge schema and relations; first-wins dedup semantics preserved exactly.
- Step 4 completes on 75k-node corpora within seconds and bounded memory (PRD §16.1/§16.2).

**Non-Goals:**
- Changing `EdgeDensity` semantics or adding new inference strategies.
- The other `fix-runtime-ram-crash` items (LSP concurrency, node representation, observability caps).
- Progress logging granularity inside Step 4 (worth doing, separate concern).

## Decisions

### D1 — Bridge only adjacent communities, capped (UseCase)

`inferCommunityBridges` derives candidate pairs from the graph's real edges: a pair (c1, c2) is a candidate iff at least one existing edge crosses from c1 to c2 (computed by mapping each edge's endpoints through the node→community index — O(E)). Centroid-to-centroid inferred edges are then emitted for those pairs only, truncated at a named constant (`maxCommunityBridges`).

- **Alternatives considered:**
  - *Keep full mesh but cap with `take N`* — rejected: `take` over a lazily-generated O(C²) list still biases toward low community IDs and does not reflect structure; adjacency is the honest signal.
  - *Top-K communities by size, full mesh among them* — rejected: still fabricates edges between unrelated communities; adjacency-based candidates reinforce real structure.
  - *Drop the feature* — rejected: cross-community centroid edges are used by downstream re-clustering and navigation; bounded adjacency-based version preserves the intent.
- **Semantics change:** communities with no real inter-community edge no longer get a fabricated bridge. This is the correct behavior; the old behavior was pathological at scale.

### D2 — Order-preserving Set-based dedup helper (Domain)

A `dedupOn :: Ord k => (a -> k) -> [a] -> [a]` helper (first occurrence wins, original order preserved, Set-tracked seen keys) replaces `nubBy` at all three sites. Semantics are identical to the replaced `nubBy` usages (first-wins over equal keys); complexity drops to O(k log k).

- **Alternatives considered:**
  - *`Map.fromListWith` keeping first* — loses input ordering (Map order ≠ list order); `crossCommunitySurprises` depends on confidence-sorted order.
  - *`hashNub`* — adds a hashing dependency for no measurable benefit at these sizes.
- **Placement:** helper lives in `Graphos.Domain.Analysis` (Domain, pure) and is imported by `Graphos.UseCase.Infer` — dependency direction respects clean architecture (UseCase → Domain).

### D3 — Cap label fan-out in doc-code inference (UseCase)

`codeLabelIdx`/`codeBaseIdx` drop entries whose match list exceeds a named constant (`maxLabelFanOut`, default 20). A doc header labeled "Config" matching 300 code nodes is ambient noise, not signal; a label matching ≤20 nodes is a plausible reference.

- **Alternatives considered:**
  - *Rank matches and keep best N per label* — no meaningful ranking signal exists at this layer (no embeddings here); arbitrary truncation is worse than skipping ambiguous labels entirely.
  - *Minimum label length filter* — orthogonal and weaker; can be added later if noise persists.

## Risks / Trade-offs

- [Fewer inferred edges change downstream community counts] → intended: re-clustering now sees structure-derived bridges only. Report/HTML output changes accordingly; goldens for clustering are unaffected (they don't run inference).
- [Adjacency computed over `gEdges` includes both edge directions] → pairs are normalized (ordered tuples) and Set-deduped; cost stays O(E log C).
- [Cap constants need tuning] → named constants with haddocks; conservative defaults (`maxCommunityBridges = 10000`, `maxLabelFanOut = 20`); revisit with corpus feedback in Act.
- [dedupOn keeps first, Map-based alternatives keep last] → helper is written to match `nubBy` first-wins exactly; unit-tested.

## Verification Strategy (Check)

- **Unit (Hspec, `cabal test`), new `Graphos.UseCase.InferSpec`:**
  - `dedupOn`: duplicates collapse to first occurrence, order preserved; property vs reference `nubBy` on small lists.
  - Bridges: two communities connected by a real edge get exactly one centroid bridge; two disconnected communities get none; result length ≤ cap.
  - Doc-code: a label matching ≤ cap yields edges; a label exceeding the cap yields none; no duplicate (source, target) pairs in output.
- **Integration (`cabal run graphos -- .` at default density):** "Inferred N additional edges" drops from the 48,112 baseline to the order of real inter-community adjacency; pipeline completes; `scripts/audit_graph.py` passes.
- **Build gate:** `cabal build` with dev `-Wall -Werror` clean; full suite green.

## Iteration & Rollback (Act)

- **If the corpus still blows up at Step 4:** remaining suspects are re-clustering input size and analysis; profile with `+RTS -s`/heap profiling (fix-runtime-ram-crash item 7) and open a follow-up.
- **Rollback:** two-module revert; no schema/data impact.
- **Standardize:** convention — no unbounded pair enumeration; no `nubBy` on lists that scale with graph size; caps are named constants with haddocks.
