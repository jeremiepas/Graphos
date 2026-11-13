# Fineract before/after verification (task 5.3)

Full-corpus rerun against [apache/fineract](https://github.com/apache/fineract)
(shallow clone, HEAD at the time of this run, 7250 `.java` files, 144 MB) — the
repository that originally motivated this change. Both runs used identical
flags (`--no-viz --no-cluster --fresh --no-detect --no-semantic-edges
--granularity fine`) so the only variable is the binary:

- **before**: `graphos` built from `main` (commit `8b1c8fa`, pre this change)
  in a separate git worktree.
- **after**: `graphos` built from this change's branch tip.

## Headline numbers

| Metric | Before | After | Delta |
|---|---:|---:|---:|
| Total nodes | 326,483 | 325,941 | −542 |
| Total edges | 351,240 | 414,375 | +63,135 (+18.0%) |
| `calls` edges | 0 | 20,709 | **+20,709** |
| `contains` edges | 267,501 | 309,887 | +42,386 |
| `references` edges | 64 | 104 | +40 |
| `type_identifier` nodes (bare reference leak) | 2,824 | 0 | **−2,824** |
| Methods labeled with a bare return-type name (`void`, `int`, `String`, …) | 5,803 / 40,059 (14.5%) | **0** / 40,059 | −5,803 |
| Nodes carrying a structured `annotations` array | 0 | 33,920 | +33,920 |
| Nodes carrying `httpVerb` | 0 | 829 | +829 |
| Nodes carrying `route` | 0 | 782 | +782 |
| Nodes carrying `configKeys` (`@Value`) | 0 | 37 | +37 |
| Nodes carrying `topics` (`@KafkaListener`/`@RabbitListener`) | 0 | 1 | +1 |
| `Env` nodes (`System.getenv(...)`) | 0 | 2 | +2 |
| `Config` nodes (`@Value("${...}")`) | 0 | 36 | +36 |
| `Topic` nodes | 0 | 1 | +1 |

Every number in this table that this change's proposal predicted moving (the
four diagnosed gaps) moved in the predicted direction, at real-world scale —
this isn't just the synthetic fixture working, it's the actual originating
corpus.

## Secondary observation (not predicted, found while cross-checking)

`Conditional`/`Statement`/`Return`/`Loop`/`Try`/`Field`/`Property` node counts
each increased too (e.g. `Statement` 4,279 → 6,009, `Conditional` 257 → 524).
Root cause, confirmed by diffing a sample of the newly-surviving nodes: every
identifier-less Tier-3 node (`if_statement`, `return_statement`, etc.) gets
`baseId = ""` with no per-file prefix (`maybe "" (...)`, not
`makeNodeId sf kind ""`), so its final id after per-file `#L<line>` suffixing
is a **bare, kind-agnostic, cross-file-collidable** string like `"#L2902"`.
When every file's extraction is merged into one corpus-wide node map, an
identifier-less node from file A can silently overwrite an unrelated
identifier-less node from file B at the same line number — regardless of
kind. The 2,824 `type_identifier` nodes removed by this change were
themselves identifier-less and were occupying slots in that same shared id
space; removing them freed up collisions that were previously hiding other
kinds' nodes.

This is a **pre-existing architectural gap** (the `baseId = maybe "" (...)`
fallback, unchanged by this change) that this change did not introduce and
does not fully fix — Tier-3 nodes of the *same* kind at the same cross-file
line number can still collide. Worth its own follow-up; out of scope here.

## Gaps this run surfaced in the shipped scope

- `@Scheduled` usages in Fineract did not produce any `cron`/`fixedDelay`
  distillation keys — likely `@Scheduled(fixedRate = ...)` /
  `@Scheduled(initialDelay = ...)`, which `namedArg` doesn't look for (only
  `cron` and `fixedDelay` are implemented). Minor, easy follow-up.
- Only 1 `topics`/`Topic` node despite Fineract's Kafka usage — consistent
  with `@KafkaListener`/`@RabbitListener` being rare relative to raw
  `KafkaTemplate`/produce-side code, which this change's annotation-only gate
  does not cover (by design — producer-side topic names aren't annotation
  arguments).
