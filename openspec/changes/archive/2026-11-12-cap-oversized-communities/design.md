## Context

Leiden clustering (resGamma default 1.0) can place a densely self-referential
subgraph into a single community regardless of resolution. Observed: a 32k–43k
node community in a 132k-node graph, resistant to resolution tuning. This is a
structural property of the input, so the fix must be a deterministic
post-clustering guard rather than a parameter tweak.

## Goals / Non-Goals

**Goals:**
- Guarantee no community exceeds a configurable fraction of the graph.
- Keep centrality metrics meaningful when noise nodes are present.
- Pure, deterministic splitting in Domain/Community.

**Non-Goals:**
- Replacing Leiden or changing its parameters.
- Deciding what counts as generated/vendored (owned by the detection change).

## Decisions

- **Recursive sub-clustering of oversized communities**: re-run Leiden on the
  induced subgraph of an oversized community with a higher effective resolution,
  falling back to connected-component or degree-based partitioning if it still
  will not split.
  - *Alternative considered:* global resolution increase — rejected, empirically
    ineffective and harms well-formed communities.
  - *Alternative considered:* hard random chunking — rejected, destroys semantic
    cohesion; used only as last-resort fallback.
- **Cap expressed as a fraction of total nodes**, not an absolute count, so it
  scales across repo sizes.
  - *Alternative considered:* absolute cap — rejected, brittle across graph sizes.
- **Centrality exclusion via a node flag** set by detection, read by Analysis.
  - *Alternative considered:* post-hoc filter on results — rejected, still wastes
    computation and can miss transitive effects.

## Risks / Trade-offs

- [Recursive split cost on very large communities] → bounded recursion depth and
  a node threshold; last-resort partition guarantees termination.
- [Split reduces cohesion of a legitimately large module] → cap fraction is
  configurable and can be disabled.
- [Fallback partitioning is semantically weak] → only triggered when Leiden
  cannot split; logged clearly.

## Migration Plan

- Additive with a safe default (0.05); regenerate graph to apply.
- Rollback: disable the cap.
- Verify with `cabal test` (splitter properties: no community over cap,
  termination) and a smoke run on a graph containing a known blob.
