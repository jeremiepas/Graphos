## Context

Leiden clustering exposes `resGamma` (default 1.0) via `--resolution`. The help
text claims "higher = fewer larger communities" and recommends 0.3–0.5 for large
graphs. Empirically, lowering to 0.4 enlarged the biggest community and 2.0 did
not split it, so the guidance is misleading and possibly the mapping is inverted
relative to standard Leiden (where higher resolution yields more, smaller
communities).

## Goals / Non-Goals

**Goals:**
- Documentation that matches real behavior, verified by measurement.
- A runtime echo of the effective resolution.
- A clear caveat about dense subgraphs.

**Non-Goals:**
- Redesigning the clustering algorithm.
- The community-size cap itself (separate change, referenced here).

## Decisions

- **Verify empirically first**, then align docs to observed behavior; only change
  code if the mapping is provably inverted relative to intent.
  - *Alternative considered:* assume standard Leiden and rewrite docs — rejected,
    the observed behavior must be the source of truth.
- **Add an INFO echo of effective resolution** so runs are self-documenting.
  - *Alternative considered:* debug-only — rejected, low-cost and broadly useful.
- **Reference the community-size cap** for the dense-blob case rather than
  implying resolution can fix it.
  - *Alternative considered:* omit caveat — rejected, this was the exact source of
    wasted runs this session.

## Risks / Trade-offs

- [Correcting a code mapping could change existing users' results] → prefer a docs
  fix unless the mapping is clearly inverted; if code changes, note it as behavior
  change and keep the default (1.0) stable.
- [Empirical check depends on a representative graph] → use a known large graph
  with a dense component for verification.

## Migration Plan

- Docs-first; optional code correction gated on the empirical finding.
- Rollback: revert doc/log changes.
- Verify with a measured `--cluster-only` sweep (e.g. 0.5 / 1.0 / 2.0) recording
  largest-community size, and confirm the help text matches the trend.
