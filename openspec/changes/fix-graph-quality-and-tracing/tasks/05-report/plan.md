# Plan: Report/export consistency

## Goal
Ensure `GRAPH_REPORT.md` and `graph.json` share the same graph state and that duplicate surprising connections are removed.

## Approach
- Move incremental `writeNodes`/`writeEdges` to after enrichment and re-clustering, using the final graph and community map.
- Deduplicate surprising connections in `Report.hs` by (source, target, relation, reason).
- Create an audit script that validates report/export parity and quality targets.

## Check Criteria
- Report totals equal `graph.json` totals.
- No duplicate "Surprising Connections" entries.
- Audit script passes on the integration run.
- `cabal test` and `cabal build` pass.

## Affected Files
- `src/Graphos/UseCase/Pipeline.hs`
- `src/Graphos/UseCase/Report.hs`
- `scripts/audit_graph.py`
