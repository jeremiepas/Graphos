# Do: Report/export consistency

## Changes Made
- `src/Graphos/UseCase/Pipeline.hs`
  - Removed early `writeNodes`/`writeEdges`.
  - Added final `writeNodes`/`writeEdges` after re-clustering, using `enrichedGraph'` and `finalComm`.
- `src/Graphos/UseCase/Report.hs`
  - Added `dedupSurprises` sorting/grouping by (source, target, relation, reason) and rendering only one entry per group.
- `scripts/audit_graph.py`
  - Created regression script comparing `GRAPH_REPORT.md` totals to `graph.json` and checking for junk labels / cross-file imports / component count.
