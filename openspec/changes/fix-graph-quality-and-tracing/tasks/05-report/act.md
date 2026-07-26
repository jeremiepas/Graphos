# Act: Report/export consistency

## Standardized
- `scripts/audit_graph.py` is the regression gate for future graph output changes.

## Follow-up
- Add the audit script to CI so every PR that changes extraction/build/report must pass it on a full pipeline run.
