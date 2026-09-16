## Tasks — correct 09-merge mergeGraphs wording to old-wins

- [ ] Write the `## MODIFIED Requirements` delta for workflow-09 in the change dir (old-wins wording).
- [ ] Align `docs/workflows/09-merge.md` node-dedup + edge-merge wording to old-wins.
- [ ] Grep `openspec/` and `docs/` for any remaining new-wins claim on `mergeGraphs` and align (leave immutable archive snapshots and theoretical "last-write-wins has no logical timestamp" arguments untouched).
- [ ] `openspec validate --changes` passes, then `openspec archive avi-836-correct-merge-spec-wording -y`; confirm live `openspec/specs/09-merge/spec.md` no longer claims new-wins.
- [ ] Commit `AVI-836: ...`, push branch, open PR `feature/AVI-659 -> develop`, request review from `haskell-reviewer`.
