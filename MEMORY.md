# MEMORY.md — devhaskell-g5-gap-1 (AVI-512)

## Branch / PR hygiene
- **Branch:** `feature/AVI-512-devhaskell-g5-gap-1` (created via `git checkout -b`).
- **Why kept explicit (no PR):** No managed GitHub identity available this run → cannot push + open PR. Commit-only work on the branch.
- **Outcome:** Explicit keep — no merge, no push to origin/main. Lane remains in *waiting/escalated* posture until a managed identity is available.

## Change: devhaskell-g5-gap-1
- **Change name / issue:** `devhaskell-g5-gap-1` (G5 GAP-1 Surface Functions, AVI-1291 math-requirements adoption).
- **Schema:** spec-driven.
- **Archive location:** `openspec/changes/archive/2026-11-03-devhaskell-g5-gap-1/`.
  - `proposal.md` (Why + What Changes)
  - `design.md`
  - `specs/spec.md` (delta spec for new capabilities: cond, isAcyclicDirected, connectedComponents, weaklyConnectedComponents, isolatedNodes, hasCycleUndirected, *WithCached* variants; builds on existing scc-decomposition spec).
  - `tasks.md` (all tasks `- [x]`).
- **Artifacts status:** all done.
- **Queue / SLA:** No open PR → no merge gate. Escalate to head-of-dev when a managed identity is available.

## Notes
- PR #209 (pr128-head) was OPEN/DIRTY/reviewRequired for AVI-1966; unrelated lane, not this change.
- Commit target: `AVI-512-add-math-in-graphos-project` per heartbeat AVI-1348.
