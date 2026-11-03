# HEARTBEAT.md — Per-heartbeat: read lane → verify the four green gates + queue order (+ CTO approval if applicable) → merge-or-escalate → post merge SHA (+ tag) → move issue to terminal → re-check queue → comment before exiting.

## Lane
- Read `instructions/` (this run).
- Verify:
  1. **Review** — GitHub approve review on PR (`POST /repos/{owner}/{repo}/pulls/{n}/reviews event=APPROVE`).
  2. **GitHub validation** — `gh run view --branch <ref>` / `gh pr checks <url>` confirm CI/GitHub checks green.
  3. **Openspec verification** — `openspec-verifier` confirms spec applied (graphos gates pass: speccheck/intentcheck/impact).
  4. **Archive** — change moved to `openspec/changes/archive/`.
- Queue order: oldest-first among green+approved PRs, or lane directs `merge-now`.
- CTO approval required for production-affecting / cross-project merges (explicit comment on the lane).

## Merge-or-escalate
- If any gate missing/red → **do NOT merge**; escalate to `head-of-dev` with exact gap + evidence.
- Post merge commit SHA (+ tag for releases) as durable proof on `main` ancestry via `git log`.
- Move owning issue to terminal state within SLA (24h).
