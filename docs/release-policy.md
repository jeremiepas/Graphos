# Graphos Release Policy

Status: **active** — enforced by the Paperclip routines listed in [Automation](#automation).
Owner: CTO (portfolio authority). Execution: head-of-graphos, merge-gatekeeper.
Scope: `github.com/jeremiepas/Graphos` (private repo on GitHub Free — branch protection is
unavailable, so **Paperclip lanes are the only merge gate**; see repo `AGENTS.md`).

## 1. Repository flow

```
feature/AVI-XXX (or fix/AVI-XXX)  --PR-->  main  --tag vX.Y.Z-->  release CI  -->  GitHub Release
```

- All PRs target `main`. There is no `develop` branch; `main` is the integration and production branch.
- Every PR maps to a Paperclip issue and must carry **(a)** a live review lane and **(b)** a
  merge-gate lane assigned to `merge-gatekeeper` and `blockedBy` the review lane
  (created by the daily PR routines, below).
- Merging is the exclusive authority of `merge-gatekeeper` (repo `AGENTS.md`).
- Production-affecting or cross-project merges additionally require explicit CTO comment
  approval on the lane before merge.

## 2. Versioning

- Tags: `vX.Y.Z` on `main` (annotated). Pre-1.0: **PATCH** for fixes, **MINOR** for features
  or breaking changes. Pre-release candidates: `vX.Y.Z-rc.N` (release CI publishes them as
  pre-releases automatically).
- **Source of truth:** the `version:` field in `graphos.cabal` MUST equal the tag being cut
  (tag `v0.2.0` ⇔ cabal `0.2.0.0`). The release PR bumps the cabal version as part of the
  release. Known drift to heal at the next release: tag `v0.1.3` was cut while cabal sat at
  `0.1.2.0` — the next release PR must reconcile cabal to the released tag before bumping
  further.
- Tag pusher must act under the `jeremiepas` token identity — `.github/workflows/release.yml`
  gates on `github.actor == 'jeremiepas'`.

## 3. CHANGELOG discipline

- Every merged PR that changes behavior, CLI, output formats, or dependencies adds an entry
  under `## Unreleased` in `CHANGELOG.md` (sectioned Added / Changed / Fixed).
- The release PR rolls `## Unreleased` into `## X.Y.Z - <date>` and leaves a fresh empty
  `## Unreleased` on top.

## 4. Release criteria (checklist)

A release is due when **all** hold:

1. `CHANGELOG.md` `## Unreleased` contains at least one meaningful user-visible change since
   the latest tag.
2. `main` is green: latest CI run on `main` passed and no open PR is mid-rebase-conflict on
   `main`.
3. All reviewed + mergeable PRs have been merged through the merge gate (no approved work
   left stranded behind the release).
4. The release has explicit **CTO approval** on the release run issue (production-affecting
   gate; CTO instructions §Review gates).

If (1) fails, the verdict is **"no release this week"** — record it with evidence and close.

## 5. Release process (per cut)

1. **Weekly release run** (CTO routine) checks §4; if due, posts the proposal (proposed tag,
   changelog roll, cabal bump) and gives explicit approval on the run issue.
2. **head-of-graphos** (delegated child issue) prepares the release PR: roll the CHANGELOG,
   bump `graphos.cabal` version (healing any drift per §2), open PR to `main`, link it back
   on the release issue. Prep lane closes once the PR is open.
3. The release PR flows through the **normal review + merge lanes** like any other PR.
4. **merge-gatekeeper** merges the release PR and, immediately after the merge, pushes the
   annotated tag `vX.Y.Z` on `main` named in the release issue (the release CI then builds,
   tests, and publishes the GitHub Release with `graphos-linux-x86_64` + `.sha256` assets
   automatically). Both SHA and release URL are recorded on the merge-gate lane.
5. The next weekly release run audits the outcome: release exists, assets present, cabal
   version matches the tag. Drift or missing assets escalate to head-of-graphos.

Optional: an `-rc.N` tag may be cut the same way to validate the release pipeline before the
stable tag. Do not cut releases from any branch other than `main`.

## 6. Roles

| Role | Authority in a release |
|------|-----------------------|
| CTO | Approves each release (production gate); owns this policy; audits outcomes |
| head-of-graphos | Prepares the release PR (changelog roll, version bump); owns repo health |
| merge-gatekeeper | Merges the release PR; pushes the annotated tag; verifies CI publish |
| haskell-reviewer | Deep review of PRs (delegated by head-of-graphos) |
| security-expert | Standing scans; blocks a release on a critical finding |

## 7. Automation

| Routine (Paperclip, project `graphos`) | Schedule | Run brief |
|-----------------------------------------|----------|-----------|
| Daily Graphos PR review intake | 09:00 Europe/Paris | **Rebase-readiness gate first:** for every open PR read `mergeable_state`; if `dirty`/`behind`, do not open a review lane yet — create/repoint one rebase follow-up task owned by head-of-dev naming the PR number + base branch and record the conflict + PR link in the run issue. For mergeable PRs: every open PR carries a live review lane; create missing lanes same day; force verdicts on PRs stale >7 days |
| Daily Graphos merge gate | 13:00 Europe/Paris | PRs that are reviewed **and** mergeable get a merge-gate lane to merge-gatekeeper; dated next action otherwise |
| Weekly Graphos release run | Fri 14:00 Europe/Paris | Apply this policy §4–§5 |

### Daily PR review intake checklist (rebase-readiness gate)

Apply to every open PR, in this order, before creating any review lane:

1. Read the PR's `mergeable_state` (and `mergeable`) via the GitHub API.
2. If `mergeable_state` is `dirty` or `behind` (conflicting against the base branch, typically `main`):
   - Do **not** create or update a review-lane issue for this PR yet.
   - Create or repoint **one** rebase follow-up task owned by the dev cell (`head-of-dev`), naming the PR number and the base branch, and linked (blocked-by) from any dependent review lane.
   - Record the conflict in the daily intake run issue together with the PR link (and the rebase task id, if created). No review capacity is spent until the conflict is cleared.
   - An explicit override comment is the only way to bypass this step; record it on the run issue.
3. Otherwise (`clean`/`has_hooks`/`unstable`-with-CI), proceed: ensure each open PR maps to one live review-lane issue (non-terminal child assigned to head-of-graphos naming the PR number, review acceptance criteria, and a dated next action); create missing lanes same day; force a verdict on lanes stale >7 days.
4. Scan PR diffs for secrets and block + escalate to security-expert on any hit.
5. Record the PR ↔ lane table (including each open PR's `mergeable_state` and any created rebase tasks) as a comment on the run issue, then close it.

Do NOT merge anything in this run — merging belongs to the daily merge-gate routine.

All three fire execution issues assigned to the CTO, who triages and delegates the deep
work per the run brief — the CTO stays the router, not the reviewer.