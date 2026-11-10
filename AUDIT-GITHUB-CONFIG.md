# GitHub Configuration Security Audit — `jeremiepas/Graphos`

**Issue:** [AVI-1484](/AVI/issues/AVI-1484) — *audit github configuration*
**Date:** 2026-10-26
**Scope:** Public repository `jeremiepas/Graphos` (Haskell). External-contributor access limited to the security level requested.
**Data source:** GitHub REST API (`GET /repos/{owner}/{repo}`, collaborators, branch protection, environments, Actions permissions, security-and-analysis, workflows). Authenticated with a broker-minted managed GitHub token.

---

## Verdict

**The repository's access model is already well-scoped for an open-source project.** There is exactly **one collaborator** (the owner, `jeremiepas`) with `admin` role, **0 outside collaborators**, and **0 pending invitations** — so no external contributor has any direct push/collaborator access. External parties can only interact through **fork pull-requests**, and those execute in GitHub's sandboxed runners with `contents: read` only. No `pull_request_target` triggers exist, so fork PRs **cannot** read repository secrets or run with elevated repository permissions.

Several hardening gaps remain (branch protection off, fork-PR CI execution, no CODEOWNERS, Dependabot security updates off) that do **not** grant external access today but should be closed to defend the owner-only access model.

---

## Findings

### F1 — No branch protection on `main`  ⬛ HIGH
- `GET /repos/jeremiepas/Graphos/branches/main/protection` → **404 "Branch not protected"**.
- `GET /repos/jeremiepas/Graphos/protect/branches` → **404** (no protected branches at all).
- Default branch is `main`; all three merge modes (rebase / squash / merge-commit) are enabled.
- **Impact:** with push access, changes (including force-push) can land directly on `main`. Today this is mitigated solely by the single-collaborator ownership model, so it does not expose *external* access — but it is the single biggest gap if any collaborator is ever added.
- **Recommendation:** enable branch protection on `main` (required status checks, required linear history or required review on PRs, `Require branches to be up to date`), and push-protection review.

### F2 — Fork pull-requests execute CI  🟡 MEDIUM
- Workflows with a `pull_request` trigger: `haskell.yml`, `lean.yml`, `graphos-analyze.yml`, `openspec-archived.yml`, `studio.yml`.
- All use `actions/checkout@v6`; `graphos-analyze.yml` declares `permissions: contents: read`.
- **Impact:** an external fork actor can open a PR and run workflow steps on GitHub-hosted runners. Because the trigger is `pull_request` (not `pull_request_target`), this runs with the PR's read-only scope on a sandboxed runner — **no access to repo secrets and no elevated repo rights**. Residual risk is command-injection via PR-provided content processed in `run:` steps and any self-review on fork PRs.
- **Recommendation:** review whether any `run:` step interpolates PR-body/PR-author values (prefer `${{ github.event.pull_request.user.login }}` auto-sanitization; never embed unchecked PR content in shell). Consider restricting fork execution (e.g. `pull_request_target` + human review, or `actions/checkout` with fork pull request protection) if the workflows process untrusted input.

### F3 — No CODEOWNERS  🟡 MEDIUM (low external impact)
- `GET /repos/jeremiepas/Graphos/contents/CODEOWNERS` → **404** (absent).
- **Impact:** no enforced ownership/review of key paths (e.g. `.github/workflows`). Mitigated today by owner-only collaboration.
- **Recommendation:** add a `CODEOWNERS` file requiring review of workflow, CI, and infrastructure paths.

### F4 — Dependabot security updates disabled  🟢 LOW
- `security_and_analysis.dependabot_security_updates.status` = **disabled**. Secrets scanning (**enabled**) and push protection (**enabled**) are on.
- **Impact:** no automated advisory on vulnerable dependencies. Does not affect contributor access.
- **Recommendation:** enable Dependabot security updates (and optionally Dependabot version updates) in Settings → Pull requests → Security.

### F5 — Repo Actions permissions allow all actions  🟢 LOW
- Actions permissions = `allowed_actions: ALL` (not restricted to GitHub-published actions), `sha_pinning_required: false`.
- **Impact:** public Actions runners (composite/JS actions) can reference third-party code. Low external impact given no secrets leak, but narrows the blast radius of any malicious OSS vulnerability.
- **Recommendation:** consider `allowed_actions: PUBLISHED_GITHUB_ACTIONS` for tighter pinning.

### F6 — Access summary (the requested control)  ✅ SATISFIED
- Direct collaborators: **1** — `jeremiepas` (admin).
- Outside collaborators: **0**.
- Pending invitations: **0**.
- Owner account `jeremiepas` is a **personal account** (no org; `GET /orgs/jeremiepas` → 404). No environments defined; no Actions secrets configured.

---

## Recommendations (prioritized)

1. **Enable branch protection on `main`** (F1) — required checks + PR reviews; this is the control that makes the owner-only model durable if collaborators are ever added.
2. **Review/limit fork-PR CI execution** (F2) — scan `run:` steps for untrusted interpolation; adopt fork pull-request protection.
3. **Add `CODEOWNERS`** (F3) to enforce review of `.github`, CI, and infra paths.
4. **Enable Dependabot security updates** (F4).
5. **Constrain Actions permissions to published actions** (F6/F5) as defense in depth.

---

## Conclusion

For the stated objective — verifying that **external contributors have limited access** — the configuration is **acceptable at the security level requested**: no external collaborator or outside access exists, and fork PRs are confined to sandboxed, `contents: read` CI with no secret exposure. The open gaps are hardening controls (branch protection, CODEOWNERS, Dependabot) that should be closed to *protect* that owner-only model, but none currently grant external access.
