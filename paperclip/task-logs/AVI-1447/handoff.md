# AVI-1447 — Calls-edge emission (issue finalized)

**Change dir:** `openspec/changes/AVI-1447-calls-edge-emission/`
**Branch:** `feature/AVI-1447-calls-edge-emission` (head `1a4d24e`, on origin)
**Issue:** system identifier **AVI-1462**, UUID `49e41cd2-cd1e-4f04-afaf-fa7fd296436b`;
title text "AVI-1457: emit calls edges for call/new expressions (change
AVI-1447-calls-edge-emission)"; change contract `AVI-1447-calls-edge-emission`.

> Naming note: the title/summary text references "AVI-1457" but the **system
> `identifier` on the issue is `AVI-1462`**. This is a historical merge artifact
> (the issue was created under AVI-1462 with an AVI-1457 title). The canonical
> identifier is **AVI-1462**; it was left unchanged. The work itself is the
> `AVI-1447` calls-edge change.

---

## FINAL STATUS — finalized 2026-10-26 by run `4f06fa87`

**Issue is CLOSED.** Status `in_progress` → **`done`**; the zombie execution lock is
released (`executionRunId`/`checkoutRunId` cleared to `null`).

**Governance outcome recorded on the issue (AGENTS.md §Branch/PR hygiene, outcome #2):**
a **`branch` work product** was recorded naming `feature/AVI-1447-calls-edge-emission`
and stating why the PR is deferred. Two comments (findings + disposition) were posted.
No `pull_request` work product exists because **no GitHub PR can be opened** in this
environment (see below).

**No force-mutation of run/issue state was needed.** Unlike the prior attempt by run
`b2df5d67` (which correctly reported a 409 run-ownership conflict because that run was
not the owner), this run **is** `4f06fa87-662f-42f7-8630-77466818dd25` — the run that
held the lock — so writes succeeded with `authorizationReason: allow_self`
(HTTP 201/200). The lock released itself on the `done` transition.

---

## What is DONE (local + origin)

| Piece | Commit | On origin? |
|---|---|---|
| `calls` edge emission in `Convert.hs` (Task 2) | `97dfa23` | ✅ pushed |
| `calls` edge tests in `ConvertSpec.hs` (Task 3) | `97dfa23` | ✅ pushed |
| Delta spec dir (proposal.md, specs/extraction/spec.md, tasks.md) | `1a4d24e` | ✅ pushed |

`origin/feature/AVI-1447-calls-edge-emission` is at `1a4d24e` (impl `97dfa23` + delta
spec `1a4d24e`). **All work — implementation, tests, and delta spec — is on origin.**

> Verified: `graphos-test` binary green — `1355 examples, 0 failures, 2 pending`. The
> two pending are fidelity oracles (ImportEdgesSpec, GraphCoverageSpec) gated on
> `GRAPHOS_FIDELITY_ROOT`/`GRAPHOS_FIDELITY_GRAPH` (unset here), so they are pending by
> design, not failures. The unrelated prior `UpdateConfluenceSpec` failure is gone.
> Real graph (`/tmp/fresh2/graph.json`): `calls = 1` (baseline 0);
> `contains`/`imports`/`depends_on` unchanged.

### Emission contract (per `specs/extraction/spec.md`, asserted by tests)
- `edgeId = srcId <> "->call:" <> tgtId`, `Calls`, `Confidence 0.9`, weight `0.9`, `edgeExtra = Nothing`.
- Source = the enclosing definition's label; target = first `identifier` node of the callee expression (member_expression → receiver, e.g. `db` from `db.get`).
- New `new Foo()` targets the imported `External`/file node.
- Nested-call suppression: only the outermost call is emitted.
- Unresolvable callee (global/builtin) → drop the edge, never crash.
- No regression of `contains`/`imports`/`depends_on`.

---

## Why the PR is deferred (governance gate)

The implementation and tests are complete; the only remaining step — opening the PR —
could not be completed because **no usable GitHub identity exists in this environment**:

- GitHub REST API: both `GH_TOKEN` and `GITHUB_TOKEN` return `401 Bad credentials`;
  remote GitHub API is unreachable (TLS handshake to the host times out).
- `gh` CLI: the only binary on the path is a Paperclip shim refusing every GitHub
  command ("requested GitHub command is not installed").
- `PAPERCLIP_GITHUB_AUTH_MODE=managed`, `PAPERCLIP_GITHUB_OPERATION_ACTIVE` unset.

So the branch is retained (explicit keep) and the PR is deferred only pending a valid
GitHub identity. The delta spec `1a4d24e` was already pushed — there is nothing left to
push. Any future task-bound run with real GitHub access can open the PR from the existing
branch head:

```bash
git checkout feature/AVI-1447-calls-edge-emission   # already on origin at 1a4d24e
gh pr create --base main --head feature/AVI-1447-calls-edge-emission \
  --title "AVI-1447: emit calls edges per spec (enclosing-def source, resolved callee target)"
```

---

## Control-plane artifacts recorded this run (2026-10-26, run `4f06fa87`)

- Issue `49e41cd2` (AVI-1462) status **`in_progress` → `done`** (PATCH, HTTP 200,
  `allow_self`).
- **`branch` work product** recorded (POST HTTP 201): names
  `feature/AVI-1447-calls-edge-emission`, status `active`, `isPrimary: true`, with the
  full explicit-keep rationale.
- Two comments: findings (`**AVI-1462 / AVI-1457 — calls-edge emission: implementation
  complete, PR deferred**`) and disposition (`Disposition: implementation + tests
  complete and verified …`).

## What was deliberately NOT done

- No GitHub PR opened (no GitHub identity available).
- No run or issue state was force-mutated; the lock released naturally on the `done`
  transition — no external head-of-dev action is required.
- The canonical issue identifier (AVI-1462) was left unchanged.
- Code was not modified in this run.

---

### Artifacts produced by this run (local only)
- `AVI-1447-calls-edge-handoff.md` — this final handoff, reflecting the recorded
  `branch` work product and `done` status.
- No branch was modified beyond the already-present origin commits.
