## Context

The graphos project already has a workforce of three Paperclip agents (Chief of staff / ceo, cto of graphos project / cto, agents bulder / researcher-head-of-HR) but no Product Owner. The gap is backlog ownership: nobody turns the 25+ active OpenSpec changes into a prioritized, dependency-ordered, developer-ready task stream. Today that work falls on the CTO and Chief of staff by hand — the exact high-touch pattern an agent should absorb.

This change adds one managed agent to the Paperclip company `avionix`. It touches no Graphos Haskell code, build config, or OpenSpec core specs. The OpenSpec artifacts under `openspec/changes/add-product-owner-agent/` are the durable spec for the role itself; the actual hire is a Paperclip `agent-hires` request.

## Goals

- Hire a Product Owner agent that owns the graphos development backlog end-to-end.
- Pin the agent to the Graphos repo so its prioritization is grounded in real OpenSpec/cabal/docs state, not abstracted away in the Hermes workspace.
- Install exactly the skills a PO needs (workflow, plan→task decomposition, board view, durable memory) — no coding or hiring skills.
- Encode the PO's boundaries explicitly so it does not drift into the CTO's or `agents bulder`'s remit.

## Non-Goals

- Writing or modifying any Graphos Haskell source, Domain/UseCase/Infrastructure module, cabal file, or `devenv.nix`.
- Hiring developer agents for the graphos team — that stays with `agents bulder`. The PO only signals staffing gaps.
- Code review or merge approval — that stays with the CTO / QA.
- Replacing the `paperclip-create-agent` workflow — the hire still goes through the standard `agent-hires` endpoint and board approval gate.

## Design

### Reporting line

```
Chief of staff (ceo, dd95c167)
└── Product Owner — graphos (product-manager)   [NEW]
```

The PO reports to the Chief of staff, not the CTO, so business priority is mediated by the CEO role while the PO partners laterally with the CTO on technical sequencing. This matches the existing org pattern where `cto of graphos project` also has no `reportsTo` set yet but operates as a peer technical authority; the PO's reporting line keeps the CEO as the single upward escalation point for the graphos delivery org.

### Adapter and model

- `adapterType: opencode_local` — same family as `agents bulder`, consistent with the instance's local-adapter setup.
- `adapterConfig.cwd: /home/jeremie/Documents/Graphos` — the PO lives in the project it owns the backlog for. This is the single most important adapter decision: it lets the PO run `openspec list`, read spec files, and inspect `graphos.cabal` / `CHANGELOG.md` / `docs/` directly.
- `adapterConfig.model: ollama/glm-5.2:cloud` — **standard tier**. Backlog triage, dependency mapping, and self-contained issue authoring need solid context handling and structured output, but not the largest frontier model. Cheap-tier would mis-prioritize on nuanced dependency graphs; frontier-large would burn budget on structured task decomposition that a standard model handles. Standard is the cost/quality saddle for a PO.
- `runtimeConfig.heartbeat.enabled: false`, `wakeOnDemand: true` — the PO is event-driven (Chief of staff, CTO, dependency-resolution wake), not timer-driven. A timer heartbeat would have it re-prioritize the same backlog every interval with no new context, which is waste.

### Skills (day one)

| Skill | Why |
|---|---|
| `paperclipai/paperclip/paperclip` | Core heartbeat/issue workflow — required to operate at all. |
| `paperclipai/paperclip/paperclip-converting-plans-to-tasks` | Core PO competency: turn a plan/roadmap into an executable, dependency-linked issue graph. |
| `paperclipai/paperclip/paperclip-board` | Read-only board view to surface backlog health and priority shifts to the Chief of staff without giving the PO board-write authority. |
| `paperclipai/paperclip/para-memory-files` | Durable project memory — priority decisions and roadmap rationale survive across heartbeats instead of being re-derived each wake. |

No coding skills (e.g. no `coder`, no `qa`) and no hiring skills — enforcing the boundary at the skill layer, not just in prose.

### Instruction bundle (`AGENTS.md`)

Managed bundle (`instructionsBundle.files["AGENTS.md"]`), no `promptTemplate`. The bundle encodes:

1. **Role** — Product Owner for graphos; owns the development backlog.
2. **Remit** — prioritize against OpenSpec state, sequence by dependency, author developer-ready child issues, dispatch to the developer team.
3. **Reporting** — reports to Chief of staff; partners with CTO on technical sequencing.
4. **Boundaries** — does NOT write Haskell, does NOT review code, does NOT hire. Escalates staffing gaps to `agents bulder`, technical judgment to the CTO.
5. **Source of truth** — `openspec list` / `openspec show` / spec files in the Graphos repo; `graphos.cabal` and `docs/` for surface awareness.
6. **Dispatch contract** — Paperclip child issues with `parentId`/`goalId`, `blockedByIssueIds` for dependency order, self-contained descriptions with acceptance criteria and repo-relative pointers.
7. **Memory** — durable priority/roadmap rationale in `para-memory-files`.

### Governance

The hire goes through `POST /api/companies/{companyId}/agent-hires` with `sourceIssueId` = AVI-5. If the instance requires board approval, the response carries an `approval` object and the PO is `pending_approval` until the board acts; on approval the originating issue is closed/commented per the `paperclip-create-agent` follow-up loop.

## Risks

- **Model under-power**: if `glm-5.2:cloud` struggles with the 25+ change dependency graph, prioritization quality drops. Mitigation: the spec requires a standard-tier model (not cheap); if observed, escalate to a large-tier model swap via the Chief of staff.
- **Boundary drift into CTO remit**: a PO that starts making architectural calls collides with `cto of graphos project`. Mitigation: the boundary is enforced in the instruction bundle, the skill set (no coding skills), and the spec scenarios (PO routes technical judgment to the CTO).
- **Stale OpenSpec state**: the PO reads the OpenSpec tree as ground truth; if change directories are abandoned without archiving, prioritization is noisy. Mitigation: the PO surfaces 0/N-tasks stale changes as a "defer or re-prioritize" decision rather than silently dropping them.
- **Missing developer agent**: the PO cannot dispatch to a developer that does not exist. Mitigation: the spec requires escalation to `agents bulder` with a named role request rather than silent failure or self-implementation.