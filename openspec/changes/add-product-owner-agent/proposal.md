## Why

The graphos project (Haskell LSP-based context graph builder) currently has no agent that owns the development backlog end-to-end. The existing agent workforce — `Chief of staff` (ceo), `cto of graphos project` (cto), and `agents bulder` (researcher / head of HR) — covers strategy, technical direction, and hiring, but nobody translates project goals into a prioritized, dependency-ordered, developer-ready task stream. The CTO and Chief of staff are forced to do backlog grooming and developer-team coordination by hand, which is exactly the high-touch work an agent should absorb (Paperclip Rule #1: never ask a human to do what an agent could do).

The `graphos` project carries a deep Haskell graph-tool surface (FGL, Leiden community detection, LSP extraction, MCP server, Neo4j/Memgraph export, observability) tracked across 25+ active OpenSpec changes. Prioritizing that backlog, sequencing it by dependency, and dispatching bounded work to developer agents requires a dedicated Product Owner that lives inside the Paperclip workforce, reports to the Chief of staff, and partners with the CTO on technical sequencing.

## What Changes

- **Add a `Product Owner — graphos` managed agent** in the Paperclip company `avionix`, reporting to the `Chief of staff` (`dd95c167-3c7e-40f3-825d-40e41c9c20cb`).
- **Role: `product-manager`** — owns the graphos development backlog: reads the project's OpenSpec changes/specs, prioritizes work against project goals, breaks epics into developer-ready child issues, sequences them by dependency (`blockedByIssueIds`), and dispatches them to the developer team via Paperclip child issues.
- **Model: `standard`** (e.g. `ollama/glm-5.2:cloud`) — backlog triage and issue authoring need solid context handling but not the largest frontier model; a standard-size model balances budget and quality for structured task decomposition.
- **Skills on day one** (from the company-skills catalog / Paperclip skill library):
  - `paperclipai/paperclip/paperclip` — core heartbeat/issue workflow (required to operate).
  - `paperclipai/paperclip/paperclip-converting-plans-to-tasks` — converts plans/roadmaps into executable, dependency-linked issue graphs (core PO competency).
  - `paperclipai/paperclip/paperclip-board` — read-only board view for surfacing backlog health and priority shifts to the Chief of staff.
  - `paperclipai/paperclip/para-memory-files` — durable project memory so priority decisions and roadmap rationale survive across heartbeats.
- **Adapter: `opencode_local`** with `cwd` pinned to the Graphos repo (`/home/jeremie/Documents/Graphos`) so the agent can read `openspec/changes`, `openspec/specs`, `graphos.cabal`, and `docs/` to ground its prioritization in the real codebase state.
- **Heartbeat: disabled** (`runtimeConfig.heartbeat.enabled=false`, `wakeOnDemand=true`) — the PO wakes when the Chief of staff, CTO, or a dependency-resolution event routes work to it; it does not poll on a timer.
- **Capabilities**: `Owns the graphos development backlog: prioritization, dependency sequencing, developer-ready child-issue authoring, and dispatch to the developer team. Does not write product code.`
- **Boundary**: the PO does NOT write Haskell, does NOT review code (that is QA / CTO), and does NOT hire (that is `agents bulder`). It owns what to build and in what order, not how to build it.

No existing Graphos pipeline code, Domain/UseCase/Infrastructure modules, or Haskell sources change. No `opencode.json`, `devenv.nix`, or build config changes. This is a Paperclip-workforce-only addition.

## Capabilities

### New Capabilities
- `product-owner-graphos`: The Paperclip managed agent — backlog ownership, OpenSpec-aware prioritization, dependency-sequenced child-issue authoring, and developer-team dispatch for the graphos project.

### Modified Capabilities
<!-- None — no existing spec-level behavior changes. -->

## Impact

- **Code**: None. No Haskell sources, no Domain/UseCase/Infrastructure modules. The addition is a Paperclip agent-hire request plus an `AGENTS.md` instruction bundle.
- **Files added**: Paperclip `agent-hires` record; managed instruction bundle `AGENTS.md` (stored under the agent's `instructions/` path by the platform, not in the Graphos repo).
- **OpenSpec artifacts**: this change (`add-product-owner-agent`) — `proposal.md`, `specs/product-owner-graphos/spec.md`, `design.md`, `tasks.md` — is the durable spec for the role.
- **Dependencies**: No new Haskell or npm dependencies. Relies on existing Paperclip control-plane APIs (`/api/issues`, `/api/companies/{id}/issues`) and the agent's read access to the Graphos repo (OpenSpec tree, cabal file, docs).
- **APIs**: None new. The agent uses Paperclip issue/child-issue/subtask endpoints.
- **Compatibility**: Additive. Existing agents (Chief of staff, cto of graphos project, agents bulder) are untouched. No **BREAKING** changes.