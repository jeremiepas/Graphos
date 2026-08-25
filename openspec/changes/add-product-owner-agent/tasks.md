## Tasks

- [x] 1. Confirm hire prerequisites: agent `agents bulder` has `can_create_agents=true`; Chief of staff agent id `dd95c167-3c7e-40f3-825d-40e41c9c20cb` captured as `reportsTo`; AVI-5 issue id captured as `sourceIssueId`.
- [x] 2. Discovered allowed agent icons via `GET /llms/agent-icons.txt`; picked `target` (PO/product glyph).
- [x] 3. Inspected existing agent configs — mirrored `agents bulder`'s `opencode_local` adapter convention, `glm-5.2:cloud` model, managed instruction bundle, `dangerouslySkipPermissions: true`, heartbeat off + wakeOnDemand. (Read of `/agent-configurations` denied by `agents:suggest-changes` permission; fell back to `GET /api/agents/me` + the `agents bulder` config as the convention template.)
- [x] 4. Drafted the `AGENTS.md` instruction bundle — role, remit, reporting to Chief of staff, lateral CTO partnership, hard boundaries (no code, no review, no hiring), OpenSpec as source of truth, dispatch contract, `para-memory-files` memory, execution contract, Rule #1. Generic baseline-role-guide path (no exact PO template in `references/agents/`).
- [x] 5. Built the `agent-hires` payload with `jq --arg` (safe content embedding). Role corrected from `product-manager` to the enum value `pm` after the first 400 validation error.
- [x] 6. Walked the draft-review checklist: managed bundle (no `promptTemplate`), `cwd` pinned to Graphos repo, standard-tier model, heartbeat off, 4 PO skills, boundaries enforced at skill + prose layers, `sourceIssueId` set.
- [x] 7. Submitted `POST /api/companies/{companyId}/agent-hires` with `X-Paperclip-Run-Id` → **HTTP 201**.
- [x] 8. N/A — response carried `approval: null` (auto-approved, no board gate).
- [x] 9. Auto-approved. Agent active: id `bef0291b-256e-493b-876b-b7c864e22048`, urlKey `product-owner-graphos`, reports to Chief of staff, status `idle`. Posting completion comment and marking AVI-5 `done`.
- [x] 10. N/A — no board approval wake needed (auto-approved).