## Tasks

- [ ] 1. Confirm hire prerequisites: agent `agents bulder` has `can_create_agents=true`; capture Chief of staff agent id `dd95c167-3c7e-40f3-825d-40e41c9c20cb` as `reportsTo`; capture AVI-5 issue id as `sourceIssueId`.
- [ ] 2. Discover allowed agent icons via `GET /llms/agent-icons.txt`; pick an icon consistent with existing agents (e.g. a PO/product glyph).
- [ ] 3. Inspect existing agent configs via `GET /api/companies/{companyId}/agent-configurations` to mirror naming, icon, reporting-line, and adapter conventions used by `agents bulder` and `cto of graphos project`.
- [ ] 4. Draft the `AGENTS.md` instruction bundle for the Product Owner — graphos role per the design (role, remit, reporting, boundaries, OpenSpec as source of truth, dispatch contract, memory). Use the `paperclip-create-agent` baseline role guide (generic fallback — no exact PO template) adapted for a PO.
- [ ] 5. Build the `agent-hires` request payload: name `Product Owner — graphos`, role `product-manager`, `reportsTo` = Chief of staff id, `adapterType: opencode_local`, `adapterConfig.cwd` = `/home/jeremie/Documents/Graphos`, `adapterConfig.model` = `ollama/glm-5.2:cloud`, `runtimeConfig.heartbeat.enabled: false`, `wakeOnDemand: true`, `desiredSkills` = the four PO skills, `instructionsBundle.files["AGENTS.md"]` = drafted bundle, `sourceIssueId` = AVI-5 id.
- [ ] 6. Walk the `paperclip-create-agent` draft-review checklist and fix any failing item before submitting.
- [ ] 7. Submit the hire: `POST /api/companies/{companyId}/agent-hires` with `X-Paperclip-Run-Id` header.
- [ ] 8. If the response carries an `approval` object, post a comment on AVI-5 linking the approval and the pending agent; leave AVI-5 in `in_review` pending board decision.
- [ ] 9. If the hire is auto-approved (no approval object), post a completion comment on AVI-5 with the new agent's id/urlKey and mark AVI-5 `done`.
- [ ] 10. On board approval wake (`PAPERCLIP_APPROVAL_ID`), read the approval + linked issues, confirm the agent is active, and close AVI-5 with a comment naming the agent and its initial reporting line.