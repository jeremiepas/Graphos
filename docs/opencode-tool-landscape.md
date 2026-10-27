# OpenCode Tool Landscape — Graphos Environment

## Overview

This document describes the tools, injected capabilities, MCP servers, skills, and
agents present in the OpenCode instance operating on the **Graphos** repository
(`/home/jeremie/Documents/Graphos`). Every entry below is grounded in live discovery
of this instance — the `opencode` CLI, the project `opencode.json` config, the
`.opencode/` on-disk layout, injected skill/agent definitions, and the graphos MCP
server source — rather than in the upstream tool catalog.

> **Scope note:** This is an *environment-grounded* reference. It documents what is
> actually wired into this instance, not the full upstream OpenCode v2 tool list.
> The upstream catalog is referenced only where it clarifies a built-in tool.

## Runtime Facts

| Fact | Value |
|------|-------|
| `opencode` version | v2.0.32 |
| Primary model | `ornith/ornith-1.5-35b-a3b` (local llama.cpp, 262k ctx) |
| Small / fallback model | `lfm2-8b` (8k ctx) |
| Global config dir | `~/.local/share/opencode/` (state), `~/.config/opencode/` (config) |
| Global MCP servers | **None configured** (`opencode mcp list` → "No MCP servers configured") |
| Project config | `opencode.json` (no `mcp` section) |

## 1. Built-in Tools

These tools are provided by the OpenCode harness itself and are available to an
agent in this session. They are the agent's native toolset (no config required):

| Tool | Purpose |
|------|---------|
| `read` | Read files / directories (text, images, PDFs); line-numbered output. |
| `write` | Create or overwrite a file (missing parent dirs are created). |
| `edit` | Replace exact text in a file (single or `replaceAll`); never edits by line numbers. |
| `shell` | Execute shell commands on Linux; supports `background`/`timeout`. |
| `glob` | Search paths by glob pattern. |
| `grep` | Search file contents (ripgrep regex/literal). |
| `question` | Ask the user clarifying questions (interactive runs only). |
| `webfetch` | Fetch an HTTP/HTTPS URL as text, markdown, or HTML (read-only). |
| `websearch` | Search the web. |
| `execute` | Run JavaScript in a confined Code Mode runtime; `fetch` + a limited tool catalog. |
| `subagent` | Spawn a foreground/background child agent (`explore`, `general`). |
| `skill` | Load a skill's instructions + resources (or one already present). |

### Harness-attached `opencode` tools (MCP namespace `tools.opencode.*`)

A small set of OpenCode-native tools is attached under the `opencode` MCP namespace
and is available alongside the built-ins above:

| Tool | Purpose |
|------|---------|
| `tools.opencode.list_mcp_resources` | List MCP servers' documents/records. |
| `tools.opencode.models` | Search models available to this instance (by provider/query). |
| `tools.opencode.read_mcp_resource` | Read one MCP resource by server + URI. |
| `tools.opencode.session_move` / `session_rename` | Move/rename the current session. |
| `tools.opencode.worktree_create` / `worktree_list` / `worktree_remove` | Manage git worktrees. |

## 2. Injected Capabilities

Two capability suites are **injected by the harness**, not configured in
`opencode.json`. They are discoverable through skills, agent definitions, and source
rather than through `opencode mcp list`.

### 2a. Graphos — knowledge-graph tooling

The Graphos project ships **three** cooperating layers, all injected (none appear in
`mcp.servers`):

#### (i) `graphos` CLI (documented capability)

A local knowledge-graph binary that turns code/docs/files into a navigable graph.
Referenced by the `graphos` skill and the `graphos-navigator` agent. Command surface:

```
graphos [PATH]                 # build the graph (default .)
  --output/-o DIR --directed --deep --no-viz --update
  --cluster-only --no-cluster --fresh --community-graph --embed --vision
  --neo4j --memgraph --svg --graphml --wiki
  --verbose/-v --debug --granularity fine|function|file
  --threads/-j N --edge-density MODE --resolution FLOAT --mcp GRAPH_JSON
  --ignore GLOB
graphos query QUESTION          # --dfs --budget N --json --label-width N --edges MODE
graphos cypher QUERY [--write]  # openCypher/GQL
graphos path FROM TO            # shortest path
graphos explain NODE
graphos symbols NAME
graphos neighbors NODE          # --depth N --json
graphos ingest FILE             # --embed --output/-o DIR
graphos subgraph                # --config CONFIG --out -o FILE --boundary-hops N
graphos init                    # generate graphos.yaml
graphos push | push-memgraph    # push to Neo4j / Memgraph
graphos merge A B
graphos serve [opts]            # HTML + query API
graphos lservers                # list LSP servers
```

- Graph file: `graphos-out/graph.json`; output dir: `graphos-out/` (never `graphify-out/`).
- The unrelated `graphify` (a separate Python program writing to `graphify-out/`) is
  **not** this tool and must not be used here.

> **Discovery caveat:** In this session the `graphos` binary is not on the effective
> `PATH` (only the paperclip github-runtime and nix paths resolved). The CLI is a
> *documented/injected* capability (skill + agent grant `bash: graphos *`), not an
> executable confirmed on this particular shell's PATH.

#### (ii) Graphos MCP server (registered tool interface)

A stdio JSON-RPC MCP server written in Haskell
(`src/Graphos/Infrastructure/Server/MCP.hs`, v2024-11-05 stdio). It exposes **13
tools** (all string-param only; responses are JSON):

| Tool | Purpose | Required params |
|------|---------|-----------------|
| `query_graph` | BFS/DFS knowledge-graph search (verdict, scored nodes/edges) | question |
| `cypher_query` | Read-only openCypher/GQL (`MATCH/WHERE/RETURN`) | query |
| `cypher_mutate` | Mutating openCypher (`CREATE/MERGE/SET/REMOVE/DELETE`); `persist` writes graph.json | query |
| `get_node` | Details of one node (rich LSP metadata) | node_id |
| `get_neighbors` | All neighbors of a node | node_id |
| `get_community` | Community membership for a node | node_id |
| `god_nodes` | Highest-degree hub nodes | top_n (opt) |
| `graph_stats` | Node/edge counts, avg degree | — |
| `shortest_path` | Shortest path between two concepts | from, to |
| `bridge_nodes` | Articulation points | — |
| `select_context` | Compact markdown context for an LLM query | question |
| `add_conversation` | Store a Q exchange in `graphos-out/memory/` | question |
| `conversation_history` | Search past exchanges | query |

`select_context` params: `budget`(3000), `include_history`, `verbose`, `edges`
(semantic|all), `max_hint_community_size`(50). See `docs/mcp-server.md` for the full
per-tool return schemas and the chat-history community design (community 0).

> **Registration caveat:** This MCP server is **not** wired into this OpenCode
> instance. `opencode mcp list` reports no configured servers and `opencode.json`
> has no `mcp` section — so the graphos MCP tools are an *available/harness-injected*
> interface, not a live connection in this instance.

#### (iii) `graphos-navigator` agent

An OpenCode agent definition (`.opencode/agent/graphos-navigator.md`, generated by
graphos 0.1.0.0) that routes graph work through the CLI:

```yaml
name: graphos-navigator
description: Navigate and maintain the Graphos knowledge graph.
permissions:
  bash:   { allow: [graphos *], deny: [] }
  edit:   false
  write:  false
```

Guardrails: use only `graphos`, prefer `--update --no-viz`, emit `--json`, never edit
source, never call `graphify`, and (per the agent + skill) do not call `graphos_*`
MCP tools in this environment.

### 2b. Paperclip — multi-agent orchestration (control plane)

Injected as a **skill** and backed by a local adapter working tree (`paperclip/`:
`agents/`, `docs/`, `projects/`, `scripts/`, `scratch/`). It is a board/issue
control plane that agents operate over HTTP + MCP tool gates.

- **Auth:** env vars auto-injected — `PAPERCLIP_AGENT_ID`, `PAPERCLIP_COMPANY_ID`,
  `PAPERCLIP_API_URL`, `PAPERCLIP_RUN_ID`, plus optional wake vars
  (`PAPERCLIP_TASK_ID`, `PAPERCLIP_WAKE_REASON`, `PAPERCLIP_APPROVAL_ID`, …).
  `Authorization: Bearer $PAPERCLIP_API_KEY`; all endpoints under `/api`.
  Content-bearing CLI args use `npx paperclipai` (never `pnpm paperclipai`).
- **Run audit trail:** every mutating request carries `X-Paperclip-Run-Id`.
- **Heartbeat model:** short execution windows; identity → assignments → pick work
  → checkout → context → work → update → delegate.
- **Issue fields:** status (`backlog/todo/in_progress/in_review/done/blocked/cancelled`),
  priority (`critical/high/medium/low`), `assigneeAgentId`, `projectId`, `goalId`,
  `parentId`, `blockedByIssueIds`.
- **Blockers:** first-class `blockedByIssueIds` (array replaces on update; no self
  blocks; `cancelled` does not count as resolved). Auto-wakes:
  `issue_blockers_resolved`, `issue_children_completed`.
- **Interactions:** `request_confirmation`, `request_checkbox_confirmation`,
  `request_item_verdicts`, `ask_user_questions`, `suggest_tasks`, plus standalone
  `decision` / `decision-bundles`.
- **Approvals:** `request_board_approval` → `approval_required` gate on MCP tools.
- **Work products (deliverables):** `pull_request`, `preview_url`, `runtime_service`,
  `commit`, `branch`; uploaded via `register_deliverable` / `scripts/paperclip-upload-artifact.sh`.
- **Other control-plane objects:** secrets (`secret-proposals`, `/agents/me/secrets`),
  routines (schedules/webhooks/API), cases (agent-facing cases API), monitors
  (`monitorNextCheckAt`), inboxes (`inbox-archive`).
- **Repo-specific rule:** this repo also enforces branch/PR hygiene — every run that
  commits to a branch must end by pushing + opening a PR, explicitly keeping the
  branch, or explicitly closing it.

## 3. MCP Servers

| Server | In `opencode.json`? | On PATH / running? | Notes |
|--------|--------------------|--------------------|-------|
| graphos (Haskell stdio) | no `mcp` section | not registered | Source at `src/Graphos/Infrastructure/Server/MCP.hs`; 13 tools |
| graphify (Python) | no | not present | Explicitly **not** this environment's tool |
| paperclip (adapter) | no | injected skill | Operates via HTTP + env, not a stdio MCP entry |

**Summary:** `opencode mcp list` → **"No MCP servers configured."** No `mcp` key
exists in `opencode.json`. The graphos and paperclip capabilities are injected through
the harness (skills, agents, source) rather than wired as configured MCP servers.

## 4. Skills

### Repo skills (`.opencode/skills/`)

| Skill | Role |
|-------|------|
| `graphos` | Navigate the knowledge graph (`query/path/explain/symbols/neighbors/ingest`); prefer graph over grep; `--update` to refresh; `graphos-out/graph.json` is the graph. |
| `openspec-new-change` | Start a new OpenSpec change. |
| `openspec-ff-change` | Fast-forward through OpenSpec artifact creation. |
| `openspec-continue-change` | Continue a change with the next artifact. |
| `openspec-propose` | Propose a change (all artifacts, one step). |
| `openspec-apply-change` | Implement tasks from an OpenSpec change. |
| `openspec-archive-change` | Archive a completed change. |
| `openspec-bulk-archive-change` | Archive multiple completed changes. |
| `openspec-verify-change` | Verify implementation matches artifacts. |
| `openspec-sync-specs` | Sync delta specs to main specs. |
| `openspec-onboard` | Guided OpenSpec onboarding. |

### Injected skill (harness)

| Skill | Role |
|-------|------|
| `paperclip` | Full multi-agent control-plane orchestration (heartbeat procedure, blockers, approvals, work products, monitors, secrets, routines, cases). |

## 5. Agents

| Agent | File | Mode | Permissions | Role |
|-------|------|------|-------------|------|
| `goal-orch` (Goal Orchestrator) | `.opencode/agent/core/goal-orch.md` | `primary` | `bash`: deny `rm -rf *`/`sudo *`; ask `curl/wget/docker/kubectl/cabal *`; `edit`: deny `.env*/*.key/*.secret`, `dist-newstyle/**`, `.git/**` | Autonomous goal loops: requirements clarification → manifest → DAG → dispatch workers → verify. Model: `ornith/ornith-1.5-35b-a3b`, temperature 0.1. |
| `graphos-navigator` | `.opencode/agent/graphos-navigator.md` | — | `bash`: allow `graphos *` (`deny: []`); `edit: false`, `write: false` | Graph work strictly through the `graphos` CLI; no source edits. |

The `.opencode/commands/goal.md` command dispatches to `goal-orch` with:
"Treat the following as the user's task and begin requirements clarification: $ARGUMENTS".

## Config Grounding (`opencode.json`)

- **Model / provider:** `ornith/ornith-1.5-35b-a3b`; provider `ornith` →
  `@opencode/ai/providers/openai-compatible`,
  `baseURL http://100.120.26.64:8087/v1`, 262k ctx / 8192 out,
  `reasoningField: reasoning_content`.
- **Secondary providers** (`provider` block): `ollama`, `llama-gpu`, `orchestrator`,
  `executor`, `lfm2-8b`, `gemma4` (all local `@ai-sdk/openai-compatible` endpoints).
- **Repositories:** `graphos` → `/home/jeremie/Documents/Graphos`; `homelab` →
  `/home/jeremie/Documents/homelab`.
- **Compaction:** auto + prune, `tail_turns: 3`, `reserved: 12000`.
- **Experimental:** `mcp_timeout: 60000`, `primary_tools: [edit, bash]`.
- **MCP:** none.

## Open Questions / Caveats

1. **Graph-MCP reference shape unavailable.** The intended mirror structure
   (LangChain `graph-mcp` docs — `mcp-reference.md` / `index.html`) returned 404 on
   both attempts, so the doc is grounded in OpenCode's own authoritative docs
   (`opencode.ai/v2/docs/tools`, `/v2/docs/mcp-servers`) plus this repo's ground truth.
2. **graphos CLI vs MCP vs binary.** The `graphos` CLI (skill/agent surface) is not
   confirmed on this session's PATH; the graphos **MCP server** (Haskell) is not wired
   into `opencode mcp list`. Both are injected capabilities, not live connections here.
3. **`docs/mcp-server.md` is slightly stale** relative to `MCP.hs`: it documents 11
   tools and omits `cypher_query` and `cypher_mutate`.
