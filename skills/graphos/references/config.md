# Graphos configuration reference

Read this before creating or editing a `graphos.yaml`, or when a setting "has no effect".

## Where config comes from (later wins)

1. Built-in defaults
2. Global: `$XDG_CONFIG_HOME/graphos/graphos.yaml` (usually `~/.config/graphos/graphos.yaml`)
3. Project: `graphos.yaml` in the directory being built
4. CLI flags

`graphos init` writes a commented project file. Startup prints `[config] Global: <path>` when the global file is loaded. Templates matching the shared setup are in [`../config/`](../config/).

## Key spelling: the most common silent failure

Unknown keys are **ignored without a warning**, and casing differs by section:

| Section | Key style | Accepted keys |
|---|---|---|
| top level | snake_case | `granularity`, `extractors`, `lsp`, `language_ids`, `file_extensions`, `semantic_edges`, `detection`, `memory`, `cache`, `sources`, `ingest`, `output`, `pdf_extraction` |
| `neo4j`, `memgraph` | **camelCase** | `uri`, `user`, `password`, `pushMode`, `subgraphSize` |
| `labeling` | **camelCase** | `provider`, `model`, `apiKey`, `baseUrl`, `batchSize`, `headers` |
| `embedding` | **camelCase** | `enabled`, `provider`, `model`, `baseUrl`, `dimension`, `batchSize`, `concurrency`, `maxTokens`, `docPrefix`, `queryPrefix`, `streaming`, `headers` |
| `vision` | **camelCase** | `enabled`, `model`, `apiKey`, `baseUrl`, `maxTokens`, `batchSize`, `headers` |
| `observability` | **camelCase** | `enabled`, `endpoint`, `metricsPort`, `serviceName`, `serviceVersion`, `exportInterval`, `debugTraceDir`, `debug` |
| `semantic_edges` | snake_case | `enabled`, `max_fan_out`, `threshold`, `min_ident_length`, `path_extensions` |
| `detection` | snake_case | `mode`, `generator_signatures`, `vendor_segments`, `minified_line_threshold`, `leading_lines` |
| `memory` | — | `budget`: `auto` \| `off` \| size (`8G`, `512M`; bare number = MB) |
| `cache` | snake_case | `max_mb` (default 512) |
| `extractors.<ext>` | snake_case | `mode` (`tree-sitter` \| `lsp` \| `stub`), `grammar`, `language_id`, `granularity` |
| `lsp.<ext>` | snake_case | `command`, `args`, `language_id` (`command: ""` disables) |

`neo4j.pushMode` is a plain string in the config file (`full` | `subgraph` | `community`, default `subgraph`), while the CLI flags `--neo4j-push-mode` / `push --mode` expect constructor names (`FullPush`, `SubgraphPush`, `CommunityPush`).

So `push_mode`, `base_url`, `batch_size`, `api_key` and `max_tokens` do nothing in the camelCase sections. Even `graphos init` comments and older config files contain the snake_case spellings.

## Secrets and environment variables

- `labeling.apiKey` expands `${VAR}` from the environment (e.g. `"${OPENAI_API_KEY}"`). Prefer this to a literal key.
- `vision.apiKey`: an empty value inherits from `labeling`. The literal `"${OPENAI_API_KEY}"` is also recognised.
- `neo4j.password` is **not** expanded: it is used literally. The built-in default `graphos_dev` matches the repo's `docker-compose.yml` local stack. Use a real password only in a personal, untracked config, never in a committed project `graphos.yaml`.
- `sources[].path` expands `~` and `${VAR}`.

## Settings that matter for analysis

| Setting | Recommended | Why |
|---|---|---|
| `granularity` | `function` (default) | `file` keeps one node per file and drops symbols; `fine` is statement-level and very large. CLI `--granularity` overrides. Per extension: `extractors.".json".granularity: file`. |
| `extractors.*.mode` | `tree-sitter` | Fast, needs no language server. `lsp` gives richer semantics but needs the server installed (`graphos lservers`). |
| `semantic_edges.enabled` | leave on; pass `--no-semantic-edges` for dependency analysis | Inferred code↔doc edges add noise to import graphs. |
| `memory.budget` | `auto` | Graphos derives a heap cap from available RAM and re-execs itself with it. `--max-heap` / `--no-memory-budget` override. |
| `detection.mode` | `exclude` (default) | Drops generated, vendored and minified files. `collapse` keeps one node per such file; `off` ingests everything. |
| `observability.enabled` | `false` | Tracing/metrics need the OTEL stack from `docker-compose.otel.yaml`. `--no-observability` forces it off. |
| `embedding.enabled` | `false` unless an OpenAI-compatible `/v1/embeddings` server runs at `baseUrl` | Otherwise builds stall or fail on the embedding step. `--embed` forces it on. |
| `labeling` | only used by `--label` | Community labels via any OpenAI-compatible API (OpenAI, Ollama, LiteLLM). |

## `.graphosignore`

Gitignore syntax, read from the build root, and it takes precedence over `.gitignore`. Use it to keep the graph focused: build output, nested worktrees or repo copies (each one duplicates the codebase), agent/tool runtime state, generated specs, and graphos's own `graphos-out/`. `--ignore GLOB` adds patterns for a single run. A template is in [`../config/graphosignore.example`](../config/graphosignore.example).

Project `graphos.yaml` and `.graphosignore` are often kept **untracked**. A fresh clone or `git worktree` then has neither and silently falls back to defaults: copy them in before building.
