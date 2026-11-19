# Documentation Overhaul — Diátaxis restructure + drift guard

## Why

The documentation has drifted from the code and from itself. Concrete, verified today:

1. **Stale facts.** `Infrastructure/Server/MCP.hs` exports **13 tools**; `docs/workflows/README.md`
   (row 08) and `openspec/config.yaml` still say **11**. `config.yaml`'s context block still states
   "LSP over tree-sitter" as the key decision and `nix-shell shell.nix` as setup, while the README
   (correctly) says tree-sitter-first and the repo uses `devenv` — `shell.nix` no longer exists.
2. **Broken structure.** The workflow index table links `14-observability.md`, `15-config-init.md`,
   `16-lsp-discovery.md` — none exist on disk; the real files `14-symbols.md`, `15-neighbors.md`,
   `16-agent-scaffolding.md` are not indexed at all. `CONTRIBUTING.md` links
   `README.md#language-server-requirements`, an anchor that no longer resolves.
3. **Contradictory governance.** `CONTRIBUTING.md` prescribes GitFlow with a `develop` integration
   branch; `GITFLOW.md` and `AGENTS.md` (trunk-based "Option A — accepted plan") say `develop` is
   retired and must not be targeted. The release process is described twice (CONTRIBUTING.md
   §Release process and `docs/release-policy.md`) with no single owner.
4. **No shape.** `docs/` is flat and unindexed: audience-facing reference (`mcp-server.md`),
   explanation (`flow-diagram.md`, `INFRASTRUCTURE_SCHEMA.md`), internal working notes
   (`brainstorm-context-optimization.md`, `pdca/`), and a dated point-in-time snapshot
   (`openspec-state-report.md`, generated 2026-10-07, now stale) sit side by side with nothing
   telling a reader which is which. README.md is a 385-line mega-doc mixing all four Diátaxis
   quadrants, duplicates its "LSP security notes" section verbatim, and omits real subcommands
   (`ingest`, `research`, `clustereval`, `install-skill`) from its CLI reference.

Every one of these rotted silently because **nothing executable checks the docs**. A rewrite
without a guard reproduces the same decay; this change therefore pairs the restructure with a
`cabal test`-enforced drift suite.

> Note on rules: `openspec/config.yaml` requires proposals to cite PRD.md section numbers, but no
> PRD.md exists in the repo, and its capability↔workflow rule cites "01…16-lsp-discovery" while
> `docs/workflows/` now spans 01–18 with different names. This change fixes those stale rule
> references (task 5.2); capabilities here reference the workflow docs and config context directly.

## What Changes

- **Restructure `docs/` along Diátaxis** with a `docs/README.md` index as the single entry point:
  - *Tutorials*: new `docs/tutorials/getting-started.md` (install → run on a sample repo → read
    `graphos-out/` → first query). None exists today.
  - *How-to / workflows*: `docs/workflows/` stays, with its index table rebuilt 1:1 against the 18
    files on disk (names, links, tool counts corrected).
  - *Reference*: `docs/reference/` — MCP tools (from `mcp-server.md`), CLI surface (all 22 parser
    subcommands), supported file types (moved out of README), `graph.json`/JGF contract pointer,
    release policy (single-sourced from `docs/release-policy.md`).
  - *Explanation*: `docs/explanation/` — architecture (`INFRASTRUCTURE_SCHEMA.md`,
    `flow-diagram.md`, `decision-tree.md`), tree-sitter vs LSP rationale, `embedding-models.md`;
    `docs/math/` and `docs/math-requirements/` stay in place but are indexed as formal foundations.
  - *Internal*: `docs/internal/` — `brainstorm-context-optimization.md`, `pdca/`, and
    `openspec-state-report.md` (kept with an explicit "point-in-time snapshot" banner + refresh
    command), excluded from the audience index.
- **Rewrite `README.md` as the front door** (~150 lines): what/why, install, quickstart, honest
  capability summary, output tour, and links into `docs/` — exactly one LSP security note, no
  duplicated sections, CLI coverage complete via the reference page.
- **Reconcile governance docs**: `GITFLOW.md` becomes the sole owner of the branch/merge model
  (trunk-based, per AGENTS.md Option A); `CONTRIBUTING.md` defers to it and drops the
  `develop`-based narrative; release process single-sourced in `docs/reference/release-policy.md`
  with CONTRIBUTING linking to it; broken anchors fixed.
- **Add a docs drift guard** (`tests/DocsSpec.hs`, runs in `cabal test`): relative-link + anchor
  integrity across README/CONTRIBUTING/docs; workflow index ↔ files-on-disk parity; documented MCP
  tool names ↔ `allTools`; documented CLI subcommands ↔ parser command list.
- **Refresh `openspec/config.yaml`**: context block corrected (tree-sitter-first, 13 MCP tools,
  devenv, workflows 01–18) and stale rule references fixed (PRD.md, 1:1 workflow list).
- `CHANGELOG.md` entry under Unreleased.

## Capabilities

### New Capabilities

- `documentation`: Diátaxis-structured documentation tree with a single index, an accurate
  front-door README, single-sourced governance docs, and a `cabal test`-enforced drift guard
  (link integrity, workflow-index parity, MCP tool parity, CLI coverage).

### Modified Capabilities

- None. No runtime behaviour changes; the only code added is test code plus (at most) exporting
  existing name lists (`allTools` names, parser subcommand names) for the test suite to consume.

## Impact

- **Docs**: `README.md`, `CONTRIBUTING.md`, `GITFLOW.md`, `docs/**` (moves via `git mv`, new
  index, new tutorial, new reference pages). `AGENTS.md`, `SECURITY.md`, `CODE_OF_CONDUCT.md`
  unchanged except link fixes if needed.
- **Code**: `tests/DocsSpec.hs` (new); small export additions in
  `src/Graphos/Infrastructure/Server/MCP.hs` and `src/Graphos/CLI/Parser.hs` if the name lists are
  not already importable. No Domain/UseCase change.
- **APIs**: none. No `graph.json` schema change, no CLI change.
- **Dependencies**: none new (markdown link extraction done with plain `Text` parsing in test code).
- **Tests**: new `DocsSpec` assertions as above; suite stays green under `-Wall -Werror`.
- **Governance**: `openspec/config.yaml` context + rules corrected (owner sign-off on the rule text
  is part of review).

## Relationship to other changes

- **`openspec-view` / `graphos view dashboard`**: the retired-in-place
  `docs/internal/openspec-state-report.md` snapshot names `graphos view` as its refresh command;
  no code dependency.
- **Active changes touching docs** (`multi-source-graphs`, `intent-graph-verification`, …): they
  edit workflow/spec files, not the docs tree layout; merge-order conflicts limited to
  `docs/workflows/README.md` row text — trivial rebases. The drift guard will protect their future
  doc edits rather than conflict with them.
