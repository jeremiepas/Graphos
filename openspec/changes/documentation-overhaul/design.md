# Design — documentation-overhaul

## Context

Documentation spans four audiences (new user, task-driven user, fact-checker, architecture
reader) and three governance surfaces (CONTRIBUTING, GITFLOW, AGENTS). Today all of it is
hand-maintained with no executable verification, and it has measurably diverged from the code
(see proposal §Why). This design fixes the structure once and makes the divergence class
impossible to reintroduce silently.

Clean-architecture framing: this change touches **no Domain or UseCase code**. The only code is
test code (`tests/DocsSpec.hs`) plus, if needed, re-exports of two existing name lists from
Infrastructure (`MCP.allTools`) and CLI (`Parser` subcommand names). Tests may import any layer,
so no dependency rule is bent.

## Decision 1 — Diátaxis as the organizing principle for `docs/`

| Quadrant | Location | Contents after change |
|---|---|---|
| Tutorial | `docs/tutorials/` | `getting-started.md` (new) |
| How-to | `docs/workflows/` | existing 01–18, index rebuilt |
| Reference | `docs/reference/` | `mcp-tools.md`, `cli.md`, `supported-file-types.md`, `release-policy.md`, `graph-json.md` (pointer to JGF contract spec) |
| Explanation | `docs/explanation/` | `architecture.md` (from INFRASTRUCTURE_SCHEMA), `flow-diagram.md`, `decision-tree.md`, `embedding-models.md`, `extraction-tradeoffs.md` (tree-sitter vs LSP) |
| (internal) | `docs/internal/` | `brainstorm-context-optimization.md`, `pdca/`, `openspec-state-report.md` |

`docs/math/` and `docs/math-requirements/` stay at their current paths (they are referenced by
issue history and the Lean formalization work) and are indexed from `docs/README.md` under
"Formal foundations"; moving them is explicitly out of scope.

**Alternatives considered**

| Alternative | Why rejected |
|---|---|
| Keep flat `docs/` with only an index | Index alone doesn't stop quadrant-mixing; new files land uncategorized again |
| MkDocs/Material site | Build infra + theme maintenance for a repo whose readers live in the terminal/GitHub; can layer on later without undoing this structure |
| Move everything including math-requirements | Breaks issue-history links for no reader benefit; high churn, zero drift reduction |

## Decision 2 — Drift guard as an Hspec suite inside `cabal test`

`tests/DocsSpec.hs` asserts, from the repo root:

| Check | Source of truth | Documented surface checked |
|---|---|---|
| Link integrity | filesystem | every relative link + `#anchor` in `README.md`, `CONTRIBUTING.md`, `docs/**/*.md` resolves |
| Workflow index parity | `docs/workflows/*.md` on disk | index table rows ↔ files, 1:1 both directions |
| MCP tool parity | `MCP.allTools` names | tool names listed in `docs/reference/mcp-tools.md`, set-equal |
| CLI coverage | parser subcommand names | each subcommand appears in `docs/reference/cli.md` |
| Single-sourcing | — | `CONTRIBUTING.md` links `GITFLOW.md` and contains no `develop`-flow prescription; README contains exactly one "LSP security notes" heading |

Anchor resolution uses GitHub's slugging rules (lowercase, spaces→`-`, strip punctuation),
implemented in ~30 lines of pure `Text` code — no new dependency.

**Alternatives considered**

| Alternative | Why rejected |
|---|---|
| CI-only link checker (lychee/markdownlint job) | Not runnable offline via `cabal test`; and external tools can't compare docs against compiled truth (`allTools`, parser) — the parity checks are the point |
| Pre-commit hook | Opt-in per clone; silently skipped is the failure mode we're eliminating |
| Generate docs from code (literate/golden files) | Highest fidelity but large machinery; parity assertions give 90% of the protection at 5% of the cost. Can evolve toward generation later |

markdownlint via CI remains a nice-to-have on top; it is not the enforcement mechanism.

## Decision 3 — README as front door, depth lives in `docs/`

README keeps: identity + tagline, honest capability summary (short form of the file-type tiers),
install, quickstart, `graphos-out/` tour, one LSP security note, and a links table into the four
quadrants. Everything else (full file-type tier table, full CLI reference, MCP tool list,
tuning flags) moves to `docs/reference/`.

Alternative — keep the 385-line mega-README: rejected; it is where the duplication and omissions
accumulated, because one page serving four quadrants has no criterion for what belongs.

## Decision 4 — Governance single-sourcing

`GITFLOW.md` already states the accepted trunk-based model (Option A) and matches `AGENTS.md`.
It becomes the sole normative description of branching/merge authority. `CONTRIBUTING.md` keeps
contributor-facing practicalities (dev env, commit convention, PR expectations, OpenSpec
process) and links to `GITFLOW.md` for the branch model and to
`docs/reference/release-policy.md` for releases; its GitFlow/`develop` release narrative is
deleted, not annotated — a contradicted instruction marked "historical" still gets followed.

Alternative — rewrite GITFLOW.md into CONTRIBUTING and delete GITFLOW.md: rejected; AGENTS.md
and agent tooling reference GITFLOW.md as the merge-perspective rule set, and mixing
agent-facing merge authority into the human contributor guide re-creates the two-audience blur.

## Decision 5 — Snapshots are quarantined, not deleted

`docs/openspec-state-report.md` moves to `docs/internal/` with a banner stating generation date,
source commit, and the refresh command. Deleting it loses a useful worked example of the
dashboard output; leaving it in the audience tree presents 2026-10-07 state as current.

## Verification strategy

- `cabal build` clean under `-Wall -Werror` (test-only code paths included).
- `cabal test --test-options='--match Docs'` red on the current tree (proves the guard bites on
  the known breakages), green after the restructure; full `cabal test` green.
- `openspec validate documentation-overhaul --strict` passes.
- Manual: render README + `docs/README.md` on the GitHub UI and spot-check the quadrant links.
