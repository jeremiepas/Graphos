# Tasks — Documentation Overhaul

Ordering rule: the drift guard (section 1) lands **before** any doc is rewritten — the check
criteria exist first, the current tree fails them (proving they bite), and the restructure turns
them green. Sections 2–4 are parallel-safe with respect to each other once section 1 is merged.

## 1. Drift guard (DocsSpec) — checks first, red on the current tree

### 1.1 Markdown link + anchor checker
- [ ] Add `tests/DocsSpec.hs`: pure extractor for relative links `[t](path)` / `[t](path#anchor)`
      and heading→anchor slugger (GitHub rules: lowercase, spaces→`-`, strip punctuation)
- [ ] Walk `README.md`, `CONTRIBUTING.md`, `docs/**/*.md`; assert every relative link resolves to
      an existing file and every anchor to an existing heading; failure message = source file +
      target + line
- [ ] Register the spec in `graphos.cabal` test suite; `-Wall -Werror` clean
- [ ] Check criterion: on the current tree the suite fails naming (at least)
      `docs/workflows/README.md → 14-observability.md / 15-config-init.md / 16-lsp-discovery.md`
      and `CONTRIBUTING.md → README.md#language-server-requirements`

### 1.2 Parity assertions against compiled truth
- [ ] Export (or confirm importable) the MCP tool name list from
      `src/Graphos/Infrastructure/Server/MCP.hs` (`allTools` names) and a subcommand-name list
      from `src/Graphos/CLI/Parser.hs`
- [ ] Assert workflow index table rows ↔ `docs/workflows/NN-*.md` files, 1:1 both directions
- [ ] Assert MCP tool names in the MCP reference doc are set-equal to `allTools` names (13 today)
- [ ] Assert every parser subcommand name appears in the CLI reference doc
- [ ] Assert `README.md` has exactly one "LSP security notes" heading and no duplicate headings;
      assert `CONTRIBUTING.md` links `GITFLOW.md`, has no `develop`-flow prescription, and links
      the release policy
- [ ] Check criterion: all parity assertions fail on the current tree (11-vs-13 tool count,
      missing `ingest`/`research`/`clustereval`/`install-skill`, duplicated README section,
      `develop` narrative present); point them at the future doc paths behind a small path table
      so sections 2–4 flip them green without test edits

## 2. Docs tree restructure (Diátaxis)

### 2.1 Index + directory moves
- [ ] Create `docs/README.md` index: four quadrant sections + "Formal foundations" entry for
      `docs/math/` + `docs/math-requirements/` + link to `docs/workflows/`
- [ ] `git mv` into `docs/reference/`: `mcp-server.md → mcp-tools.md`,
      `release-policy.md → release-policy.md`
- [ ] `git mv` into `docs/explanation/`: `INFRASTRUCTURE_SCHEMA.md → architecture.md`,
      `flow-diagram.md`, `decision-tree.md`, `embedding-models.md`
- [ ] `git mv` into `docs/internal/`: `brainstorm-context-optimization.md`, `pdca/`,
      `openspec-state-report.md`; add snapshot banner (generation date, source commit, refresh
      command) to the state report
- [ ] Repo-wide link sweep for moved paths (`grep -rn` old names across repo incl. `openspec/`)
- [ ] Check criterion: DocsSpec index + link checks green for `docs/`

### 2.2 Workflow index rebuild
- [ ] Rebuild `docs/workflows/README.md` table 1:1 against the 18 files on disk (names, links,
      numbering); fix row 08 tool count (13); redraw or simplify the dependency map to match
- [ ] Check criterion: DocsSpec workflow-parity assertion green

### 2.3 New reference pages
- [ ] `docs/reference/cli.md`: every parser subcommand (22 today) with one-line description and
      key flags; move README "CLI Reference" + "Query Options" content here
- [ ] `docs/reference/supported-file-types.md`: move README tier tables (code + non-code) here
      verbatim-honest, keeping the stub/placeholder caveats
- [ ] `docs/reference/graph-json.md`: short page — JGF envelope, `metadata.graphos` fields,
      `migrate-graph`, pointer to `openspec/specs/graph-json-contract/`
- [ ] Check criterion: DocsSpec CLI-coverage + MCP-parity assertions green

### 2.4 Tutorial
- [ ] `docs/tutorials/getting-started.md`: install → run `graphos .` on a small sample repo →
      tour `graphos-out/` (graph.json, GRAPH_REPORT.md, graph.html) → first `graphos query` →
      where to go next (one path, no alternatives, no theory — link out for why)
- [ ] Check criterion: a newcomer can complete it with only `cabal install graphos`; links green

## 3. README rewrite (front door)

### 3.1 Rewrite README.md
- [ ] Rewrite to ~150 lines: identity/tagline, what-it-does paragraph, compact capability
      summary (tree-sitter-first + honest caveat line, linking the file-type reference), install,
      quickstart, `graphos-out/` tour, **one** LSP security note, links table into the four
      quadrants + workflows index, license
- [ ] Delete duplicated "LSP security notes" and the content moved to `docs/reference/`
- [ ] Check criterion: DocsSpec README checks green (single security-notes heading, links
      resolve); no CLI subcommand of the parser absent from README+linked reference

## 4. Governance reconciliation

### 4.1 CONTRIBUTING.md
- [ ] Replace the GitFlow/`develop` release narrative with: link to `GITFLOW.md` (branch model)
      and `docs/reference/release-policy.md` (release process); keep dev-env, commit convention,
      OpenSpec process, PR expectations, triage, security pointers
- [ ] Fix `README.md#language-server-requirements` anchor to the new LSP reference location
- [ ] Fold CONTRIBUTING's versioning/tagging detail into `docs/reference/release-policy.md`
      (single source), deduplicating against its current content
- [ ] Check criterion: DocsSpec governance assertions green; GITFLOW.md and AGENTS.md untouched
      and non-contradicted

### 4.2 openspec/config.yaml refresh
- [ ] Context block: tree-sitter-first solution + key decision, 13 MCP tools, devenv (not
      `shell.nix`), workflows 01–18, output dir list current
- [ ] Rules: drop the PRD.md section-number requirement (or re-point it at
      `openspec/config.yaml` context + workflow docs); fix the capability↔workflow rule to the
      actual 01–18 list
- [ ] Check criterion: reviewer sign-off on rule text (governance change); `openspec validate
      --changes` still passes for all active changes

## 5. Close-out

### 5.1 Full verification
- [ ] `cabal build` and full `cabal test` green under `-Wall -Werror` (skip
      BoundedDeltaCommunitySpec random-graph property per known hang)
- [ ] `openspec validate documentation-overhaul --strict` passes
- [ ] Render README + docs/README.md on GitHub UI; spot-check quadrant links

### 5.2 Changelog + archive prep
- [ ] `CHANGELOG.md` Unreleased entry: docs restructure, README rewrite, DocsSpec drift guard,
      governance single-sourcing
- [ ] Confirm no stale references to pre-move paths remain (`grep -rn` the old filenames)
