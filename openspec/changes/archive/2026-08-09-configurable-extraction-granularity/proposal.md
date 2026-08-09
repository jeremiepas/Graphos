# Proposal: configurable-extraction-granularity

## Why

A 982-code-file run produced **117,161 nodes / 138,472 edges** (~119 nodes per file) and community detection ran for minutes, violating the PRD §16.1 target (Leiden @ 100k nodes < 30s). The cause is tree-sitter extraction granularity (PRD §3.2, §11): `definitionTypes` in `Graphos.Infrastructure.Extract.TreeSitter.Convert` whitelists statement-level AST nodes — `expression_statement`, `return_statement`, `if/for/while/try` statements, `parameter`, `variable_declarator`, and JSON `object`/`array`/`pair` — and the converter recurses into function bodies unconditionally.

Consequences:
- **~85% of nodes are implementation detail**, not knowledge: statement nodes labeled with raw source text (`user = dbGetById(id)`), parameters, local variables, and one node per key-value pair of every JSON file.
- **Everything downstream pays**: Leiden clustering (quadratic hotspots amplify at 117k), `graph.html` size, edge inference, Neo4j push, and MCP query relevance (PRD §7, §8).
- **No user control**: granularity is hard-coded; large repos cannot trade detail for speed.

## What Changes

- A new **`Granularity` config enum** with three levels:
  - `fine` — current behavior (all `definitionTypes`), for small repos and debugging.
  - `function` — module + functions/methods/classes/interfaces/types/enums/traits/fields/imports/exports + module-level variables; extraction **stops recursing at function bodies**. Expected ~15–25 nodes/file.
  - `file` — one module node per file, for vendored dirs and data files.
- **Resolution order** (most specific wins): CLI flag `--granularity` → per-extension `ExtractorConfig` override → global `extraction.granularity` in config → built-in default.
- **Default changes to `function`** (behavior change, documented in CHANGELOG; `fine` restores today's output with one config line).
- **`.json` defaults to `file` granularity** in `defaultExtractors`, eliminating thousands of `pair` nodes from lock files and config JSON.

Out of scope: Leiden algorithmic fixes (O(n²) vector copies — separate change), tree-sitter cross-file import resolution, LSP extraction granularity.

## Capabilities

### New Capabilities
- `extraction-granularity`: configurable node granularity for tree-sitter extraction with fine/function/file levels, per-extension override, and CLI flag (workflows: 01-full-pipeline, 02-incremental-pipeline).

### Modified Capabilities
<!-- none — existing specs (node-schema, extraction-quality) are unaffected: node shape is unchanged, only which nodes are emitted -->

## Impact

- **Code**: `src/Graphos/Domain/Config.hs` (Granularity type, `ExtractorConfig` field, global extraction section, defaults), `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs` (tiered `definitionTypes`, recursion stop), `src/Graphos/UseCase/Extract.hs` (thread granularity to converter), `app/Main.hs` (CLI flag), config YAML template.
- **Behavior**: default output for tree-sitter-extracted files shrinks ~6×; node schema unchanged; LSP/stub extraction paths untouched.
- **Docs**: CHANGELOG entry for the default change; config reference update (PRD §14).
- **Tests**: Hspec fixtures per granularity level; resolution-order unit tests.

## PDCA Cycle

- **Plan**: Hypothesis — statement-level nodes are the dominant cause of node inflation and downstream slowness. Success criteria (PRD §16.1): on a 982-file repo, `function` granularity yields ≤ 25 nodes/file (~25k total) and end-to-end pipeline (including clustering) completes in minutes → target < 2 min; `.json` files emit exactly 1 node at `file` level; `fine` reproduces current counts.
- **Do**: Implement the enum, resolution order, tiered whitelist, recursion stop, CLI flag, and `.json` default (see design.md, tasks.md).
- **Check**: `cabal test` for level fixtures and resolution order; full pipeline run on this repo at each level comparing node counts and wall-clock; `scripts/audit_graph.py` still passes at `function` level.
- **Act**: If `function`-level counts remain > 25 nodes/file, audit which types leak through and adjust the tier; standardize granularity guidance in PRD §14; feed remaining clustering slowness into the separate `fix-leiden-scalability` change.
