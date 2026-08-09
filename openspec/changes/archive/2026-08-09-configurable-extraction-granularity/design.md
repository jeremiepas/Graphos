# Design: configurable-extraction-granularity

## Context

Tree-sitter extraction converts AST nodes to graph nodes via a single flat whitelist (`definitionTypes`, `Graphos.Infrastructure.Extract.TreeSitter.Convert`) and unconditionally recurses into every AST subtree. The whitelist mixes three tiers:

| Tier | Examples | Approx. share of 117k-node run |
|------|----------|-------------------------------|
| Structure | `module`, `program`, `source_file`, `package_clause` | ~1% |
| API surface | function/method/class/interface/type/enum/trait/struct declarations, fields, imports/exports | ~15% |
| Implementation detail | `expression_statement`, `return_statement`, `if/for/while/try`, `parameter`, `variable_declarator`, JSON `object`/`array`/`pair` | ~85% |

Labels for tier-3 nodes are raw source text fragments, making them noise for community detection, context selection (PRD §7), and MCP answers (PRD §8). Config already supports per-extension extractor settings (`ExtractorConfig` in `Graphos.Domain.Config`, Domain layer), so a granularity knob fits the existing shape.

## Goals / Non-Goals

**Goals:**
- Three granularity levels (`fine` / `function` / `file`) selectable globally, per extension, and per run.
- `function` default: ≤ ~25 nodes/file on this repo's 982-file corpus; module-level variables and class fields retained; function bodies not descended.
- `.json` files default to `file` level (1 node per file).
- `fine` reproduces today's output exactly (escape hatch, no data migration).
- Pure, testable decision logic; node schema unchanged.

**Non-Goals:**
- Leiden performance fixes (separate change: quadratic vector copies).
- Cross-file import resolution for the tree-sitter path.
- Granularity control for LSP or Haskell-stub extraction paths.
- New node kinds or schema changes.

## Decisions

### D1 — `Granularity` enum in Domain.Config (Domain layer)

A three-constructor sum type (`GranularityFine | GranularityFunction | GranularityFile`) with Aeson instances (serialized as `fine` / `function` / `file`), living beside `ExtractorMode` in `Graphos.Domain.Config`. Pure data, no IO — respects the Domain-has-zero-IO rule.

- **Alternatives considered:**
  - *Boolean flag (`statementLevel: true/false`)* — rejected: the `file` level is nearly free, solves JSON/vendored-dir inflation, and a boolean would need replacing the first time a third level is wanted.
  - *Numeric depth limit* — rejected: AST depth is grammar-specific and meaningless to users; named levels map to intent.

### D2 — Resolution order: CLI → per-extension → global → default

`ExtractorConfig` gains an optional granularity field; `GraphosConfig` gains a global extraction default; `app/Main.hs` gains `--granularity`. Effective granularity is resolved in UseCase (`Graphos.UseCase.Extract`) before invoking the converter, most specific source winning.

- **Alternatives considered:**
  - *Global-only knob* — rejected: JSON needs `file` while TypeScript needs `function` in the same run; per-extension override is the whole point for data files.
  - *Per-directory overrides* — rejected for now: `.graphosignore` already handles vendored dirs; revisit if demanded.
- **Layering:** resolution is a pure function (config values in, level out) — unit-testable without IO; only the CLI parse is Infrastructure/app.

### D3 — Tiered whitelist + recursion stop at function boundaries (Infrastructure)

`definitionTypes` splits into named tiers (structure / API surface / implementation detail). Level behavior:

| Level | Node whitelist | Recursion rule |
|-------|----------------|----------------|
| `fine` | all three tiers | full tree walk (today) |
| `function` | structure + API surface | stop descending when the current node is function-kind (function/method/arrow/constructor) |
| `file` | structure only | do not descend below root |

The recursion stop — not just the whitelist — is what removes locals and parameters robustly across grammars: anything inside a function body is invisible regardless of its AST type, so per-language whitelist drift cannot re-introduce statement noise at `function` level. Module-level `variable_declaration`/`lexical_declaration` remain visible because they sit outside function bodies; they are included in the API-surface tier.

- **Alternatives considered:**
  - *Whitelist trim only (no recursion stop)* — rejected: `variable_declarator` at module scope and inside functions are the same AST type; without a boundary rule you either lose module constants or keep locals.
  - *Post-extraction filtering in UseCase.Build* — rejected: wasted work (extract then discard) and the parent-contains-child edge structure would need re-stitching.

### D4 — Default `function`; `.json` default `file`

`defaultGraphosConfig` sets the global level to `function`; the `.json` entry in `defaultExtractors` carries a `file` override. Documented as a behavior change in CHANGELOG; `fine` restores previous output with one config line.

- **Alternatives considered:**
  - *Default `fine` for backward compatibility* — rejected: the default violated PRD §16.1 on a mid-size repo; defaults should be the recommended configuration, and no persisted data breaks (graphs are regenerated outputs).
  - *Special-case JSON in code* — rejected: per-extension config (D2) already expresses it declaratively.

### D5 — Threading granularity to the converter (UseCase → Infrastructure)

`tsNodesToExtraction` (and the node/edge walkers) take the resolved level as a parameter. `Graphos.UseCase.Extract` resolves the level per file (D2) and passes it down. No global state, no config reads inside the converter — conversion stays a pure function of (path, AST, level).

- **Alternatives considered:** reading config inside Convert — rejected: breaks purity of the conversion layer and makes fixtures awkward.

## Risks / Trade-offs

- [Default change surprises existing users] → CHANGELOG entry + log line at extraction start stating the active granularity; `fine` is a one-line rollback.
- [Grammar-specific function node types missed by the boundary rule] → the stop-set (function-kind types) is derived from the existing `tsTypeToKind` Function/Method/Constructor mappings; fixtures per language (TS, Python, Rust, JSON) guard it. Act step audits leaks if nodes/file stays high.
- [`function` level hides genuinely useful detail for some users] → `fine` remains fully supported; per-extension override allows mixing levels in one run.
- [Markdown path uses the tree-sitter mode flag but a built-in parser] → verify the markdown extractor ignores the level or handles it sensibly (headers are structure, keep them); covered by a fixture.
- [Node-count assertions in existing tests may assume fine-level output] → run full suite; fix fixtures to pin an explicit level rather than relying on the default.

## Verification Strategy (Check)

- **Unit (Hspec, `cabal test`):**
  - Resolution order: CLI beats per-extension beats global beats default (pure function tests).
  - Converter fixtures: one TS-like AST fixture asserted at all three levels — `fine` includes statement nodes; `function` includes module/functions/class/fields/module-consts and excludes anything inside a function body; `file` yields exactly the root node.
  - JSON fixture: `file` level yields 1 node.
  - Aeson round-trip for the enum (`fine`/`function`/`file` strings).
- **Integration (`cabal run graphos -- .`):** run at each level on this repo; assert `function` node count ≪ `fine` node count; `scripts/audit_graph.py` passes at `function`.
- **Build gate:** `cabal build` clean with dev `-Wall -Werror` flags.
- **Target check (PRD §16.1):** on the 982-file corpus, `function` level total nodes ≤ ~25k and pipeline wall-clock materially below the `fine` baseline (recorded in Check notes).

## Iteration & Rollback (Act)

- **If `function` still yields > 25 nodes/file:** audit emitted kinds distribution (audit script extension), move leaking types out of the API-surface tier, re-run.
- **Rollback:** set `extraction.granularity: fine` (config) or revert commits; outputs are regenerated artifacts, no stored-data migration in either direction.
- **Standardize:** document the three levels and the resolution order in PRD §14 (configuration) on archive; record the observed node/file ratios per level as reference numbers; hand remaining clustering slowness to `fix-leiden-scalability`.
