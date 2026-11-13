## Context

Graphos currently supports compiled-in tree-sitter grammars for TypeScript, Python, Go, Rust, Haskell, Ruby, Java, C/C++, YAML, SQL, JSON, and Markdown. Four qualification languages (PHP, Kotlin, Perl, C#) have no grammar binding, so their files collapse to stub nodes and produce no symbols. Java parses but its import statements are not collected by `Imports.hs`, so no `imports` edges are emitted.

The base Haskell `tree-sitter` package pins the runtime to ABI 9–11, so every new grammar must either ship as a Hackage package compiled against that ABI or be vendored at a compatible revision, per `vendor/README.md`. New language AST node type names must be discovered by parsing representative files before adding them to `Convert.hs` whitelists.

## Goals / Non-Goals

**Goals:**
- Make PHP, Kotlin, Perl, and C# produce file/module, function, class/type, and import nodes in default tree-sitter mode.
- Make `getGrammarPtr` succeed for `php`, `kotlin`, `perl`, and `csharp`.
- Emit `imports` edges for Java files in repositories that contain the imported source files.
- Keep the change backward compatible with existing `graphos.yaml` configs.

**Non-Goals:**
- Runtime loading of arbitrary grammar `.so` files (handled separately by `avi-1766-custom-extractors`).
- Full semantic import resolution for every PHP/Kotlin/Perl construct; best-effort namespace/package resolution is in scope, but cross-package autoloading rules are not.
- Upgrading the base `tree-sitter` runtime ABI; if a parser cannot be found at ABI 11, the grammar is skipped and documented as unavailable.

## Decisions

### Decision 1: Prefer Hackage, fall back to vendoring

**Decision:** Check Hackage for `tree-sitter-php`, `tree-sitter-kotlin`, `tree-sitter-perl`, and `tree-sitter-c-sharp` compiled against runtime ABI 9–11 first. If unavailable, vendor upstream `src/parser.c` (and scanner) at an ABI-11-compatible revision under `vendor/tree-sitter-<lang>/`.

**Alternatives considered:** Vendoring everything. Rejected because Hackage packages reduce build maintenance when ABI-compatible.

**Rationale:** Matches existing mix (Hackage for most; vendored for cpp/yaml/sql) and respects the ABI guard.

### Decision 2: Add an `ImportLang` for Java only in this change

**Decision:** Only Java receives full `LangJava` resolution in this change. PHP/Kotlin/Perl/C# use the existing quoted-specifier path in `Convert.hs` where their grammars expose string literals, and a follow-up can add namespace resolution if needed.

**Alternatives considered:** Building a resolver for every new language in one pass. Rejected due to cost and unknown AST shapes.

**Rationale:** Java has a deterministic `package → path` mapping and a concrete repro, so it yields measurable connectivity gains now.

### Decision 3: Source-root discovery for Java

**Decision:** Resolve by checking `src/main/java`, `src/test/java`, and any ancestor directory whose relative path matches the package declaration. First existing file wins.

**Alternatives considered:** Requiring a `pom.xml`/`build.gradle` parse. Rejected as heavy; directory/package heuristics match the issue.

**Rationale:** Deterministic and repository-context only.

### Decision 4: Convert whitelists driven by parse inspection

**Decision:** Before editing `Convert.hs`, parse representative files for each new grammar and record observed node type names. Add only types with clear mappings in `tsTypeToKind`.

**Alternatives considered:** Guessing node type names from upstream grammar docs. Rejected; upstream grammar node naming varies.

**Rationale:** Avoids silent zero-symbol outcomes.

## Risks / Trade-offs

- [Grammar unavailable at ABI 11] → Mitigation: skip that extension binding, document in vendor README, and keep stub fallback.
- [Scanner C++ linkage] → Mitigation: add scanner to `cxx-sources` only after confirming ABI compatibility.
- [Java ambiguity when multiple source roots have same package] → Mitigation: prefer the closest ancestor and record resolution order in tests.
- [Convert whitelists incomplete] → Mitigation: integration tests assert non-zero symbol counts on reference repositories.
- [Tests may lack sample repos] → Mitigation: commit small fixtures under `test-corpus/`.

## Migration Plan

1. Add grammar bindings and cabal entries.
2. Register extensions and defaults.
3. Add Convert whitelist entries.
4. Implement Java import resolution.
5. Add tests and fixtures.
6. Run `cabal build` and `cabal test`.

Rollback: revert the openspec change or disable extensions via `graphos.yaml`.

## Open Questions -- resolved

- *Are ABI-11-compatible Hackage packages available for any of the four
  languages?* Only PHP (`tree-sitter-php >= 0.5`). C# and Perl are vendored at
  ABI 11; Kotlin has none at any compatible ABI and is aliased to the Java
  grammar. See `vendor/README.md`.
- *Should Perl module resolution follow `@INC`/`lib/` semantics or be
  deferred?* Deferred. Perl `use`/`require` emit external placeholder targets
  keyed by the bare specifier, which satisfies the no-orphan-import
  requirement without guessing at `@INC`.
" 