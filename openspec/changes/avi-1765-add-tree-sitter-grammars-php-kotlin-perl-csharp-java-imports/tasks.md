## 1. Grammar discovery and build feasibility

- [x] 1.1 For each target language (PHP, Kotlin, Perl, C#), determine whether a Hackage `tree-sitter-*` package exists at runtime ABI 9–11; record findings in `vendor/README.md`.
- [x] 1.2 For languages without a usable package, obtain or pin an upstream grammar revision whose `LANGUAGE_VERSION` is <= 11 and verify `tree-sitter generate` succeeds with CLI 0.16.9; document revision, ABI, and license in `vendor/README.md`.
- [x] 1.3 Run a parsing experiment for each grammar against 3 representative files and record observed root and definition node type names for use in Convert whitelists.

## 2. Cabal and vendor wiring

- [x] 2.1 Add Hackage dependencies or `vendor/` source stanzas to `graphos.cabal` for the selected grammars, including parser and scanner files.
- [x] 2.2 Add FFI imports to `src/Graphos/Infrastructure/Extract/TreeSitter/Vendored.hs` (or a dedicated module) for each vendored grammar.
- [x] 2.3 Extend `getGrammarPtr` in `src/Graphos/Infrastructure/Wiring.hs` with entries for `php`, `kotlin`, `perl`, and `csharp`.
- [x] 2.4 Run `cabal build` and resolve any ABI or linkage errors before proceeding.

## 3. Domain configuration and extension registry

- [x] 3.1 Register `.php`, `.kt`, `.kts`, `.pl`, `.pm`, `.psgi`, and `.cs` in `Grammar.knownExtensions` with matching `LanguageInfo`.
- [x] 3.2 Add default extractor entries in `defaultExtractors` for the new extensions and update `.rst`/`.adoc` handling if needed.
- [x] 3.3 Add the new extensions to `fecCode` in `defaultFileExtensions`.
- [x] 3.4 Add LSP and language-ID defaults only if mature servers are available; otherwise document tree-sitter-only support.

## 4. Conversion whitelist updates

- [x] 4.1 Add relevant module/function/class/type/import AST node types to `structureTypes`, `apiSurfaceTypes`, and `importExportTypes` based on parse experiments.
- [x] 4.2 Add `tsTypeToKind` mappings for all newly whitelisted types.
- [x] 4.3 Add function-boundary entries so function bodies can be omitted at `GranularityFunction`.
- [x] 4.4 Verify conversion by running graphos on small fixtures and asserting non-zero symbol counts.

## 5. Java import resolution

- [x] 5.1 Add `LangJava` to `ImportLang` and `importLangForGrammar "java"`.
- [x] 5.2 Implement `collectImportSites` for Java (import declarations, with and without wildcard).
- [x] 5.3 Implement source-root discovery (`src/main/java`, `src/test/java`, package-path ancestors) and path resolution.
- [x] 5.4 Implement `resolveImportSites` dispatch for `LangJava`, including external node handling for unresolvable imports.
- [x] 5.5 Register Java in `withImportEdges` (already automatic via `importLangForGrammar`).
- [x] 5.6 Add tests using jpetstore-style fixtures for both internal and missing targets.

## 6. Tests and verification

- [x] 6.1 Add Hspec unit tests for `getGrammarPtr` for each new grammar.
- [x] 6.2 Add parsing tests ensuring each new extension yields expected node kinds at `GranularityFunction`.
- [x] 6.3 Add integration test or fixture run on a Java repository asserting at least one `imports` edge.
- [x] 6.4 Run `cabal test graphos-test` and record green output in the issue/PR.
- [x] 6.5 Update workflow docs and `docs/INFRASTRUCTURE_SCHEMA.md` for the new supported languages.
" 
## 7. Outcomes that differ from the plan

- **Kotlin has no ABI 9-11 grammar.** `getGrammarPtr "kotlin"` is an
  intentional alias for `tree-sitter-java` (`Wiring.hs`). Kotlin parses
  coarsely: `package`, `import`, `class` and member declarations are
  recognized; a `fun` is labelled from the Java `method_declaration` shape.
- **Kotlin import resolution was added too** (not planned in 5.x). The
  `import-resolution` delta forbids emitting `Import` nodes without `imports`
  edges, and Kotlin-via-Java emitted exactly that. `LangKotlin` resolves
  against `src/main/kotlin` / `src/test/kotlin` first, then the Java roots
  (interop), then falls back to an external node.
- **PHP/Perl/C# specifiers are unquoted**, so `Convert.extractSpecifier` --
  which only looked for string literals -- returned nothing and their `Import`
  nodes carried an empty label and no edge. `Convert.bareSpecifier` now reads
  the unquoted form (including the C# alias directive `using A = B.C;`).
- **Three node names in the whitelists were dead** (verified by parsing with
  the vendored `parser.c`, see `vendor/README.md`): Perl `use Foo;` is
  `use_no_statement` (not `use_constant_statement`), a named `sub f { }` is
  `function_definition` (not `subroutine`), and a quoted `require 'x.pl'`
  does not parse at all. PHP's import forms were never emitted because they
  were missing from `apiSurfaceTypes`, and C#'s root `compilation_unit` was
  missing from `structureTypes`, so `.cs` files had no module node.
- **`.pl` / `.pm` / `.psgi` were missing from `fecCode`** (task 3.3), so Perl
  files were never even detected; registering the grammar alone had no effect.
- **Identifier-less declarations collided on an empty node id.** Verifying
  4.4 on a mixed-language fixture corpus surfaced it: Perl `package`, PHP
  `namespace`, C# `namespace A.B` and C#/Java field declarations all resolved
  to no identifier, so each got node id `""` and only one survived the merge
  of the per-file extractions. `Core.extractIdentifier` now knows
  `package_name` / `namespace_name`, and falls back (keyed on the declaration
  type, so a type reference is never mistaken for a name) to the C#
  `qualified_name` namespace form and to the `variable_declarator` that holds
  a field's name.
- 3.4: LSP defaults were available and are registered (`omnisharp`,
  `phpactor`, `perlnavigator`, `kotlin-language-server`).
- 6.4: `graphos-test` is **1663 examples, 1 failure, 4 pending in 84 s**, with
  two caveats that are both pre-existing and unrelated to this change:
  - The one failure belongs to the separate, unapplied
    `avi-1440-ts-declaration-only-type-nodes` change: its regression test is
    committed but its task 1.1 -- removing `type_identifier` from
    `apiSurfaceTypes` -- is not done.
  - `BoundedDeltaCommunitySpec.hs:224`, "delta update equals full rebuild on
    small random graphs (property)", does not terminate, so a plain
    `cabal test graphos-test` never finishes. Verified on `main` (362c1fc)
    with this branch's changes stashed: identical hang, so CI's
    `cabal test all` is blocked independently of this work. The figures above
    come from a run with `--skip "small random graphs"`; everything else in
    the suite completes.
