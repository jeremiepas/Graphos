## 1. Grammar discovery and build feasibility

- [ ] 1.1 For each target language (PHP, Kotlin, Perl, C#), determine whether a Hackage `tree-sitter-*` package exists at runtime ABI 9–11; record findings in `vendor/README.md`.
- [ ] 1.2 For languages without a usable package, obtain or pin an upstream grammar revision whose `LANGUAGE_VERSION` is <= 11 and verify `tree-sitter generate` succeeds with CLI 0.16.9; document revision, ABI, and license in `vendor/README.md`.
- [ ] 1.3 Run a parsing experiment for each grammar against 3 representative files and record observed root and definition node type names for use in Convert whitelists.

## 2. Cabal and vendor wiring

- [ ] 2.1 Add Hackage dependencies or `vendor/` source stanzas to `graphos.cabal` for the selected grammars, including parser and scanner files.
- [ ] 2.2 Add FFI imports to `src/Graphos/Infrastructure/Extract/TreeSitter/Vendored.hs` (or a dedicated module) for each vendored grammar.
- [ ] 2.3 Extend `getGrammarPtr` in `src/Graphos/Infrastructure/Wiring.hs` with entries for `php`, `kotlin`, `perl`, and `csharp`.
- [ ] 2.4 Run `cabal build` and resolve any ABI or linkage errors before proceeding.

## 3. Domain configuration and extension registry

- [ ] 3.1 Register `.php`, `.kt`, `.kts`, `.pl`, `.pm`, `.psgi`, and `.cs` in `Grammar.knownExtensions` with matching `LanguageInfo`.
- [ ] 3.2 Add default extractor entries in `defaultExtractors` for the new extensions and update `.rst`/`.adoc` handling if needed.
- [ ] 3.3 Add the new extensions to `fecCode` in `defaultFileExtensions`.
- [ ] 3.4 Add LSP and language-ID defaults only if mature servers are available; otherwise document tree-sitter-only support.

## 4. Conversion whitelist updates

- [ ] 4.1 Add relevant module/function/class/type/import AST node types to `structureTypes`, `apiSurfaceTypes`, and `importExportTypes` based on parse experiments.
- [ ] 4.2 Add `tsTypeToKind` mappings for all newly whitelisted types.
- [ ] 4.3 Add function-boundary entries so function bodies can be omitted at `GranularityFunction`.
- [ ] 4.4 Verify conversion by running graphos on small fixtures and asserting non-zero symbol counts.

## 5. Java import resolution

- [ ] 5.1 Add `LangJava` to `ImportLang` and `importLangForGrammar "java"`.
- [ ] 5.2 Implement `collectImportSites` for Java (import declarations, with and without wildcard).
- [ ] 5.3 Implement source-root discovery (`src/main/java`, `src/test/java`, package-path ancestors) and path resolution.
- [ ] 5.4 Implement `resolveImportSites` dispatch for `LangJava`, including external node handling for unresolvable imports.
- [ ] 5.5 Register Java in `withImportEdges` (already automatic via `importLangForGrammar`).
- [ ] 5.6 Add tests using jpetstore-style fixtures for both internal and missing targets.

## 6. Tests and verification

- [ ] 6.1 Add Hspec unit tests for `getGrammarPtr` for each new grammar.
- [ ] 6.2 Add parsing tests ensuring each new extension yields expected node kinds at `GranularityFunction`.
- [ ] 6.3 Add integration test or fixture run on a Java repository asserting at least one `imports` edge.
- [ ] 6.4 Run `cabal test graphos-test` and record green output in the issue/PR.
- [ ] 6.5 Update workflow docs and `docs/INFRASTRUCTURE_SCHEMA.md` for the new supported languages.
" 