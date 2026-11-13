## Why

On repositories written in PHP, Kotlin, Perl, or C#, Graphos in default tree-sitter mode produces only one empty node per source file because no grammar binding and no extension registration exists for those languages. Java files parse, but `Imports.hs` only resolves imports for Go/Rust/Ruby, so Java's package-qualified imports never become `imports` edges, leaving file graphs disconnected. This breaks the 39-repo qualification set and any downstream query/analysis that relies on cross-file connectivity.

## What Changes

- Add compiled-in tree-sitter grammars for PHP, Kotlin, Perl, and C# (vendored at ABI 9–11 when no Hackage package exists), wired through `Vendored.hs`, `getGrammarPtr`, `Grammar.knownExtensions`, and `defaultExtractors`.
- Register `.php`, `.kt`, `.kts`, `.pl`, `.pm`, `.psgi`, `.cs` with appropriate grammar names, extensions, and data-file/grammar granularity, and add scanner linkage where required.
- Add Java import resolution in `Imports.hs` (`LangJava`) so `import a.b.C;` resolves to `<source-root>/a/b/C.java` under `src/main/java`, `src/test/java`, or any directory whose relative path matches the package declaration.
- Update config defaults and docs to list the new languages.
- Add unit and integration coverage for parsing and import resolution.

## Capabilities

### New Capabilities
- (none) — these are compiled-in grammars, not a new runtime mechanism.

### Modified Capabilities
- `extraction`: default extractor table and grammar-binding requirements expanded to cover PHP, Kotlin, Perl, and C#; vendored grammar handling consistent with cpp/yaml/sql.
- `import-resolution`: Java imports added to the repository-context resolver set with deterministic package-path resolution.

## Impact

- Affected code: `src/Graphos/Infrastructure/Extract/TreeSitter/{Grammar,Vendored,Convert}.hs`, `src/Graphos/Infrastructure/Wiring.hs`, `src/Graphos/Infrastructure/Extract/TreeSitter/Imports.hs`, `src/Graphos/Domain/Config/Extraction.hs`, `graphos.cabal`, `vendor/`, tests under `tests/Graphos/Infrastructure/Extract/TreeSitter/`.
- Dependencies: new vendored C/C++ sources (possibly new Hackage tree-sitter-* packages if available at compatible ABI).
- Environment: requires grammar sources pinned to ABI 9–11 per `vendor/README.md`.
- Backward compatibility: existing configs keep working; new extensions gain tree-sitter defaults.
" 