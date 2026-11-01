## ADDED Requirements

### Requirement: Tree-sitter grammar bindings for PHP

The system SHALL provide a tree-sitter language binding for the `php` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.php` files produce real symbol nodes in default tree-sitter mode. The binding SHALL be provided either via a compatible Hackage `tree-sitter-*` package or via a vendored source compiled at ABI 9–11 under `vendor/`, consistent with the existing cpp/yaml/sql grammars.

#### Scenario: PHP file parsed with tree-sitter
- **WHEN** a `.php` file is processed and `grammar: php` is configured
- **THEN** `getGrammarPtr "php"` returns `Just` with a valid language pointer, and the file produces function/class/method/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for Kotlin

The system SHALL provide a tree-sitter language binding for the `kotlin` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.kt` and `.kts` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: Kotlin file parsed with tree-sitter
- **WHEN** a `.kt` or `.kts` file is processed and `grammar: kotlin` is configured
- **THEN** `getGrammarPtr "kotlin"` returns `Just` with a valid language pointer, and the file produces function/class/method/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for Perl

The system SHALL provide a tree-sitter language binding for the `perl` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.pl`, `.pm`, and `.psgi` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: Perl file parsed with tree-sitter
- **WHEN** a `.pl`, `.pm`, or `.psgi` file is processed and `grammar: perl` is configured
- **THEN** `getGrammarPtr "perl"` returns `Just` with a valid language pointer, and the file produces subroutine/package/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for C Sharp

The system SHALL provide a tree-sitter language binding for the `csharp` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.cs` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: C Sharp file parsed with tree-sitter
- **WHEN** a `.cs` file is processed and `grammar: csharp` is configured
- **THEN** `getGrammarPtr "csharp"` returns `Just` with a valid language pointer, and the file produces class/method/namespace nodes rather than a single stub node

### Requirement: Default extractors for new language extensions

The system SHALL default `defaultExtractors` to `ExtractTreeSitter` for the new language extensions with the following grammar and language_id values:

| Extension | Mode | Grammar | Language ID |
|-----------|------|---------|-------------|
| `.php` | tree-sitter | php | php |
| `.kt` | tree-sitter | kotlin | kotlin |
| `.kts` | tree-sitter | kotlin | kotlin |
| `.pl` | tree-sitter | perl | perl |
| `.pm` | tree-sitter | perl | perl |
| `.psgi` | tree-sitter | perl | perl |
| `.cs` | tree-sitter | csharp | csharp |

#### Scenario: Newly supported extension uses tree-sitter by default
- **WHEN** no `extractors` section is provided in `graphos.yaml`
- **THEN** `.php`, `.kt`, `.pl`, and `.cs` files are extracted with `mode: tree-sitter` and the corresponding grammar shown above

### Requirement: Grammar extension registry includes new extensions

`Grammar.knownExtensions` SHALL contain entries for `.php`, `.kt`, `.kts`, `.pl`, `.pm`, `.psgi`, and `.cs`, each mapping to a `LanguageInfo` whose `liGrammarName` equals the configured grammar (`php`, `kotlin`, `perl`, `csharp` respectively) and whose `liFileType` is `CodeFile`.

#### Scenario: Extension lookup succeeds
- **WHEN** `languageForExt` is called with any of `.php`, `.kt`, `.pl`, or `.cs`
- **THEN** it returns `Just` with the matching `LanguageInfo`
" 