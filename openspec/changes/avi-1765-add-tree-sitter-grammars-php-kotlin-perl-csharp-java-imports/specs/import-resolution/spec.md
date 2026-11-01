## ADDED Requirements

### Requirement: Java import resolution to source roots

The system SHALL resolve a Java import declaration `import a.b.C;` to the module node for `<source-root>/a/b/C.java` when that file is in the corpus. Candidate source roots SHALL be, at minimum, `src/main/java`, `src/test/java`, and any directory whose own relative path is an ancestor consistent with the package declaration of the imported file (or whose subtree contains the package path). Resolution SHALL occur in tree-sitter mode through the repository-context import resolver (Imports.hs), not through the quoted-specifier path used by TypeScript.

#### Scenario: Spring Petclinic-style Java import resolves
- **WHEN** the corpus contains `src/main/java/org/example/Foo.java` and another file contains `import org.example.Foo;`
- **THEN** the graph contains an `imports` edge from the importing file's module node to the `Foo` module node, with target `source_file` `src/main/java/org/example/Foo.java`

#### Scenario: Test source root is consulted
- **WHEN** an imported type exists only under `src/test/java`
- **THEN** resolution targets the corresponding test-root file rather than emitting an external node

#### Scenario: Missing target is not fabricated
- **WHEN** the resolved path does not exist in the corpus
- **THEN** no internal edge is emitted and the specifier is counted as unresolved (or emitted as an external node, consistent with Go/Rust/Ruby behavior)

### Requirement: New grammars participate in import edge policy

For each newly added grammar, the implementation SHALL ensure that any node of kind `Import` emitted during extraction is accompanied by an `imports` edge governed by either the quoted-specifier conversion path or a language-specific resolver in Imports.hs. An extraction path that emits `Import` nodes without corresponding `imports` edges is non-conforming, per the existing cross-file connectivity requirement.

#### Scenario: No orphan import nodes for new grammars
- **WHEN** a PHP, Kotlin, Perl, or C# file with imports is extracted
- **THEN** every emitted `Import` node's enclosing file has at least one `imports` edge, or no `Import` nodes are emitted at all
" 