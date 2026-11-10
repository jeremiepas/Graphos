## Why

GitHub issue #119 (motivation: 39-repo qualification set had empty graphs for
Perl/PHP/Kotlin and no file-import edges for Java) reports that every extractor
binding is compiled in: ExtractorMode only offers lsp / tree-sitter / stub and
both grammar and LSP tables are baked in. Adding a language (Perl, PHP, Kotlin,
COBOL, Terraform/HCL, JSP, web.xml) is a code change. Users need to plug their
own extractor for an extension without patching Graphos.

## What Changes

- Add mode: command external process per file (or batch) printing graph.json-shaped
  nodes/edges on stdout. Graphos validates, prefixes ids with directory bucket,
  merges like any extractor. Per-file errors recorded and never abort build.
- Add user-supplied runtime tree-sitter grammar plus .scm queries loaded at
  runtime against the bundled ABI 9-11 runtime.
- Add declarative resolve: import-resolution rules in config.
- Modify extraction capability for dispatch, fingerprinting, validation.

## Capabilities

### New Capabilities
- external-command-extractor: external-process extractor producing graph.json-shaped nodes/edges.
- runtime-tree-sitter-grammar: load tree-sitter grammar .so at runtime plus .scm queries.

### Modified Capabilities
- extraction: dispatch extended, error recording, cache fingerprint.
- import-resolution: declarative resolve: config adds built-in resolvers.

## Impact

Domain/Config/Extraction, UseCase/Extract/Core, Infrastructure/Extract/Command,
Infrastructure/Extract/TreeSitter, Infrastructure config loader, tests,
workflow docs 05-path and 16-lsp-discovery.
