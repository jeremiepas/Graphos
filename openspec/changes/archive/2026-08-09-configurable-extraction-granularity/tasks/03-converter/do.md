# Do: Tiered whitelist + recursion stop

- `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs`: three exported tier lists + `functionBoundaryTypes`; `typesFor`/`descendInto`; `tsNodesToExtraction`/`tsNodeToGraphNodes`/`tsNodeToGraphEdges` parameterized by `Granularity`; `definitionTypes` retained as the fine-level whitelist (tier concatenation).
- `tests/Graphos/Infrastructure/Extract/TreeSitter/ConvertSpec.hs`: TS-like fixture (class/method/field/local/statements/module-const/import) + JSON fixture, asserted at all three levels (11 cases); registered in `graphos.cabal`.
- Markdown path unaffected: it delegates to the built-in Markdown parser before reaching this converter.
