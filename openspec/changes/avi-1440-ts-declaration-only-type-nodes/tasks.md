## 1. Narrow the TypeScript type whitelist to declarations

- [ ] 1.1 Remove `"type_identifier"` from `apiSurfaceTypes` in
  `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs` (references are not
  declarations; other-language whitelist entries are left untouched)
- [ ] 1.2 Remove the `type_identifier -> "Type"` clause from `tsTypeToKind` and replace
  it with a comment documenting which declaration kinds DO yield `kind: "Type"`
  (`type_declaration`, `type_alias_declaration`, `type_definition`, `type_alias`,
  `type_item`, `type_synonym`, `data_type`, `new_type`, and C++ `alias_declaration`)

## 2. Regression test

- [ ] 2.1 In `tests/Graphos/Infrastructure/Extract/TreeSitter/GrammarsSpec.hs`, add a
  TypeScript case that parses the vendored `example/ts-lsp-test/src/types.ts` fixture at
  `function` granularity and asserts: (a) declared `Interface`/`Enum`/`Type` nodes
  survive; (b) reference labels `Date`, `number`, `string` are absent; (c) exactly one
  `Type` node exists. Reuse the existing `extract` helper.

## 3. Verify

- [ ] 3.1 `cabal build --flag dev` is clean (no `-Werror` failures)
- [ ] 3.2 The GrammarsSpec TypeScript case is green
- [ ] 3.3 `openspec validate` passes the delta spec
