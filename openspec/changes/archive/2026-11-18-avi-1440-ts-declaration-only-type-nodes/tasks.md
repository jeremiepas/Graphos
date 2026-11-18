## 1. Narrow the TypeScript type whitelist to declarations

- [x] 1.1 Remove `"type_identifier"` from `apiSurfaceTypes` in
  `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs` (references are not
  declarations; other-language whitelist entries are left untouched)
- [x] 1.2 Remove the `type_identifier -> "Type"` clause from `tsTypeToKind` and replace
  it with a comment documenting which declaration kinds DO yield `kind: "Type"`
  (`type_declaration`, `type_alias_declaration`, `type_definition`, `type_alias`,
  `type_item`, `type_synonym`, `data_type`, `new_type`, and C++ `alias_declaration`)

## 2. Regression test

- [x] 2.1 In `tests/Graphos/Infrastructure/Extract/TreeSitter/GrammarsSpec.hs`, add a
  TypeScript case that parses the vendored `example/ts-lsp-test/src/types.ts` fixture at
  `function` granularity and asserts: (a) declared `Interface`/`Enum`/`Type` nodes
  survive; (b) reference labels `Date`, `number`, `string` are absent; (c) exactly one
  `Type` node exists. Reuse the existing `extract` helper.

## 3. Verify

- [x] 3.1 `cabal build --flag dev` is clean (no `-Werror` failures)
- [x] 3.2 The GrammarsSpec TypeScript case is green
- [x] 3.3 `openspec validate` passes the delta spec

Implemented in #88 (`416466a`): `type_identifier` is out of `apiSurfaceTypes` and
`tsTypeToKind` (comment at `tsTypeToKind`), regression cases in `GrammarsSpec`
("TypeScript: only declared types/enum/interface kept …") and `ConvertSpec`
("a bare type_identifier reference emits no node"). Verified 2026-11-18 on
`origin/main` `f8fe023`: `cabal build --flag dev all --enable-tests` clean, both
cases green, `openspec validate --strict` passes. The delta targeted a
non-existent `01-extraction-granularity` capability with MODIFIED; it is an
ADDED requirement of `extraction-granularity`.
