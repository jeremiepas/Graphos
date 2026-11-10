# Vendored tree-sitter grammars

Graphos links the Haskell `tree-sitter-0.9` runtime, which accepts only
grammar **ABI versions 9–11**. Hackage has no `tree-sitter-*` package for the
languages below, and current upstream grammars are generated at ABI 14/15, so
their sources are vendored here at an ABI-11-compatible revision and compiled
into the `graphos` library (`c-sources` / `cxx-sources` in `graphos.cabal`).
FFI symbols are bound in `Graphos.Infrastructure.Extract.TreeSitter.Vendored`.

| Grammar | Upstream | Revision | ABI | Notes |
| ------- | -------- | -------- | --- | ----- |
| `tree-sitter-cpp` | <https://github.com/tree-sitter/tree-sitter-cpp> | `fb8250e` (2020-11-02) | 11 | upstream `src/` copied as-is |
| `tree-sitter-yaml` | <https://github.com/ikatyang/tree-sitter-yaml> | `v0.3.1` | 11 | upstream `src/` copied as-is |
| `tree-sitter-sql` | <https://github.com/m-novikov/tree-sitter-sql> | `148b442` (2022-06-19) | 11 | `parser.c` regenerated from `grammar.js` with tree-sitter CLI **0.16.9** |

Every grammar ships a byte-identical `src/tree_sitter/parser.h`, so a single
`include-dirs` entry serves all three.

## Upgrading

Do not drop in a newer `parser.c` without checking its first lines:
`#define LANGUAGE_VERSION` must be `<= 11`, or `ts_parser_set_language` will
reject the grammar at runtime and every file of that language becomes a stub.

To regenerate a grammar at ABI 11:

```sh
curl -sL https://github.com/tree-sitter/tree-sitter/releases/download/0.16.9/tree-sitter-linux-x64.gz \
  | gunzip > tree-sitter && chmod +x tree-sitter
./tree-sitter generate   # inside the grammar checkout; writes src/parser.c
```

CLI 0.16.9 predates named precedences (`precedences:`) and reserved words, so
newer `grammar.js` files (for example DerekStride/tree-sitter-sql) do not
generate with it.
