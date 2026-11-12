# Vendored tree-sitter grammars

Graphos links the Haskell `tree-sitter-0.9` runtime, which accepts only
grammar **ABI versions 9–11**. Hackage has no `tree-sitter-*` package for the
languages below, and current upstream grammars are generated at ABI 14/15, so
their sources are vendored here at an ABI-11-compatible revision and compiled
into the `graphos` library (`c-sources` / `cxx-sources` in `graphos.cabal`).
FFI symbols are bound in `Graphos.Infrastructure.Extract.TreeSitter.Vendored`.

| Grammar | Upstream | Revision | ABI | License | Notes |
| ------- | -------- | -------- | --- | ------- | ----- |
| `tree-sitter-cpp` | <https://github.com/tree-sitter/tree-sitter-cpp> | `fb8250e` (2020-11-02) | 11 | MIT | upstream `src/` copied as-is |
| `tree-sitter-yaml` | <https://github.com/ikatyang/tree-sitter-yaml> | `v0.3.1` | 11 | MIT | upstream `src/` copied as-is |
| `tree-sitter-sql` | <https://github.com/m-novikov/tree-sitter-sql> | `148b442` (2022-06-19) | 11 | MIT | `parser.c` regenerated from `grammar.js` with tree-sitter CLI **0.16.9** |
| `tree-sitter-c-sharp` | <https://github.com/tree-sitter/tree-sitter-c-sharp> | vendored 2026-10-31 (AVI-1716) | 11 | MIT | revision predates C# 9 records; `record_declaration` is not a node at this ABI |
| `tree-sitter-perl` | <https://github.com/tree-sitter/tree-sitter-perl> | vendored 2026-10-31 (AVI-1716) | 11 | MIT | see the Perl node-type note below |

`tree-sitter-php` is **not** vendored: it is provided by `tree-sitter-php >= 0.5` on Hackage (ABI 11, `LANGUAGE_VERSION 11` verified in vendor `src/parser.c`).

## PHP node-type note

`tree-sitter-php 0.5.0.2` (vendor `src/parser.c`) exposes imports as
`include_expression`, `require_expression`, `include_once_expression`,
`require_once_expression` — not `include_statement` / `require_statement`.
`Convert.hs` maps the `*_expression` names (plus `namespace_use_declaration` for
`use Foo\Bar;`). The include/require forms are *expressions* nested in an
`expression_statement`, so they are whitelisted in `apiSurfaceTypes` in their
own right — the enclosing statement is implementation detail and is not
emitted at `GranularityFunction`.

## C# node-type note

The file root is `compilation_unit` (not `program` / `source_file`), so it is
listed in `structureTypes` and `moduleTypes`; without it a `.cs` file gets no
module node and its `using` edges have no source. `using System;` is a
`using_directive` whose specifier is unquoted — `Convert.bareSpecifier` reads
it, including the alias form `using A = B.C;` (a `name_equals` child).

## Perl node-type note

Verified by parsing with the vendored `src/parser.c` (AVI-1765) — the names
differ from what the grammar's symbol table suggests:

| Source | Node type |
| ------ | --------- |
| `sub total { 1 }` | `function_definition` (**not** `subroutine`, which is the anonymous `sub { }`) |
| `package Shop::Cart;` | `package_statement` |
| `use Foo;` / `no Foo;` | `use_no_statement` |
| `use constant PI => 3;` | `use_constant_statement` |
| `use parent 'Base';` | `use_parent_statement` |
| `require Foo::Bar;` | `require_statement` |
| `require 'other.pl';` | **does not parse** — yields an `ERROR` node |

Only the bareword `require` form is claimed; the quoted form is unsupported at
this ABI.

## Kotlin note

No `tree-sitter-kotlin` package exists at ABI 9–11. `getGrammarPtr "kotlin"` is
an intentional alias for `tree-sitter-java` (see `Wiring.hs:335`) and is
documented as Java-alias-only.

Every vendored grammar ships a byte-identical `src/tree_sitter/parser.h`, so a single
`include-dirs` entry serves all grammars.

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
