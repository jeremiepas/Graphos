## Why

In tree-sitter mode, Go, Rust and Ruby files produce no `imports` edges
(GitHub #83). TypeScript files do. That breaks the existing `import-resolution`
requirement that every extraction path producing `Import` nodes also produces
`imports` edges. Measured on public repositories:

- **Go** (miniflux/v2): Graphos emits one `Import` node per file and 0 edges.
  The repository has 857 internal import paths across 253 non-test files.
- **Rust** (atuinsh/atuin, 14-crate workspace): 5,193 `Import` nodes and 0
  edges. Repeated `use` text collapses into one node (135 cases), and a `use`
  inside a function body gets no node (141 lines).
- **Ruby** (getlago/lago-api): `require` and `require_relative` produce no
  edges.

## What Changes

- New `Graphos.Infrastructure.Extract.TreeSitter.Imports` module. It reads the
  import sites off the AST in a pure step. It then resolves them in IO, using
  repository context: the nearest `go.mod`; the crate and workspace
  `Cargo.toml` files plus the module files on disk; and the Ruby project root
  with its `lib/` directories. Finally it appends `imports` edges with the
  TypeScript shape: module node → placeholder whose `source_file` is the repo
  file, or `external:<name>` for packages.
- **Go:** an internal package import becomes one edge per non-test `.go` file
  in the package directory. Each edge has weight `1/n` and
  `extra.package_files = n`. Aliases (`m`, `_`, `.`) are kept in
  `extra.alias`. Go `import_declaration` no longer goes through the TypeScript
  `from ` specifier path.
- **Rust:** `crate::`, `self::`, `super::`, 2018 uniform paths, and workspace
  or path crates (code name = dependency key with `-` → `_`, plus the
  package's own `[lib]` name). Each path walks the module files (`x.rs` or
  `x/mod.rs`) and stops at the deepest module that has a file.
  `std`/`core`/`alloc` and declared dependencies become external edges. Local
  items such as enum variants produce no edge.
- **Rust nodes:** `use_declaration` ids carry the start line, giving one node
  per occurrence. A `use` inside an opaque function body gets a node,
  contained by the function.
- **Ruby:** `require_relative` resolves against the file's directory. An
  in-repo `require` resolves against the enclosing `lib/` directories,
  `<root>/lib`, `<root>`, and `<root>/lib/<gem>/` (vendored gems). Anything
  else becomes an external gem placeholder. Calls with an interpolated or
  non-literal argument are skipped.
- When the grammar fails on a file (an `ERROR` root), the root is still used
  as the file's `Module` node, so imports read from the partial tree have a
  source.
- The extraction-cache fingerprint gains a `treesitter-imports` entry, so
  cached extractions from older binaries are not reused.

## Capabilities

### Modified Capabilities
- `import-resolution`: Go, Rust and Ruby import resolution, and one Rust
  `Import` node per `use` occurrence.

## Impact

- Code: `Extract/TreeSitter/Imports.hs` (new), `Convert.hs` (per-occurrence
  `use` ids, function-body `use` nodes, `fileModuleNode`, Go excluded from
  the TypeScript specifier path), `Resolver.hs` (exports `resolveDots`),
  `Wiring.hs` (the `epParseWithGrammar` path appends the import edges), and
  `UseCase/Extract/Core.hs` (fingerprint).
- Tests: `ImportsSpec`, which uses small on-disk Go, Rust and Ruby projects
  parsed with the real grammars.
- Rust `Import` node ids change: they gain a `#L<line>` suffix.
- Out of scope: Zeitwerk constant references (Ruby), `#[path]` attributes and
  `go.work` / `replace` directives.
