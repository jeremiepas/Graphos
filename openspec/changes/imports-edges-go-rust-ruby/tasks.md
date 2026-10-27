## 1. Import sites and resolution

- [x] 1.1 `collectImportSites`: Go `import_spec`, Rust `use_declaration` (with enclosing inline modules), Ruby `require`/`require_relative` with a literal string
- [x] 1.2 Go: nearest `go.mod` module path → package directory → non-test `.go` files, weight `1/n`, alias in `extra`
- [x] 1.3 Rust: `Cargo.toml` package/lib/path and workspace dependencies; module walk for `crate`/`self`/`super`/uniform/workspace-crate paths; groups, globs, aliases
- [x] 1.4 Ruby: `require_relative`, in-repo `require` (`lib/` load paths, root), gems as externals
- [x] 1.5 Edges and placeholders with the TypeScript shape, appended in `epParseWithGrammar`; no placeholder overrides a real node

## 2. Rust node defects

- [x] 2.1 One `Import` node per `use` occurrence (id carries the start line)
- [x] 2.2 `use` inside a function body gets a node at `function` granularity, contained by the function

## 3. Verification

- [x] 3.1 `ImportsSpec`: Go module with 2 packages + alias + stdlib; Rust 2-crate workspace with `crate::`/`super::`/group/glob, repeated `use`, fn-body `use`; Ruby `require_relative` + `lib/` require + gem
- [x] 3.2 `cabal build --flag dev` (-Werror) and `cabal test` green
- [x] 3.3 Real repositories: miniflux/v2, atuinsh/atuin, getlago/lago-api (numbers in the PR)
