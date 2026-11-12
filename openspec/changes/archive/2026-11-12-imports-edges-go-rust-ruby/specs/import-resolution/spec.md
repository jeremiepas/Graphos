## ADDED Requirements

### Requirement: Go, Rust and Ruby imports edges

Tree-sitter extraction of Go, Rust and Ruby files SHALL emit one `imports` edge per imported unit. The edge SHALL start at the file's module node and point at a placeholder node of kind `External`. The placeholder's `source_file` SHALL be the imported repository file when the import resolves inside the repository, and `external:<name>` otherwise. Specifiers that are not literal SHALL NOT produce a target, and neither SHALL specifiers that contain whitespace or interpolation.

#### Scenario: Go internal package import
- **WHEN** a Go file in module `example.com/shop` imports `m "example.com/shop/internal/model"`, and that package directory holds two non-test `.go` files
- **THEN** the file has one `imports` edge to each of the two files, each with weight 0.5, `extra.package_files = 2` and `extra.alias = "m"`, and no edge to a `_test.go` file

#### Scenario: Go stdlib import
- **WHEN** a Go file imports `"fmt"`
- **THEN** it has an `imports` edge to a placeholder whose `source_file` is `external:fmt`

#### Scenario: Rust paths across modules and crates
- **WHEN** a Rust file uses `super::Kind`, `crate::util::helper`, `crate::util::{self, fmt::*}` and `other_crate::model::Item`, where `other_crate` is a workspace path dependency
- **THEN** each path has an `imports` edge to the file of the deepest module it names: the parent module's file, `util.rs`, `util/fmt.rs`, and the other crate's `model` file

#### Scenario: Rust local item
- **WHEN** a Rust file uses `Color::*` and `Color` is an enum declared in the same file
- **THEN** no `imports` edge is emitted for that path

#### Scenario: Ruby requires
- **WHEN** a Ruby file calls `require_relative "../helpers/money"`, `require "shop/pricing"` (present as `lib/shop/pricing.rb`) and `require "json"`
- **THEN** it has `imports` edges to `app/helpers/money.rb`, `lib/shop/pricing.rb` and `external:json`, and a `require` with an interpolated string produces no edge

### Requirement: One Rust Import node per use occurrence

Every Rust `use_declaration` SHALL produce its own `Import` node. This applies when two declarations have the same text, and when the declaration sits inside a function body at `function` granularity.

#### Scenario: Repeated use
- **WHEN** `use crate::util::helper;` appears at module level and again inside `mod tests`
- **THEN** the extraction holds two `Import` nodes, one for each start line

#### Scenario: Use in a function body
- **WHEN** a function body contains `use crate::util::fmt::render;`
- **THEN** the extraction holds an `Import` node for it, and the function `contains` it
