# import-resolution (delta)

## ADDED Requirements

### Requirement: Declarative resolve configuration

Graphos SHALL support top-level resolve: mapping extensions to built-in resolver
kinds (package, psr4, module-path, path-prefix) with roots or source-of-truth
file, replacing hardcoded per-language handling.

#### Scenario: Java package resolution from config

- **WHEN** build uses resolve: { ".java": { kind: package, roots: ["src/main/java"] } }
  and imports com.example.Foo
- **THEN** imports edge targets src/main/java/com/example/Foo.java when present

### Requirement: Unknown resolver kind fails fast

Graphos SHALL fail config loading on unknown kind, naming extension and kind.

#### Scenario: Unknown kind rejected

- **WHEN** config contains resolve: { ".x": { kind: frobnicate } }
- **THEN** config loading fails naming .x and frobnicate

### Requirement: Resolution precedence

A config resolve: entry SHALL override a built-in resolver for the same extension with a warning
naming the extension and the overridden resolver.

#### Scenario: Config overrides built-in resolver

- **WHEN** user provides resolve entry for extension with built-in resolution
- **THEN** config resolver used and warning logged
