## ADDED Requirements

### Requirement: Domain layer has no Infrastructure imports

Modules under `src/Graphos/Domain/` SHALL NOT import any module under `Graphos.Infrastructure.*`. Types required by both layers (e.g., observability configuration) SHALL be defined in the Domain layer, with Infrastructure importing from Domain.

#### Scenario: No Domain-to-Infrastructure imports

- **WHEN** `src/Graphos/Domain/` is searched for `import Graphos.Infrastructure`
- **THEN** zero matches are found

#### Scenario: OtelConfig lives in Domain

- **WHEN** `OtelConfig` and `defaultOtelConfig` are resolved
- **THEN** they are defined in a Domain module, and `Infrastructure.Observability.SDK` imports them from there

#### Scenario: Pipeline config compiles against Domain-only types

- **WHEN** `Domain.Types.Pipeline` is compiled
- **THEN** it succeeds with no Infrastructure module in its import list and `cabal test` passes
