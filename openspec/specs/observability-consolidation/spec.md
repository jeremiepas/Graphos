# Observability Consolidation Capability

## Purpose

Maintain exactly one observability implementation to eliminate dead/duplicate code, reduce maintenance burden, and prevent parallel implementations from drifting.

## Requirements

### Requirement: Single observability implementation

The project SHALL contain exactly one observability implementation, `Graphos.Infrastructure.Observability.SDK`. The dead parallel module `Graphos.Infrastructure.Observability` SHALL be removed from the source tree and from `graphos.cabal`.

#### Scenario: Dead module removed

- **WHEN** the source tree and `graphos.cabal` are inspected
- **THEN** `src/Graphos/Infrastructure/Observability.hs` does not exist and `Graphos.Infrastructure.Observability` is not listed in exposed-modules

#### Scenario: Build and tests unaffected

- **WHEN** `cabal build` and `cabal test` run after removal
- **THEN** both succeed with no missing-module errors, confirming the module was unreferenced

#### Scenario: Observability behavior preserved

- **WHEN** the pipeline runs with observability enabled
- **THEN** spans, histograms, and debug traces behave identically to before the removal (SDK.hs untouched by this change)
