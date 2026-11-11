# intent-spec-checks

The four-pattern decidable spec language and the `graphos intentcheck` command. All
quantifiers are bounded by the node set, so the verifier is the decision procedure — its
soundness is the one-line Lean theorem `check_sound`, and the Domain evaluator mirrors
the Lean semantics clause for clause.

## ADDED Requirements

### Requirement: Four decidable spec patterns

The spec language SHALL provide `reaches` (a named node of one kind reaches a node of
another kind via edges of one label), `absent` (no node of a kind whose name starts with
a prefix, modulo an explicit whitelist), `covered` (every node of a kind is reached via
a label by a node of another kind), and `parallel` (total correspondence in both
directions between two kinds via a label), composable with `and` and `not`. Evaluation
SHALL be deterministic, use no model, and agree with the Lean model's semantics on the
shared fixtures.

- **Plan**: The four patterns cover the observed omission classes — missing safeguard,
  incomplete migration, untested surface, parallel-structure drift — while keeping the
  logic decidable, which is what makes the checker certifiable.
- **Do**: `Domain/IntentCheck.hs` bounded-quantifier evaluation; fixture-parity Hspec
  against the Lean toy instance.
- **Check**: Scenarios below.
- **Act**: New omission classes become new `Spec` constructors with their Lean semantics
  added first.

#### Scenario: Parallel-structure drift is detected

- **WHEN** a graph contains a `schema-field` node with no `maps` edge to any
  `contract-field` node
- **THEN** evaluating the schema↔contract `parallel` spec returns violated, naming the
  unmatched node

#### Scenario: Verdicts match the certified model

- **WHEN** the Haskell evaluator runs on the fixtures whose verdicts the Lean kernel
  accepted by `decide`
- **THEN** every verdict is identical for all four patterns and their `and`/`not`
  compositions

### Requirement: intentcheck command and gate

`graphos intentcheck --specs FILE` SHALL evaluate every spec in the file against the
projection, emit a markdown report (JSON single document under `--json`) naming each
spec's verdict and, for violations, the witnessing nodes, and SHALL exit non-zero iff at
least one spec is violated.

- **Plan**: A type-checker for intent: CI-gateable, witness-carrying, reproducible.
- **Do**: UseCase orchestration; Infrastructure spec-file loader and report writer; CLI
  registered with helper + example per `cli-subcommand-help`.
- **Check**: Scenarios below.
- **Act**: If spec files grow, add includes — never implicit discovery.

#### Scenario: Violation gates with a witness

- **WHEN** `intentcheck` evaluates a corpus where one `covered` spec fails
- **THEN** the exit code is non-zero and the report names the failing spec and the
  uncovered nodes

#### Scenario: Clean corpus exits zero

- **WHEN** every spec in the file holds on the projection
- **THEN** the exit code is zero and the report marks every spec satisfied

### Requirement: Non-triviality probe

Given probe graphs via `--probe`, `intentcheck` SHALL warn — without gating — about any
spec that no probe satisfies or that no probe violates, since a spec that accepts
everything or rejects everything measures nothing.

- **Plan**: The one decidable fragment of formalization-fidelity checking ships here;
  the probabilistic rest is explicitly deferred.
- **Do**: Evaluate each spec over the probe set; classify tautological/unsatisfiable on
  that set; warnings section in the report.
- **Check**: Scenario below.
- **Act**: Curate a standard probe set from real before/after exports as adoption grows.

#### Scenario: Vacuous spec is flagged, not gated

- **WHEN** a spec holds on every supplied probe graph and the main corpus passes
- **THEN** the exit code is zero and the report carries a non-triviality warning naming
  that spec
