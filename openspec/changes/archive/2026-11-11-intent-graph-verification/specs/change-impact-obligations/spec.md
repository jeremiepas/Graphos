# change-impact-obligations

Turn a change (before/after projections) into a finite obligation list and an
admissibility verdict. The omission surface is the frontier — impacted but unmodified —
and every frontier node must be explicitly discharged: by a human ack or by a rule that
carries its own Lean safety obligation. Computation is pure Domain, mirrored by the Lean
model (obligation O3 for closure, O4 for the shipped rule).

## ADDED Requirements

### Requirement: Touched set from the before/after diff

`graphos impact` SHALL compute the touched set as the union of removed nodes, added
nodes, and nodes changed in attributes or in outgoing edges between the before and after
projections, matching nodes by identical `NodeId`.

- **Plan**: Out-edge changes are modifications too — rewiring a call is invisible to
  attribute diffing; `Domain/Graph/Diff.hs` provides added/removed today and gains
  changed-detection.
- **Do**: Extend the diff with attribute and out-edge-set comparison in
  `Domain/Impact.hs`.
- **Check**: Scenario below.
- **Act**: If renames prove noisy as remove+add pairs, consider an explicit rename map —
  never fuzzy name matching.

#### Scenario: Body edit and added node are both touched

- **WHEN** a node's `body_hash` differs between before and after and another node exists
  only in after
- **THEN** the touched set contains exactly those two nodes

### Requirement: Impact closure and frontier enumeration

The impact set SHALL be the reflexive-transitive closure of the touched set under
reversed dependency edges (`calls`, `implements`, `references`, `maps`, `depends_on` —
not `tests`), computed on the before projection; the frontier SHALL be the impact set
minus the touched set; and the report SHALL enumerate every frontier node as an
obligation. The closure SHALL be closed under reverse dependency (the property proved as
O3 in the Lean model).

- **Plan**: The frontier is the omission surface: for an LLM or a human, "rule on these
  N nodes" replaces "did we forget anything?".
- **Do**: Fuel-bounded closure in `Domain/Impact.hs` mirroring the Lean `impact`;
  QuickCheck closure property.
- **Check**: Scenarios below.
- **Act**: When git evolutionary-coupling edges land (deferred), they join the
  propagation labels behind a flag.

#### Scenario: Caller of a changed callee is on the frontier

- **WHEN** only a validator's body changed and a handler has a `calls` edge to it
- **THEN** the handler is in the impact set and listed on the frontier

#### Scenario: Tests do not propagate impact

- **WHEN** a node is reached only by a `tests` edge from a test node
- **THEN** that test node joins the frontier only if it depends on a touched node
  through a dependency label

### Requirement: Discharge discipline

An obligation SHALL be dischargeable only by an explicit ack (`--ack ID`, recorded in
the report as an audit trail) or by a registered non-affectation rule; each shipped rule
SHALL carry a safety obligation in the Lean model. The shipped `stableSignature` rule
SHALL discharge a frontier node iff every touched direct dependency kept its signature,
kind and name (body-only change); its safety obligation is O4 (observable behaviour —
reachable signature/label pairs — preserved). Open obligations SHALL be listed; the
change is complete iff none remain.

- **Plan**: An unproven discharge rule converts an obligation into false confidence —
  worse than no rule; hence one safety theorem per rule.
- **Do**: `Rule` as a pure predicate in `Domain/Impact.hs`; `stableSignature` mirrors
  the Lean definition; acks threaded from CLI.
- **Check**: Scenarios below.
- **Act**: New rules require their O4-style theorem before registration — a review
  policy, enforced by the tasks of the change that adds them.

#### Scenario: Body-only change is discharged by rule

- **WHEN** a handler's only touched dependency changed body but kept signature, kind and
  name
- **THEN** the handler's obligation is discharged by `stableSignature` and does not
  appear as open

#### Scenario: Signature change is not discharged

- **WHEN** the same dependency also changed its signature
- **THEN** the handler's obligation remains open unless explicitly acked, and the ack —
  if given — is recorded in the report

### Requirement: Admissibility verdict

With a spec file supplied, `graphos impact --gate` SHALL exit zero iff the change is
complete (all obligations discharged), the designated new spec holds on the after
projection, and every other spec in the file still holds on the after projection; the
report SHALL name each failed conjunct.

- **Plan**: Admissibility = completeness ∧ new intent realized ∧ no regression of
  validated intents — the CI form of "this change does what the ticket says and breaks
  no other ticket".
- **Do**: Conjunction over `Domain/Impact.hs` completeness and `Domain/IntentCheck.hs`
  verdicts; gate wiring in CLI.
- **Check**: Scenarios below.
- **Act**: Wire into CI beside `speccheck` once both are stable.

#### Scenario: Omission blocks admissibility

- **WHEN** a change adds a `schema-field` node with no `maps` edge while the spec file
  contains the schema↔contract `parallel` spec
- **THEN** `--gate` exits non-zero and the report names the violated spec as the failed
  conjunct

#### Scenario: Corrected change is admissible

- **WHEN** the same change also adds the corresponding `contract-field` node and `maps`
  edge, and all obligations are discharged
- **THEN** `--gate` exits zero
