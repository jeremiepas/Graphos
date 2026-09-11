# conflict-adjudication

The only stage where a model judges — and only ever two artifacts at a time, on
candidates the graph surfaced. Small-model-sized by construction.

## ADDED Requirements

### Requirement: Pairwise adjudication of surfaced candidates

The `speccheck --adjudicate` mode SHALL send each contradiction/duplication candidate
pair to the configured model as one structured-output call containing exactly the two
artifacts' bodies and their shared context (the shared target's label for contradiction
candidates), asking for a verdict (`conflict` | `compatible` | `duplicate`) with a
one-sentence rationale citing both artifacts. Verdicts SHALL be recorded per pair in the
report with the model id; a pair the model fails to answer in valid schema after the
configured retries SHALL be reported as `unadjudicated`, never silently dropped or
defaulted to either verdict.

- **Plan**: Two-document questions are what a local 7–27B answers reliably; the
  structured-output schema plus retry mirrors the extraction harness discipline.
- **Do**: Prompt + schema in UseCase; provider via the existing OpenAI-compatible client
  (llama.cpp local by default per host config).
- **Check**: Scenarios below.
- **Act**: If a model's confirm/dismiss quality is poor on the harness corpus, gate on
  candidates-only (no adjudication) rather than trusting bad verdicts — the graph stage
  degrades gracefully to "here are the pairs, judge them yourself".

#### Scenario: Bounded context per call

- **WHEN** adjudication runs over 30 candidate pairs
- **THEN** every model call contains exactly two artifact bodies, and 30 calls are made

#### Scenario: Schema failure is visible

- **WHEN** the model returns unparseable output for a pair after retries
- **THEN** the pair is reported `unadjudicated` and counted, and the exit code treats it
  as non-gating

### Requirement: Gate discipline — only confirmed conflicts block

The speccheck exit gate SHALL block only on pairs adjudicated `conflict` (and structural
certainties: cycles); `compatible`, `duplicate`, `unadjudicated` and raw candidates SHALL
appear in the report without gating (duplicates gate under `--strict-duplicates`). Every
blocking pair SHALL be traceable in the report to the candidate that surfaced it — the
gate can never block on a pair the graph did not surface (Lean model: confirmed ⊆
candidates).

- **Plan**: The trust chain stays: graph surfaces (complete for the shared-target
  pattern), model confirms (bounded), gate blocks (subset). No stage can exceed the one
  before it.
- **Do**: Gate as a pure filter over adjudicated candidates, mirrored in the Lean model.
- **Check**: Scenarios below.
- **Act**: If teams want human sign-off instead of model verdicts for gating, add a
  `--require-ack findings.json` mode — the subset discipline is unchanged.

#### Scenario: Confirmed conflict blocks with trace

- **WHEN** the model confirms one of three candidate pairs as a conflict
- **THEN** the exit code is non-zero, the report marks exactly that pair blocking, and
  the pair's entry names the shared target that surfaced it

#### Scenario: Dismissed candidates do not gate

- **WHEN** all candidates are adjudicated `compatible`
- **THEN** the report retains them with rationales and the exit code is zero
