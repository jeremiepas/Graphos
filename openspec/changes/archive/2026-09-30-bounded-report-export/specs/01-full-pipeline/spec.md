## ADDED Requirements

### Requirement: Workflow 01 — analysis completes in Stage 5, Stages 6–7 are bounded

Stage 5 (Analyze) SHALL complete with the analysis result fully evaluated
(god nodes, surprising connections, suggested questions, articulation points,
biconnected-component count). Stages 6 (Report) and 7 (Export) SHALL perform
no whole-graph analysis: their work is bounded by the size of the rendered
outputs plus streaming IO, and they SHALL log a single INFO line with the
report render duration and the shared-analysis counts. (Workflow 01;
PRD §16.1 scale targets)

#### Scenario: No whole-graph analysis after Stage 5
- **WHEN** Stage 6 starts on an inference-enriched graph
- **THEN** no FGL conversion, articulation-point, biconnectivity, cohesion,
  or O(E) candidate-list computation runs during Stages 6–7

#### Scenario: Report stage is observable
- **WHEN** the report is rendered
- **THEN** an INFO line states the render duration and the community,
  articulation-point, and biconnected-component counts
