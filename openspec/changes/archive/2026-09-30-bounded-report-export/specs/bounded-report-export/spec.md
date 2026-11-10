## Purpose

Bounded-report-export: the report/export stage runs in bounded time and
memory on inference-enriched graphs. Whole-graph analysis (FGL conversion,
articulation points, biconnected components, cohesion, top-N selections) is
computed exactly once, in the analysis stage, and Steps 6–7 only render and
write precomputed values. (Workflow 01 Steps 6–7; PRD §16.1 scale targets,
§12 export formats; incident baseline 2026-09-29: 20+ min / 25.7 GB RSS on a
16.8k-node, 410k-edge graph.)

## ADDED Requirements

### Requirement: Single-pass whole-graph connectivity analysis

The pipeline SHALL convert the enriched graph to its FGL form at most once
per run. Articulation points and the biconnected-component count SHALL be
computed once from that shared conversion and carried on the analysis result;
the report, community aggregates, and the Neo4j/Memgraph subgraph pushes
SHALL consume those shared values and SHALL NOT recompute them.

#### Scenario: Report consumes shared connectivity values
- **WHEN** the pipeline reaches report generation on an enriched graph
- **THEN** the report's articulation-point and biconnected-component figures
  come from the analysis record populated during clustering
- **AND** no additional FGL conversion of the enriched graph occurs during
  report rendering

#### Scenario: Subgraph push reuses shared articulation points
- **WHEN** `--neo4j-push-mode subgraph` (or the Memgraph equivalent) runs
- **THEN** the bridge-node list passed to the push equals the analysis
  record's articulation list, with no recomputation on the export path

### Requirement: Bounded report rendering

Report rendering SHALL NOT materialize or sort full O(E) candidate lists.
Top-N selections (surprising connections, suggested questions) SHALL use a
size-bounded selection with output identical to sorting the full list and
taking N. Cohesion values printed by the report SHALL be read from the
cohesion map computed during clustering, not recomputed per community. The
biconnected-component figure SHALL be computed as a count without
materializing component member lists.

#### Scenario: Top-N selection equals the sorted reference
- **WHEN** surprising connections are selected from any edge list, including
  one with tied scores
- **THEN** the selected entries and their order are identical to taking the
  first N of the fully sorted candidate list

#### Scenario: Large-graph report stays bounded
- **WHEN** the pipeline runs on a graph of ~17k nodes and ~410k edges
  (the 2026-09-29 baseline corpus)
- **THEN** Steps 6–7 complete in under 60 seconds with peak process RSS under
  8 GB (recorded via `--rts-profile`)

### Requirement: Analysis fully evaluated in the Analyze stage

The Analyze stage SHALL fully evaluate the analysis result (god nodes,
surprising connections, suggested questions, connectivity artifacts) before
the pipeline proceeds. Report generation and export SHALL perform rendering
and IO only. A heap exhaustion caused by analysis work SHALL therefore be
attributed to the `analyze` stage by the memory-budget-guard, never to
`export`.

#### Scenario: Analysis cost lands in the analyze stage
- **WHEN** the heap budget is exhausted while computing surprising
  connections
- **THEN** the stage-named failure (memory-budget-guard) names `analyze`

#### Scenario: Export renders pre-evaluated values
- **WHEN** Step 7 begins
- **THEN** forcing the analysis record performs no further computation
  (already in normal form)
