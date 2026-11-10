## ADDED Requirements

### Requirement: Build drops self-edges to keep G_s irreflexive

`Graphos.UseCase.Build.buildGraph` SHALL drop a **self-edge** — an edge whose
`edgeSource e == edgeTarget e` — from its valid-edge set, alongside the existing
dangling-edge drop, in both the undirected and directed (`buildGraph True`) views. The
undirected support graph `G_s` SHALL remain irreflexive: no node is a member of its
own neighbour set (`adj v v == False`, Phase 1 `SupportGraph` contract). The dropped
node SHALL be preserved: a node whose only edge was a self-edge remains in `gNodes`
with `degree 0`. A real (non-self) edge incident to the same node is retained.
(PRD §3.2 Build, workflow 01 stage 3; AVI-1324, Lean seam `NoSelfLoops`.)

#### Scenario: Lone self-edge is dropped but the node is kept
- **WHEN** `buildGraph` is called on an extraction with a single node `a` and one self-edge `a → a`
- **THEN** `gNodes` still contains `a`, `neighbors g a` does not contain `a`, and `degree g a == 0`

#### Scenario: Real edge kept, coexisting self-edge dropped
- **WHEN** the extraction has edges `a → a` and `a → b`
- **THEN** `b` is a neighbour of `a`, `a` is not a neighbour of itself, and `degree g a == 1`

#### Scenario: Self-edge dropped in directed mode
- **WHEN** `buildGraph True` is called on a single node `a` with a self-edge `a → a`
- **THEN** `a` is not its own neighbour and `degree g a == 0`
