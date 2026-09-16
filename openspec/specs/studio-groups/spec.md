# studio-groups Specification

## Purpose
TBD - created by archiving change elm-graph-studio. Update Purpose after archive.
## Requirements
### Requirement: Groups with query-groups semantics

The studio SHALL provide named query+color groups following the `query-groups` capability's
semantics: ordered panel with first-match-wins coloring over community colors, per-group hide
and isolate composing conjunctively with other filters, live counts, persistence keyed by
graph identity, and JSON import/export interchangeable with the viewer's group sets.

- **Plan**: One group semantics across viewer and studio — a group set exported from either
  loads in the other, because both target the same definition shape.
- **Do**: Reuse the exported group-set JSON schema; the studio's panel is design-system
  components over the same ordered-definitions model.
- **Check**: Scenarios below.
- **Act**: If studio-only group features emerge (e.g. scope-from-group is already in
  `studio-subgraph-extract`), keep the definition shape shared and extend only studio-side
  presentation.

#### Scenario: Viewer group sets load in the studio

- **WHEN** a group set exported from the graph.html viewer is imported in the studio against
  the same graph
- **THEN** the same groups appear in the same order with the same colors and equivalent
  membership

#### Scenario: Precedence and isolate behave as specified

- **WHEN** a node matches groups at positions 1 and 2 and the user isolates position 2
- **THEN** the node renders (it is a member), colored by position 1 (first match wins), and
  non-members of group 2 are hidden

### Requirement: Dual-mode evaluation

The studio SHALL evaluate group queries through the server's group-evaluation endpoint when
connected to a server exposing it (full-graph counts, per-community rollups, unloaded-region
awareness per `query-groups`), and SHALL fall back to local evaluation over the loaded graph
— matching against node label, source path and kind — in file mode or against servers
without the endpoint. The panel SHALL state which evaluation mode produced the current
memberships, and counts from local evaluation SHALL be labeled as covering the loaded graph
only when the graph is partially resident.

- **Plan**: Correct-and-total when the server can do it; honest-and-local when it cannot.
  Silent wrong counts are the failure mode this requirement forbids.
- **Do**: Evaluation behind the data-source interface; a mode badge on the panel.
- **Check**: Scenarios below.
- **Act**: If local evaluation's simpler matching diverges confusingly from the server's
  scored engine, constrain local group queries to the documented subset and say so in the
  panel.

#### Scenario: Connected evaluation covers unloaded regions

- **WHEN** a connected studio (slice server) defines a group matching 3,000 nodes with 200
  resident
- **THEN** the panel shows 3,000 with server-evaluation indicated, and overview dots show
  per-community presence

#### Scenario: File-mode evaluation is labeled local

- **WHEN** a file-mode user defines a group
- **THEN** membership is computed from the loaded file, and the panel indicates local
  evaluation with no unloaded-region claim

#### Scenario: Editing re-evaluates groups

- **WHEN** a node is relabeled such that it newly matches an existing group's query
- **THEN** group membership and counts update after the edit in both evaluation modes

