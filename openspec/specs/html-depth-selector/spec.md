# html-depth-selector Specification

## Purpose
Carry the depth-selector capability from `add-profondeur-view-selector`, which this change
supersedes. That change is entirely unimplemented (0/24 tasks) and confined to the same module
this refonte rewrites; shipping both would mean two conflicting rewrites of `HTML.hs`. The
requirements below are the condensed form of its `html-depth-selector` delta, restated against
the refonted viewer (assets-based, facet-driven) rather than against the string-literal viewer.
## Requirements
### Requirement: Depth selector control replaces the two-phase back button

The viewer SHALL expose a depth control offering `Overview`, `Community`, `Full` and
`Custom` (N-hop neighbourhood), defaulting to `Overview`. Switching depth SHALL destroy the
previous renderer instance before creating the next, leaving no overlapping canvases. The former
back-button affordance SHALL be removed, with no dead DOM element left behind.

- **Plan**: The current viewer has a hardcoded `currentPhase = 'overview' | 'drilldown'`
  (`HTML.hs:189`) and a `btnBack` element (`HTML.hs:128`, `:393`, `:488`, `:780`, `:797`); depth
  becomes an explicit, persisted view state alongside the facet state.
- **Do**: Model depth as part of the single view-state object introduced by
  `html-viewer-interaction`; render through one dispatcher.
- **Check**: Scenarios below.
- **Act**: If `Full` depth stalls above a node threshold on the reference corpus, gate it behind
  a confirmation showing the node count rather than removing the level.

#### Scenario: Four depth levels are offered

- **WHEN** the viewer loads
- **THEN** the depth control offers `Overview`, `Community`, `Full` and `Custom`, with `Overview`
  selected

#### Scenario: Switching depth leaves one canvas

- **WHEN** a user switches from `Overview` to `Full` and back
- **THEN** exactly one renderer instance and one canvas exist at all times

#### Scenario: No dead back button

- **WHEN** the emitted document is inspected
- **THEN** no back-button element or handler is present

### Requirement: Custom depth performs an N-hop neighbourhood expansion

`Custom` depth SHALL expand an N-hop neighbourhood around a selected node, with N configurable
from 1 to 6 and defaulting to 2, computed client-side over the embedded payload. The resulting
node set SHALL equal the set returned by `graphos neighbors <id> --depth N` for the same graph.

#### Scenario: Neighbourhood matches the CLI

- **WHEN** a user selects a node and chooses `Custom` depth with N = 2
- **THEN** the rendered node set equals the output of `graphos neighbors <id> --depth 2`

#### Scenario: Hop bounds are enforced

- **WHEN** a user attempts to set N outside 1–6
- **THEN** the control clamps the value to the allowed range

#### Scenario: Large expansions are signalled

- **WHEN** an expansion would render more than 2,000 nodes
- **THEN** the viewer warns before rendering

### Requirement: Depth and facet state persist across reload

The viewer SHALL persist the selected depth, the selected community or node, the hop count and the
active facet filters in `sessionStorage`, and SHALL restore them on reload. Persisted state SHALL
remain under 4 KB.

#### Scenario: State survives reload

- **WHEN** a user selects `Custom` depth with N = 3 around a node, enables two facets, and
  reloads the page
- **THEN** the viewer restores that depth, node, hop count and facet selection

#### Scenario: Stale references degrade safely

- **WHEN** persisted state references a node or community absent from the current graph
- **THEN** the viewer falls back to `Overview` without error

