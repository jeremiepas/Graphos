# progressive-graph-viewer

The viewer's remote mode: fetch-on-demand over `graph-slice-api` instead of an embedded
payload, with a client memory budget. The LOD phase model (overview of community dots →
per-community drill-down) is unchanged — only where the data comes from changes. Composes
with `viewer-navigation` (deep links address slices) and `html-viewer-interaction` (facets
apply to loaded slices).

## ADDED Requirements

### Requirement: Aggregates-first load

In remote mode the viewer SHALL boot from the overview aggregates endpoint alone — rendering
the community-dot overview without fetching any individual nodes or edges — and SHALL fetch
community slices only when the user drills down, neighborhood slices only on
expansion/selection. The viewer SHALL never request the full graph in any mode.

- **Plan**: The overview phase already renders purely from aggregates; remote mode makes
  that the *only* boot dependency.
- **Do**: Replace the interned-payload reads with a slice-backed data layer keyed by the
  graph hash.
- **Check**: Scenarios below.
- **Act**: If time-to-overview regresses versus the embedded small-graph path, prefetch the
  first page of the largest community in idle time and record the heuristic.

#### Scenario: Boot on a 4.4 GB graph

- **WHEN** a user opens the remote-mode viewer on a graph whose `graph.json` is 4.4 GB
- **THEN** the overview renders from the aggregates response alone, and no request for the
  full node or edge set is made at any point in the session

#### Scenario: Drill-down fetches one community

- **WHEN** the user drills into community 483
- **THEN** the viewer requests community-483 slice pages only, renders hubs-first as pages
  arrive, and displays "N of M members loaded" from the response totals

#### Scenario: Deep link fetches the addressed slice

- **WHEN** a `viewer-navigation` deep link addressing community 483 + node `mod_Auth` is
  opened in a fresh tab in remote mode
- **THEN** the viewer fetches the aggregates, then the community-483 slice (and the node's
  neighborhood if the node is outside the first pages), and restores the addressed position
  without loading any other community

### Requirement: Client memory budget with eviction

The viewer SHALL enforce a client-side budget on resident nodes and edges. When loading a
new slice would exceed the budget, the viewer SHALL evict least-recently-viewed slices
(never the currently viewed community, current selection, current path highlight, or pinned
nodes) and SHALL indicate evicted state so re-navigation visibly refetches. The budget SHALL
be a live-tunable parameter (see `live-render-tuning`).

- **Plan**: The wire is bounded by `graph-slice-api`; this bounds what accumulates across a
  long exploration session.
- **Do**: Slice-granular LRU over the data layer; eviction re-collapses evicted communities
  to their aggregate dots.
- **Check**: Scenarios below.
- **Act**: If eviction thrash occurs on budgets near a single community's size, floor the
  budget at max community page ×2 and surface a warning in the tuning panel.

#### Scenario: Eviction under budget pressure

- **WHEN** a user with a budget of 5,000 resident nodes has visited four communities of
  2,000 nodes each
- **THEN** the least-recently-viewed communities are evicted back to aggregate dots, the
  current community remains fully resident, and total resident nodes stay within budget

#### Scenario: Protected residents are never evicted

- **WHEN** eviction triggers while a cross-community path highlight is active
- **THEN** the path's nodes and the current selection survive eviction regardless of
  recency

#### Scenario: Re-navigation refetches

- **WHEN** the user returns to an evicted community
- **THEN** the viewer refetches its slice pages (a loading state is shown; nothing renders
  from stale evicted data)

### Requirement: Mode selection and export threshold

The HTML export SHALL choose the viewer mode by payload size: below a configurable threshold
it SHALL emit today's self-contained document (embedded payload, works on `file://`); above
it, it SHALL emit a remote-mode document — assets still inline, no embedded payload — plus a
clear in-page message when a remote-mode document is opened without a reachable server. A
CLI flag SHALL force either mode. Embedded-mode behavior remains exactly as specified by
`html-lod-viewer`.

- **Plan**: Keeps the small-graph/file:// experience byte-compatible while making large
  graphs possible at all.
- **Do**: Threshold check in the HTML export; remote documents carry the serve origin
  relative-path convention already used by the query navigator.
- **Check**: Scenarios below.
- **Act**: Record the chosen default threshold in the design doc after measuring parse-time
  on reference hardware; revisit if browsers move.

#### Scenario: Small graph stays self-contained

- **WHEN** the pipeline exports a graph whose payload is below the threshold
- **THEN** the emitted `graph.html` embeds the payload and works opened from `file://` with
  no server, unchanged from today

#### Scenario: Large graph exports remote-mode

- **WHEN** the pipeline exports a graph whose payload exceeds the threshold
- **THEN** the emitted document contains no embedded payload, boots from the aggregates
  endpoint under `graphos serve`, and shows an explicit "needs graphos serve" message when
  opened from `file://`

### Requirement: Load-state visibility

Remote mode SHALL make data state visible: per-community loaded fraction ("N of M"),
in-flight fetch indicators, distinct rendering for aggregate-only (unloaded) versus resident
communities, and non-blocking error toasts with retry on failed slice fetches. Stale data
from a previous graph hash SHALL never render silently: on hash mismatch the viewer SHALL
drop caches and reload aggregates.

- **Plan**: Partial data is the steady state at 4.4 GB; the UI must say so rather than look
  broken or silently wrong.
- **Do**: Drive all indicators from the data layer's slice registry and response totals.
- **Check**: Scenarios below.
- **Act**: If indicator churn distracts during fast paging, debounce the counters and note
  the interval.

#### Scenario: Partial community is labeled

- **WHEN** two of five pages of community 483 have arrived
- **THEN** the drill-down shows the loaded members plus a visible "800 of 2,000 loaded"
  affordance that fetches the next page

#### Scenario: Hash mismatch drops caches

- **WHEN** the server's graph is replaced mid-session and the next slice response carries a
  different hash
- **THEN** the viewer discards resident slices, reloads aggregates, and informs the user the
  graph changed
