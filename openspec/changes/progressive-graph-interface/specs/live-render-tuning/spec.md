# live-render-tuning

Obsidian-style live parameters: a tuning panel whose changes apply to the running canvas
immediately — no reload, no re-mount, no rebuild. Today these values are compile-time
constants in the viewer assets.

## ADDED Requirements

### Requirement: Live physics parameters

The viewer SHALL expose physics parameters — at minimum gravity/central pull, link distance,
repulsion strength, and a physics on/off toggle — as controls whose changes apply to the
running simulation immediately. The solver family per LOD phase (forceAtlas2Based on the
overview, per `html-lod-viewer`) SHALL remain fixed; tuning adjusts its parameters, not the
solver.

- **Plan**: The Obsidian sliders users reach for first; vis-network accepts options updates
  on a live network.
- **Do**: Drive from a single tuning-state object applied via the network's options path;
  re-enable stabilization only when physics is toggled on.
- **Check**: Scenarios below.
- **Act**: If option updates cause visible re-layout jank on large drill-downs, batch slider
  input at trailing-edge and record the debounce.

#### Scenario: Slider moves the running layout

- **WHEN** the user drags the link-distance slider while a community drill-down is rendered
- **THEN** the layout visibly adjusts without a page reload, canvas re-mount, or loss of the
  current selection

#### Scenario: Physics toggle

- **WHEN** the user toggles physics off after stabilization
- **THEN** node positions freeze immediately, and dragging nodes no longer wakes the
  simulation

### Requirement: Live visual parameters

The viewer SHALL expose visual parameters applying immediately: node size scaling by degree
(including off), label visibility threshold (by zoom or degree), edge opacity, and arrow
visibility. Visual parameters SHALL apply to already-rendered elements, not only to nodes
rendered after the change.

- **Plan**: Density control is what makes a huge graph readable; Obsidian's node-size and
  text-fade sliders are the model.
- **Do**: Restyle datasets in place from the tuning state.
- **Check**: Scenarios below.
- **Act**: If in-place restyle of very large drill-downs stalls the frame, restyle in chunks
  on idle callbacks and note the chunk size.

#### Scenario: Degree scaling applies in place

- **WHEN** the user enables node-size-by-degree on a rendered drill-down
- **THEN** existing hubs grow and leaves shrink without refetching or re-adding nodes

#### Scenario: Label threshold declutters

- **WHEN** the user raises the label threshold on a dense view
- **THEN** labels below the threshold disappear immediately and reappear when the threshold
  is lowered back

### Requirement: Live budget parameters

The viewer SHALL expose the remote-mode budgets as live parameters: slice page size (within
the server hard cap), the client resident-node budget (see `progressive-graph-viewer`), and
the LOD drill-down render cap. Changes SHALL apply to subsequent fetches and evictions
without discarding already-resident data (except when lowering the resident budget below
current residency, which SHALL trigger normal eviction).

- **Plan**: Different machines and graphs want different budgets; fixing them server-side
  would make the 4.4 GB experience one-size-fits-none.
- **Do**: Budgets live in the same tuning state; the data layer reads them per operation.
- **Check**: Scenarios below.
- **Act**: If users set self-harming budgets, clamp with visible bounds in the panel rather
  than failing fetches.

#### Scenario: Raising page size affects the next fetch

- **WHEN** the user raises slice page size from 200 to 500 mid-session
- **THEN** the next slice request uses 500 (or the server cap if lower) and already-resident
  pages are untouched

#### Scenario: Lowering the resident budget evicts

- **WHEN** the user lowers the resident-node budget below current residency
- **THEN** LRU eviction runs to the new budget under the protection rules of
  `progressive-graph-viewer`

### Requirement: Persistence, defaults and reset

Tuning state SHALL persist per graph (keyed by graph hash) across sessions, SHALL load
before first render so a tuned experience is stable from boot, and SHALL offer a one-click
reset to the documented defaults. Defaults SHALL equal today's compiled-in values so an
untouched panel changes nothing.

- **Plan**: Tuning that resets on every reload would be worse than constants; defaults
  matching today keeps this capability strictly additive for users who never open the
  panel.
- **Do**: Same persistence pattern as `query-groups` (localStorage by hash); a defaults
  table in one place, shared by panel rendering and reset.
- **Check**: Scenarios below.
- **Act**: If per-graph persistence surprises users switching corpora, add a "use as global
  default" affordance later.

#### Scenario: Tuning survives reload

- **WHEN** a user tunes link distance and node scaling, then reloads
- **THEN** the first render already uses the tuned values

#### Scenario: Reset restores documented defaults

- **WHEN** the user clicks reset after arbitrary tuning
- **THEN** every parameter returns to the documented default and the canvas reflects it
  immediately
