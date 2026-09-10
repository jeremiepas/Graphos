# viewer-navigation

URL-addressable viewer state, history-integrated back/forward, breadcrumb trail, keyboard
navigation and in-viewer shortest-path highlighting for the graph view served at
`graphos serve` (default `http://localhost:8080/`). Additive to `html-viewer-interaction`
and `navigator-query-view`; consumes the single-dispatcher state those capabilities already
maintain (`depth`, `selection`, `hops`, `facets`).

## ADDED Requirements

### Requirement: URL-addressable view state

The viewer SHALL encode the navigation state — depth phase, selected community or node, hop
radius, and active facets — in the URL hash fragment on every state change, and SHALL restore
that state when a document is opened with a populated hash. URL state SHALL take precedence
over sessionStorage state; sessionStorage SHALL remain the fallback when the hash is empty.
Restoring SHALL apply the same stale-reference check as session restore: a hash referencing an
unknown node or community SHALL fall back to the Overview with no error dialog. Encoding and
restoring SHALL work identically over `graphos serve` and `file://` (hash only — no server
routes, no fetch).

- **Plan**: The state object already exists and is serialized (sessionStorage, <4 KB guard);
  the hash codec is a second serialization target for the same object.
- **Do**: Encode state into the hash from the dispatcher; on load, prefer hash → session →
  initial. Guard oversized facet sets the same way session persistence guards quota.
- **Check**: Scenarios below.
- **Act**: If facet sets routinely blow a sane hash length, encode facets by stable index
  rather than name and record the decision in the design doc.

#### Scenario: Deep link restores position

- **WHEN** a user opens a URL whose hash encodes drill-down into community 483 with node
  `mod_Auth` selected and the `doc` file-type facet active
- **THEN** the viewer renders community 483's drill-down with `mod_Auth` selected, its detail
  panel open, and the `doc` facet applied — without any intermediate Overview interaction

#### Scenario: URL reflects navigation

- **WHEN** a user drills from Overview into a community and selects a node
- **THEN** the address bar hash identifies that community and node, and copying the URL into a
  new tab reproduces the same view

#### Scenario: Stale deep link degrades to Overview

- **WHEN** a user opens a hash referencing a node id that no longer exists in the loaded graph
- **THEN** the viewer renders the Overview with default state and no error dialog

#### Scenario: Works on file://

- **WHEN** a deep-linked document is opened from `file://`
- **THEN** the encoded state is restored with no network request and no page reload

### Requirement: History-integrated back and forward

The viewer SHALL push a history entry for every navigation transition — depth change or
selection change — such that browser Back and Forward traverse previously visited positions
instead of leaving the page. Facet toggles and hop changes SHALL update the current entry in
place rather than pushing, so history steps correspond to positions, not filter tweaks. The
viewer SHALL also render on-screen back/forward controls that mirror browser history and are
disabled when no entry exists in that direction.

- **Plan**: The single dispatcher is the only mutation path, so classifying actions as
  push-vs-replace covers every transition; hash encoding (above) gives history entries their
  addresses for free.
- **Do**: Route SET_DEPTH/SET_SELECTION through history-pushing navigation; route
  SET_FACETS/TOGGLE_FACET/SET_HOPS through in-place replacement; render both controls in the
  toolbar.
- **Check**: Scenarios below.
- **Act**: If history-entry proliferation from rapid neighbour-chip clicking becomes noisy,
  debounce consecutive selections of the same kind and note the threshold.

#### Scenario: Back returns to previous position

- **WHEN** a user drills from Overview into community 483, selects `mod_Auth`, then presses
  browser Back twice
- **THEN** the first Back returns to community 483 with no node selected and the second Back
  returns to the Overview — the page is never unloaded

#### Scenario: Forward replays a step back

- **WHEN** a user has stepped Back from a node selection
- **THEN** pressing Forward re-selects that node and reopens its detail panel

#### Scenario: Facet toggles do not pollute history

- **WHEN** a user toggles four facets while viewing a community, then presses Back once
- **THEN** the viewer returns to the position visited before entering that community, not to a
  prior facet combination

#### Scenario: On-screen controls mirror history

- **WHEN** a user has just loaded the viewer with an empty history
- **THEN** the on-screen back control is disabled, and it becomes enabled after the first
  drill-down

### Requirement: Breadcrumb trail

The viewer SHALL display a breadcrumb of the current position with up to three segments —
Overview, the selected community's label, and the selected node's label — updating on every
navigation transition including deep-link restore. Each ancestor segment SHALL be clickable
and SHALL navigate to that level (Overview, or the community drill-down with selection
cleared). The current (last) segment SHALL NOT be a link. Community segments SHALL show the
community label, falling back to its id when no label exists.

- **Plan**: Depth, selection and the community aggregates needed for labels are already in
  the state and payload; the breadcrumb is a pure derivation of them.
- **Do**: Render the trail into a dedicated container above the canvas; drive clicks through
  the same history-pushing navigation as any other transition.
- **Check**: Scenarios below.
- **Act**: If long node labels overflow the trail, truncate with an ellipsis and full label
  on hover rather than wrapping.

#### Scenario: Trail reflects drill-down

- **WHEN** a user is in community `Logging` with node `resolveLoggingConfig` selected
- **THEN** the breadcrumb reads Overview ▸ `Logging` ▸ `resolveLoggingConfig`, with the first
  two segments clickable

#### Scenario: Clicking an ancestor navigates

- **WHEN** the user clicks the community segment of the breadcrumb while a node is selected
- **THEN** the viewer shows that community's drill-down with no node selected, and the step is
  a history entry (Back returns to the node)

#### Scenario: Trail present after deep-link restore

- **WHEN** a deep link restores a community + node position
- **THEN** the breadcrumb immediately shows all three segments without further interaction

### Requirement: Keyboard navigation map

The viewer SHALL provide keyboard shortcuts: one key to focus the search input, Escape as
hierarchical dismissal (close the open detail panel first; at a community with no panel open,
go up to Overview; at Overview, clear active search highlighting), one key to fit the graph to
the viewport, and one key to open an overlay listing all shortcuts. All shortcuts except
Escape SHALL be inert while focus is inside a text input. The overlay SHALL close on Escape or
outside click and SHALL be generated from the same table that binds the handlers, so the two
cannot drift.

- **Plan**: Only Escape-in-search exists today; the dispatcher gives each shortcut a single
  entry point, and vis-network's built-in pan/zoom keys remain untouched.
- **Do**: One document-level key handler dispatching against a declarative shortcut table;
  overlay rendered from that table.
- **Check**: Scenarios below.
- **Act**: If chosen keys collide with browser or vis-network defaults on any supported
  platform, rebind in the table and the overlay follows automatically.

#### Scenario: Focus search from anywhere

- **WHEN** a user presses the search shortcut while no input is focused
- **THEN** the search input receives focus and the key is not typed into it

#### Scenario: Escape walks up the hierarchy

- **WHEN** a user in a community drill-down with a node detail panel open presses Escape twice
- **THEN** the first press closes the panel (selection cleared, community unchanged) and the
  second returns to the Overview

#### Scenario: Shortcuts inert while typing

- **WHEN** a user types a character bound to a shortcut into the search input
- **THEN** the character appears in the input and no navigation occurs

#### Scenario: Help overlay lists bindings

- **WHEN** a user presses the help shortcut
- **THEN** an overlay lists every active shortcut with its action, and Escape closes only the
  overlay

### Requirement: In-viewer shortest path between two nodes

The viewer SHALL let a user mark a path start and a path end from the node detail panel, and
on both being set SHALL compute a shortest path between them over the loaded graph
client-side — no HTTP request — treating edges as undirected for reachability, consistent
with CLI `graphos path` (spec `05-path`). The resulting path SHALL be highlighted using the
existing subgraph-highlight mechanism, with nodes outside the path dimmed, and a summary
(hop count, endpoints) SHALL be shown with a single-action clear that restores the previous
view. When no path exists the viewer SHALL say so explicitly and SHALL NOT alter the current
highlighting. When the two endpoints live in different communities, the path view SHALL
render the involved nodes regardless of the current drill-down scope.

- **Plan**: The full node/edge payload is already embedded client-side (the served document
  carries the whole graph), so breadth-first search needs no server support; highlight and
  reset primitives already exist.
- **Do**: Two panel affordances writing path endpoints into viewer state; BFS over the
  loaded adjacency; render via the existing highlight path, bypass facet filtering for path
  members.
- **Check**: Scenarios below.
- **Act**: If BFS latency on the largest reference corpus exceeds the interaction budget,
  precompute the adjacency index once at load and record the memory cost.

#### Scenario: Path found and highlighted

- **WHEN** a user marks node A as path start and node B as path end, and an undirected path
  A→…→B exists in the loaded graph
- **THEN** the viewer highlights exactly the nodes and edges of one shortest path, dims the
  rest, and reports the hop count

#### Scenario: No path exists

- **WHEN** the two marked endpoints belong to disconnected components
- **THEN** the viewer states that no path exists and the current view's highlighting is
  unchanged

#### Scenario: Cross-community path is visible

- **WHEN** the marked endpoints belong to two different communities while the user is in one
  community's drill-down
- **THEN** the path view shows the full path including nodes from the other community

#### Scenario: Clearing the path restores the view

- **WHEN** a user activates the clear-path action after a path is highlighted
- **THEN** the highlight is removed and the viewer shows the same position (depth, selection,
  facets) it showed before the path was requested
