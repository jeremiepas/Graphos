# query-groups

Obsidian-style groups: a named query plus a color, kept live while exploring. Groups are
evaluated server-side over the full graph (not just resident slices), so membership and
counts are correct even for parts of a 4.4 GB graph the browser has never loaded.

## ADDED Requirements

### Requirement: Group definition and management

The viewer SHALL let the user create, edit, reorder and delete named groups, each defined by
a query string (the `/api/query` engine's language — same text a user would type in search)
and a color. The groups panel SHALL show each group's name, color, query and total member
count. Group evaluation SHALL be read-only — defining groups never mutates the graph.

- **Plan**: Mirrors Obsidian's graph-view groups (query → color) on the existing scored
  query engine, so any expressible search is a group.
- **Do**: Groups panel in the sidebar; evaluation via a group endpoint (below).
- **Check**: Scenarios below.
- **Act**: If users chiefly write facet-like queries (kind/filetype), add quick-templates in
  the panel rather than a second query language.

#### Scenario: Creating a group colors matches

- **WHEN** a user creates group "auth" with query `auth token` and the color orange
- **THEN** every resident node matching the query renders orange, and the panel shows the
  group's total match count over the whole graph

#### Scenario: Editing re-evaluates

- **WHEN** the user edits the "auth" query to `auth token refresh`
- **THEN** membership, colors and the count update without reloading the document

### Requirement: Server-side group evaluation

The server SHALL expose an endpoint that evaluates a group query and returns the matching
node ids (paged, bounded per `graph-slice-api` conventions) plus the total match count and
per-community match counts. The viewer SHALL color resident members from this membership,
SHALL show per-community group presence on the overview dots (so unloaded matches are
discoverable), and SHALL treat membership as stale on graph-hash change.

- **Plan**: Client-side evaluation over resident slices would silently miss every unloaded
  match — wrong on any graph that needs remote mode. Reuses
  `queryGraphWithIndexScored`.
- **Do**: One evaluation endpoint; the viewer joins membership against resident nodes and
  aggregates.
- **Check**: Scenarios below.
- **Act**: If evaluating many groups on every graph change is costly, cache evaluations by
  (graph hash, query) server-side and report the hit rate.

#### Scenario: Counts cover unloaded regions

- **WHEN** group "auth" matches 3,000 nodes of which only 200 are resident in the browser
- **THEN** the panel shows 3,000, and overview dots of communities containing matches
  indicate the group's presence even for communities never drilled into

#### Scenario: Bounded membership pages

- **WHEN** a group matches more nodes than the server response cap
- **THEN** membership arrives paged with totals and explicit truncation, per
  `graph-slice-api` conventions

### Requirement: Group precedence and visibility semantics

When a node matches several groups, the first matching group in panel order SHALL determine
its color (Obsidian semantics); reordering groups SHALL re-color accordingly. Each group
SHALL support hide (members rendered dimmed/excluded) and isolate (only members of isolated
groups rendered, composing with active facets conjunctively). Group coloring SHALL take
precedence over community coloring while at least one group is enabled.

- **Plan**: First-match-wins is the semantics Obsidian users already know; isolate is the
  "find node groups" ask — carve the cohort out of a huge graph.
- **Do**: Precedence resolved client-side from panel order over the joined memberships.
- **Check**: Scenarios below.
- **Act**: If first-match-wins confuses (nodes "missing" from later groups), show
  multi-membership in the node detail panel rather than changing color semantics.

#### Scenario: First match wins

- **WHEN** node `fn_verifyToken` matches groups "auth" (position 1, orange) and "tests"
  (position 2, grey)
- **THEN** it renders orange; moving "tests" to position 1 re-renders it grey

#### Scenario: Isolate carves the cohort

- **WHEN** the user isolates group "auth" while the `code` facet is active
- **THEN** only resident nodes that are both group members and facet matches render, and the
  overview highlights only communities containing members

### Requirement: Group persistence and portability

Group definitions (name, query, color, order, hide/isolate flags) SHALL persist per graph
(keyed by graph hash) across sessions on the same browser, SHALL survive reload and
graph-hash change (definitions persist; memberships re-evaluate), and SHALL be exportable
and importable as a JSON document so a group set can be shared or versioned alongside the
corpus.

- **Plan**: Groups are analysis artifacts; losing them on reload would make them toys.
- **Do**: localStorage keyed by hash for persistence; explicit export/import for sharing
  (group sets are too large for the `viewer-navigation` URL hash).
- **Check**: Scenarios below.
- **Act**: If teams want server-side shared group sets, add a follow-up storing them under
  `graphos-out/` — out of scope here.

#### Scenario: Groups survive reload

- **WHEN** a user defines three groups and reloads the document
- **THEN** the three groups reappear with their colors and order, and memberships
  re-evaluate against the current graph

#### Scenario: Export and import round-trip

- **WHEN** a user exports their group set and imports it in another browser viewing the same
  graph
- **THEN** the imported panel shows the same groups in the same order with the same colors
