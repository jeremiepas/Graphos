# studio-subgraph-extract Specification

## Purpose
TBD - created by archiving change elm-graph-studio. Update Purpose after archive.
## Requirements
### Requirement: Extraction scopes

The studio SHALL build an extraction scope from any of: a community, a group's members, a
node neighborhood (with depth), the current manual selection (multi-select on canvas and in
lists), or the union of several of these — and SHALL show the scope's node and edge counts
live as it is composed. Edges SHALL be included when both endpoints are in scope, plus
optionally (user toggle) boundary edges to one hop outside with their endpoint nodes.

- **Plan**: The scopes mirror how users already think in the studio — communities,
  groups, neighborhoods, selections — so extraction is one action on any of them.
- **Do**: Scope = a set algebra over node ids in the Model; counts derive from it.
- **Check**: Scenarios below.
- **Act**: If scope composition wants difference/intersection too, extend the algebra —
  the UI grows from the same scope type.

#### Scenario: Community plus neighborhood scope

- **WHEN** a user adds community 483 and the depth-2 neighborhood of `doc_readme` to the
  scope
- **THEN** the scope shows the union's node and edge counts before anything is exported

#### Scenario: Boundary toggle

- **WHEN** the user enables boundary edges on a single-community scope
- **THEN** the counts grow by the one-hop boundary nodes and edges, and disabling restores
  the internal-only counts

### Requirement: Export in the canonical view shape

The studio SHALL export the scope as a JSON document in the `json-graph-web-view` canonical
shape — vis-network `{nodes, edges}` with string ids, node `label`/`color`/community
metadata and edge `relation` preserved — with a user-supplied title, and SHALL download it
via the browser. The exported document SHALL be accepted unchanged by the views catalog
(placed in `graphos-out/views/`, it lists and renders under `graphos serve`). Group colors
active in the studio SHALL be optionally baked into node colors at export.

- **Plan**: One canonical shape already exists for ad-hoc graph JSON in this project;
  extraction targets it so every existing consumer (catalog, viewer) works day one, and no
  server write surface is needed.
- **Do**: Scope → canonical-shape encoder; download port; optional group-color baking at
  encode time.
- **Check**: Scenarios below.
- **Act**: Server-side saving (POST to the catalog) is deferred; if demanded, it is a
  `json-graph-web-view` extension, not a studio-side invention.

#### Scenario: Round-trip through the views catalog

- **WHEN** a user extracts community 483 as "auth-core" and places the downloaded file in
  `graphos-out/views/`
- **THEN** `graphos serve`'s views catalog lists "auth-core" and renders it without
  modification

#### Scenario: Group colors can be baked

- **WHEN** the user exports a scope with the "bake group colors" option while group "auth"
  (orange) is active
- **THEN** nodes matching "auth" carry orange in the exported JSON, and without the option
  they carry their community colors

### Requirement: Extraction preview

The studio SHALL preview the scoped subgraph before export — rendered on the canvas with
out-of-scope elements dimmed, and an isolated preview mode showing only the scope — so what
is exported is exactly what is seen. The preview SHALL be dismissible back to the prior view
without loss of navigation state.

- **Plan**: Extraction without preview produces surprises at 4.4 GB scale; the dim/isolate
  pattern reuses the group-visibility rendering path.
- **Do**: Scope drives the same visibility mechanism groups use; dismiss restores the prior
  position via the navigation model.
- **Check**: Scenarios below.
- **Act**: If preview of very large scopes strains the canvas, cap the preview render with
  an explicit "previewing N of M" note — the export itself stays complete.

#### Scenario: Preview isolates the scope

- **WHEN** the user enters isolated preview on a composed scope
- **THEN** only scope members render, the counts match the scope panel, and dismissing
  returns to the exact prior view

#### Scenario: What is previewed is what exports

- **WHEN** the user exports directly from an isolated preview
- **THEN** the exported document contains exactly the previewed nodes and edges

