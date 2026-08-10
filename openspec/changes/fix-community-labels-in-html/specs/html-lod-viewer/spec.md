# HTML Level-of-Detail Viewer — Delta

## Purpose

Fix the LOD viewer so the community-dot overview and the sidebar community list display LLM-generated community labels (from the `community_labels` passed to `exportHTML`) instead of the hardcoded `"Community <id>"` fallback. Falls back to `"Community <id>"` only when no label is available for a given community.

## MODIFIED Requirements

### Requirement: Community aggregate dataset in graph.json

The JSON export SHALL include a top-level `community_aggregates` key containing one entry per detected community with the fields: `id`, `member_count`, `cohesion`, `bridge_count`, `color`, `label`, `representative_labels` (up to 3 member labels), and `inter_community_edges` (list of target community IDs with edge counts). This dataset SHALL be the data source for the overview phase. The `label` field SHALL be the LLM-generated label when `community_labels` is provided for that community id, and SHALL fall back to `"Community <id>"` when no label is available (absent `community_labels` map, or community id not in the map). The same labeled dataset SHALL be embedded inline in `graph.html` so the HTML viewer displays the labels without reading `graph.json`.

- Plan: ensure the HTML viewer shows the labels the user paid LLM tokens to produce, instead of the placeholder.
- Do: thread `Maybe (Map CommunityId Text)` from `exportAll` → `epExportHTML` → `communityAggregatesToJSON`; use the label when present, fall back otherwise.
- Check: the scenarios below verify the label appears in the embedded HTML data and the sidebar.

#### Scenario: Aggregate fields populated with LLM label

- **WHEN** a community with id 4 and 17 members is exported with `community_labels = {4: "Authentication Module"}`
- **THEN** its aggregate entry has `label = "Authentication Module"` (not `"Community 4"`), and the same `label` appears in the `_communityAggregatesData` embedded in `graph.html`

#### Scenario: Fallback when no labels provided

- **WHEN** a community with id 7 is exported with `community_labels = Nothing` (or a map that does not contain key 7)
- **THEN** its aggregate entry has `label = "Community 7"` (the fallback), both in `graph.json` and in `graph.html`'s embedded data

#### Scenario: HTML viewer shows label in sidebar

- **WHEN** `graph.html` is opened in a browser and the embedded `_communityAggregatesData` contains an entry with `label = "Authentication Module"`
- **THEN** the sidebar community list renders that entry with the text `"Authentication Module"` (from `c.label`) rather than `"Community 4"`, and the overview dot's tooltip includes the label

### Requirement: Drill-down reuses member nodes with community color

The drill-down phase SHALL render member nodes using their community color (from the joined `nodeCommunityId`) and SHALL include edges internal to the community plus bridge edges (edges where one endpoint is in the expanded community and the other is in a different community, shown as dashed lines to the collapsed community dots). The `phaseHint` text shown when drilling into a community SHALL use the community's label (LLM label when available, `"Community <id>"` fallback) instead of only the numeric id.

#### Scenario: Member nodes colored by community

- **WHEN** a user drills into community 4 (color `#7dd3fc`)
- **THEN** all 17 member nodes are rendered with background color `#7dd3fc`

#### Scenario: Bridge edges shown to collapsed communities

- **WHEN** a member node of the expanded community has an edge to a node in community 8
- **THEN** a dashed edge is drawn from the member node to community 8's dot (the target node is not individually rendered)

#### Scenario: Drill-down phaseHint uses label

- **WHEN** a user drills into community 4 which has the LLM label `"Authentication Module"`
- **THEN** the `phaseHint` reads `"Exploring Authentication Module — 17 nodes"` (using the label), not `"Exploring Community 4 — 17 nodes"`