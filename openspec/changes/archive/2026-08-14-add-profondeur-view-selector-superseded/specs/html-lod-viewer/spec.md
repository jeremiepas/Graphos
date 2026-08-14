# HTML Level-of-Detail Viewer Capability — Delta

## Purpose

Update the LOD viewer (PRD §12) so the two-phase overview→drill-down state machine becomes one of several selectable depth levels. The drill-down phase is relabeled `Community` depth; the overview phase is unchanged; new `Full` and `Custom` (neighborhood) depth levels are added as peer modes via the new `html-depth-selector` capability. This delta modifies the requirements that describe phase transitions; requirements describing the overview render, the aggregate dataset, and the self-contained/served-via-HTTP contracts are unchanged.

## MODIFIED Requirements

### Requirement: Two-phase level-of-detail rendering

The HTML viewer SHALL render the graph at a user-selected depth level (PRD §12, see `html-depth-selector`): `Overview` (one dot per community, positioned by inter-community edges, sized by member count, colored by community), `Community` (one community expanded into its member nodes with internal and bridge edges), `Full` (all individual nodes rendered, no community aggregation), or `Custom` (N-hop neighborhood around a selected node). The viewer SHALL default to `Overview` depth on initial load. The viewer SHALL NOT render all individual nodes simultaneously unless the user explicitly selects `Full` or `Custom` depth. The former `currentPhase` two-phase state machine is replaced by a `currentDepth` multi-level state.

#### Scenario: Overview depth renders community dots only

- **WHEN** the HTML viewer loads a graph with 78,529 nodes across 8,519 communities (default `Overview` depth)
- **THEN** the overview renders exactly one dot per community (8,519 dots) and zero individual node dots

#### Scenario: Community depth expands a single community

- **WHEN** a user in `Overview` depth clicks a community dot (or selects `Community` depth with a previously selected community)
- **THEN** that community's members are rendered as individual node dots with their internal edges and any bridge edges to other communities, and the remaining communities stay collapsed as dots

#### Scenario: Full depth renders all nodes on explicit selection

- **WHEN** a user explicitly selects `Full` depth on a graph with 942 nodes and 3 communities
- **THEN** the viewer renders one dot per individual node (942 dots) with all inter-node edges, and no community aggregation is applied

#### Scenario: No simultaneous full-graph render unless explicitly selected

- **WHEN** the viewer is in `Overview` or `Community` depth
- **THEN** the number of rendered node-level dots SHALL NOT exceed the member count of the single expanded community (zero for `Overview`), and SHALL be strictly less than the total node count for graphs with more than one community

### Requirement: Drill-down reuses member nodes with community color

The `Community` depth SHALL render member nodes using their community color (from the joined `nodeCommunityId`) and SHALL include edges internal to the community plus bridge edges (edges where one endpoint is in the expanded community and the other is in a different community, shown as dashed lines to the collapsed community dots). The former `expandCommunity` function is kept as an internal helper invoked by the depth dispatcher when entering or swapping within `Community` depth.

#### Scenario: Member nodes colored by community

- **WHEN** a user enters `Community` depth for community 4 (color `#7dd3fc`)
- **THEN** all 17 member nodes are rendered with background color `#7dd3fc`

#### Scenario: Bridge edges shown to collapsed communities

- **WHEN** a member node of the expanded community has an edge to a node in community 8
- **THEN** a dashed edge is drawn from the member node to community 8's dot (the target node is not individually rendered)

#### Scenario: Swapping community within Community depth

- **WHEN** a user in `Community` depth (community 4 expanded) clicks community 8's dot in the sidebar list
- **THEN** the viewer swaps the rendered members to community 8 without passing through `Overview` depth