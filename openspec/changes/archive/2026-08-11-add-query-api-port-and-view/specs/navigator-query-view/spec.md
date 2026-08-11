# navigator-query-view

The in-`graph.html` navigator search view calls the query HTTP port and renders the real scored `QueryResponse` (verdict, suggestions, scored nodes, matched subgraph edges) on the existing vis-network canvas, replacing the client-side substring filter.

## ADDED Requirements

### Requirement: Navigator search calls the query HTTP port
The `graph.html` navigator search box SHALL, for query strings of length >= 2, issue `fetch('/api/query?q=' + encodeURIComponent(q) + '&mode=bfs')` (debounced) and render the returned `QueryResponse` instead of the prior client-side `allNodes` substring filter. If the fetch fails or `/api/query` is unavailable (e.g. when the HTML is opened via `file://` without a server), the navigator SHALL fall back to the existing client-side substring behavior so the HTML remains self-contained and usable offline.

#### Scenario: Search triggers HTTP query
- **WHEN** the user types "auth" (>= 2 chars) in the search box while the page is served by `graphos serve`
- **THEN** the navigator issues a `fetch` to `/api/query?q=auth&mode=bfs` and renders the resulting `QueryResponse`

#### Scenario: Offline fallback
- **WHEN** the HTML is opened via `file://` (no server) and the user types a query
- **THEN** the fetch fails and the navigator falls back to the existing client-side substring search over `allNodes`, still showing matching nodes

### Requirement: Render verdict and suggestions
The navigator SHALL render the `verdict` (strong/weak/none), `bestScore`, and `hash` from the `QueryResponse` as a header above the results, and SHALL render `suggestions` as a "Did you mean: a, b, c?" line whenever the `suggestions` array is non-empty (matching the CLI text rendering in `renderQueryResponseText`).

#### Scenario: Strong verdict shown
- **WHEN** the API returns `{"verdict":"strong","bestScore":0.85,"hash":"a3f29c01",...}`
- **THEN** the results header displays "strong (best score: 0.85) [hash: a3f29c01]"

#### Scenario: Did-you-mean shown on none
- **WHEN** the API returns `{"verdict":"none","suggestions":["authModule","authHandler"],...}`
- **THEN** the results area shows "Did you mean: authModule, authHandler?"

### Requirement: Render scored nodes ranked
The navigator SHALL render each `QueryResponse.nodes` entry (scored node: id, label, score, source_file, community) as a clickable result item ordered by score descending, matching the CLI ordering. Clicking a result item SHALL focus the corresponding node on the vis-network canvas (reuse the existing `focusNode` behavior).

#### Scenario: Scored nodes ranked descending
- **WHEN** the API returns nodes with scores [0.85, 0.42, 0.21]
- **THEN** the result items are rendered in that order and each shows its score, label, source file, and community

#### Scenario: Click focuses node
- **WHEN** the user clicks a rendered result item with `data-nodeid="auth-mod-001"`
- **THEN** the vis-network canvas focuses the node with id `auth-mod-001`

### Requirement: Render matched subgraph edges as a graph view
The navigator SHALL highlight the matched subgraph on the vis-network canvas using `QueryResponse.nodes[*].id` and `QueryResponse.edges` (src, tgt, rel, conf): matched nodes are highlighted/focused and non-matched nodes are dimmed, and the matched edges are visually emphasized. This is the "graph view" of the query response in the navigator. A "Reset" action SHALL restore the full graph view.

#### Scenario: Matched subgraph highlighted
- **WHEN** the API returns nodes `[A, B, C]` and edges `[(A,B),(B,C)]`
- **THEN** nodes A, B, C are highlighted and all other nodes are dimmed on the canvas, and edges A-B and B-C are emphasized

#### Scenario: Reset restores full graph
- **WHEN** the user clicks "Reset" after a query
- **THEN** the canvas returns to the full graph view with no dimming and no emphasis, and the search results area is cleared