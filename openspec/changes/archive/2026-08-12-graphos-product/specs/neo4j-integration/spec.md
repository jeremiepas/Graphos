## ADDED Requirements

### Requirement: Infrastructure.Export.Neo4j — three push modes
Module `Graphos.Infrastructure.Export.Neo4j` SHALL export: `pushToNeo4j :: Neo4jConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> PushMode -> Int -> IO ()`. Three modes: (1) `FullPush` — push all nodes, edges, community assignments as Cypher `CREATE`/`MERGE` statements. (~990k statements, 2-4 hours for 100k graph). (2) `SubgraphPush` — push community representatives + bridge nodes only. Default 7 representatives per community via `--neo4j-subgraph-size N`. (~64k statements, ~30s). (3) `CommunityPush` — push community-level nodes and inter-community edges only. (~8k statements, ~5s). Auto-selection: nodes < 10k → FullPush; ≥ 10k → SubgraphPush. Override: `--neo4j-push-mode full|subgraph|community`. (PRD §9.1)

#### Scenario: Auto-select FullPush for small graph
- **WHEN** `pushToNeo4j` is called on a graph with 5000 nodes without explicit mode
- **THEN** the function SHALL use FullPush and generate Cypher for all 5000 nodes + edges

#### Scenario: Auto-select SubgraphPush for large graph
- **WHEN** `pushToNeo4j` is called on a graph with 50,000 nodes without explicit mode
- **THEN** the function SHALL use SubgraphPush with ≤7 representatives per community

#### Scenario: Override to CommunityPush
- **WHEN** `--neo4j-push-mode community` is set
- **THEN** `pushToNeo4j` SHALL generate only community-level nodes and inter-community edges regardless of graph size

### Requirement: Infrastructure.Export.Neo4j — Cypher statement generation
Cypher SHALL use parameterized statements (pass values as JSON parameters, not embedded in strings) to eliminate escaping issues. Batches of up to 50 statements per request to stay within Neo4j limits. Three entity types: `Node` (code/doc concepts), `Community` (with label + cohesion), `BELONGS_TO` (Node → Community edge). (PRD §9.1)

#### Scenario: Parameterized Cypher avoids injection
- **WHEN** a node label contains special characters like quotes or backslashes
- **THEN** the Cypher SHALL use parameterized `{$param}` syntax, not string interpolation

#### Scenario: Batch size respected
- **WHEN** pushing 10,000 nodes
- **THEN** statements SHALL be batched into groups of ≤50

### Requirement: Infrastructure.Export.Memgraph — Bolt protocol variant
Module `Graphos.Infrastructure.Export.Memgraph` SHALL export `pushToMemgraph :: MemgraphConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> PushMode -> Int -> IO ()`. Same three push modes as Neo4j. Connection via Bolt protocol at configured URI. `data MemgraphConfig = MemgraphConfig { mgUri :: Text, mgUser :: Text, mgPassword :: Text, mgPushMode :: PushMode, mgSubgraphSize :: Int }`. (PRD §9, workflow 13)

#### Scenario: Push to Memgraph via Bolt
- **WHEN** `pushToMemgraph` is called with a valid Memgraph URI
- **THEN** the function SHALL connect via Bolt protocol and push graph data using the auto-selected push mode

### Requirement: Neo4j/Memgraph config from graphos.yaml
`Graphos.Domain.Config.Neo4jConfig` SHALL include: `neo4jUri :: Text` (default `bolt://localhost:7687`), `neo4jUser :: Text` (default "neo4j"), `neo4jPassword :: Text` (default "test"), `neo4jPushMode :: PushMode` (default auto), `neo4jSubgraphSize :: Int` (default 7). `MemgraphConfig` SHALL include analogous fields with default URI `bolt://localhost:7688`. Read from `graphos.yaml` `neo4j:` and `memgraph:` sections. (PRD §14.2)

#### Scenario: Read Neo4j config from graphos.yaml
- **WHEN** `graphos.yaml` contains `neo4j: { uri: "bolt://db:7687", user: "admin", password: "secret" }`
- **THEN** the merged config SHALL use `bolt://db:7687`, user "admin", password "secret"