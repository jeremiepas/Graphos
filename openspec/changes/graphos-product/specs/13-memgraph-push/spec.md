## ADDED Requirements

### Requirement: Workflow 13 — Memgraph push via Bolt protocol
Module `Graphos.Infrastructure.Export.Memgraph` SHALL export: `pushToMemgraph :: MemgraphConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`. Same three push modes as Neo4j (FullPush/SubgraphPush/CommunityPush), same representative selection, same Cypher generation. Connection via Bolt protocol at configured URI. `data MemgraphConfig = MemgraphConfig { mgUri :: !Text, mgUser :: !Text, mgPassword :: !Text, mgPushMode :: !PushMode, mgSubgraphSize :: !Int }` with defaults: URI `bolt://localhost:7688`, empty user/password, SubgraphPush mode, 7 representatives. CLI: `--memgraph`, `--memgraph-push <uri>`, `--memgraph-push-mode`, `--memgraph-subgraph-size N`. (PRD §9, workflow 13)

#### Scenario: Push to Memgraph via Bolt
- **WHEN** `--memgraph --memgraph-push bolt://localhost:7688` is set on a graph with 20k nodes
- **THEN** graph data SHALL be pushed via Bolt protocol using SubgraphPush mode

#### Scenario: Memgraph is ephemeral
- **WHEN** Memgraph restarts
- **THEN** previously pushed data SHALL be lost (in-memory, not disk-based)