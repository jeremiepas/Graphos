## 1. JGF envelope model

- [x] 1.1 Define the JGF document type (`graph` with `directed`, `type`, `label`, `metadata`, `nodes`, `edges`) and `graphos.schemaVersion`
- [x] 1.2 Node → JGF mapping: `id`/`label` top-level; `source_file`,`kind`,`community`,`line_start`,`line_end`,`signature`,`is_bridge`,`degree` under node `metadata`
- [x] 1.3 Edge → JGF mapping: `source`/`target`/`relation` top-level; `id`,`weight`,`confidence`,`extra` under edge `metadata`
- [x] 1.4 Graph metadata: `communities`,`cohesion`,`god_nodes`,`community_labels`,`graph_hash`,`schemaVersion` under `graph.metadata.graphos`

## 2. Writer

- [x] 2.1 Emit the JGF envelope from `Infrastructure/Export/JSON.hs` (graph output)
- [x] 2.2 Emit the JGF envelope from the checkpoint writer
- [x] 2.3 Set media-type-consistent shape (`application/vnd.jgf+json`) and document it

## 3. Reader (backward compatible)

- [x] 3.1 Detect format: top-level `graph` object ⇒ JGF; else legacy `nodes`/`edges`
- [x] 3.2 Parse JGF into the in-memory `Graph` (+ index/communities) losslessly
- [x] 3.3 Keep parsing legacy `graph.json` during the deprecation window
- [x] 3.4 Reject unknown **major** `schemaVersion` with a clear error

## 4. Migration

- [x] 4.1 `graphos migrate-graph <file>` (or automatic re-write on next full run) to upgrade legacy files
- [x] 4.2 Changelog + docs: documented format, media type, version policy

## 5. Verification

- [x] 5.1 `cabal build --flag dev` with `-Werror`
- [x] 5.2 `cabal test`: write→read round-trip equality on a fixture graph
- [x] 5.3 `cabal test`: legacy `graph.json` still loads; unknown-major-version rejected
- [ ] 5.4 Validate emitted file against the JGF schema and confirm MCP/query/HTML still load it
