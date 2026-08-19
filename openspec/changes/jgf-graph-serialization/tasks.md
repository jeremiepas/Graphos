## 1. JGF envelope model

- [ ] 1.1 Define the JGF document type (`graph` with `directed`, `type`, `label`, `metadata`, `nodes`, `edges`) and `graphos.schemaVersion`
- [ ] 1.2 Node → JGF mapping: `id`/`label` top-level; `source_file`,`kind`,`community`,`line_start`,`line_end`,`signature`,`is_bridge`,`degree` under node `metadata`
- [ ] 1.3 Edge → JGF mapping: `source`/`target`/`relation` top-level; `id`,`weight`,`confidence`,`extra` under edge `metadata`
- [ ] 1.4 Graph metadata: `communities`,`cohesion`,`god_nodes`,`community_labels`,`graph_hash`,`schemaVersion` under `graph.metadata.graphos`

## 2. Writer

- [ ] 2.1 Emit the JGF envelope from `Infrastructure/Export/JSON.hs` (graph output)
- [ ] 2.2 Emit the JGF envelope from the checkpoint writer
- [ ] 2.3 Set media-type-consistent shape (`application/vnd.jgf+json`) and document it

## 3. Reader (backward compatible)

- [ ] 3.1 Detect format: top-level `graph` object ⇒ JGF; else legacy `nodes`/`edges`
- [ ] 3.2 Parse JGF into the in-memory `Graph` (+ index/communities) losslessly
- [ ] 3.3 Keep parsing legacy `graph.json` during the deprecation window
- [ ] 3.4 Reject unknown **major** `schemaVersion` with a clear error

## 4. Migration

- [ ] 4.1 `graphos migrate-graph <file>` (or automatic re-write on next full run) to upgrade legacy files
- [ ] 4.2 Changelog + docs: documented format, media type, version policy

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test`: write→read round-trip equality on a fixture graph
- [ ] 5.3 `cabal test`: legacy `graph.json` still loads; unknown-major-version rejected
- [ ] 5.4 Validate emitted file against the JGF schema and confirm MCP/query/HTML still load it
