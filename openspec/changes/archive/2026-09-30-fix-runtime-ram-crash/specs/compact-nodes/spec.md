## ADDED Requirements

### Requirement: Compact Node internal representation

The `Node` type SHALL use `Data.Text.Short.Text` for `nodeLabel`, `nodeSourceFile`, and `nodeSignature` fields. The 12 `Maybe` fields SHALL be replaced with a `Word64` bit-field (`nodePresentBits`) indicating which optional fields are present, with the actual values stored in a separate `NodeExtra` record that is `Nothing` when all optional fields are absent.

The JSON serialization (ToJSON/FromJSON) SHALL remain identical — the same JSON structure is produced regardless of internal representation.

- **Plan**: Reduce per-node memory from ~400 bytes to ~150 bytes by eliminating `Maybe` wrapper overhead and using `Text.Short` for common short strings.
- **Do**: Create a `NodeCompact` internal representation with `Word64` bit-field and `Maybe NodeExtra` for optional data. Keep `Node` as the public type but change its internal representation. Ensure `ToJSON`/`FromJSON` instances produce identical JSON.
- **Check**: Hspec round-trip tests pass (JSON decode → encode produces identical output). Memory profiling shows ~2.5× reduction per node.
- **Act**: If `Text.Short` causes issues with very long labels, fall back to regular `Text` for labels exceeding 100 chars.

#### Scenario: JSON round-trip identity
- **WHEN** a `Node` is serialized to JSON via `toJSON` and then deserialized via `parseJSON`
- **THEN** the resulting `Node` is equal to the original (`==`)
- **AND** the JSON representation is identical (same keys, same values, same order)

#### Scenario: Memory reduction for large graphs
- **WHEN** 100k nodes are loaded into a `Map NodeId Node`
- **THEN** total heap usage for the node map SHALL be less than 25MB (measured via `+RTS -s`)
- **AND** this is a ~2.5× reduction from the current ~60-70MB for 100k nodes

#### Scenario: NodeExtra is Nothing for simple nodes
- **WHEN** a Node has all optional fields as `Nothing` (no `nodeLineStart`, no `nodeSignature`, no `nodeExtra`, etc.)
- **THEN** `nodeExtraRecord` SHALL be `Nothing`
- **AND** the node occupies only the space of its mandatory fields plus the `Word64` bit-field

### Requirement: Text.Short for common short strings

Fields `nodeLabel`, `nodeSourceFile`, and `nodeSignature` SHALL use `Data.Text.Short.Text` internally. The `ToJSON` instance SHALL convert `Text.Short` to regular `Text` for serialization. The `FromJSON` instance SHALL convert regular `Text` to `Text.Short` during deserialization.

#### Scenario: Short label storage
- **WHEN** a node has `nodeLabel = "myFunction"`
- **THEN** the label is stored as `ShortText` without heap allocation (if ≤ 16 bytes on 64-bit GHC)
- **AND** `toJSON` produces the same JSON string `"myFunction"` as before

#### Scenario: Long label fallback
- **WHEN** a node has `nodeLabel` exceeding 100 characters
- **THEN** the label SHALL still be stored correctly (Text.Short handles arbitrary length)
- **AND** no data loss occurs regardless of label length