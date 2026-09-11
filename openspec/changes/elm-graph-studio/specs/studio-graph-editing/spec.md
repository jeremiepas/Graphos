# studio-graph-editing

Preview-adjacent editing: fix a label, retype a node, add or remove an edge — with undo/redo —
applied through the specced cypher write subset when connected, kept as a local edit log with
export when file-loaded.

## ADDED Requirements

### Requirement: Node and edge editing operations

The studio SHALL support editing from the selection detail panel: update node properties
(label, kind, and `extra` entries), create and delete edges (with relation type), create
nodes, and delete nodes (with their incident edges, after explicit confirmation naming the
edge count). Spec-required node fields SHALL be validated before an edit is accepted (ids
unique and non-empty, file_type in vocabulary), and the canvas and detail panel SHALL reflect
an accepted edit immediately.

- **Plan**: The 12-field node schema (`node-schema`) is the contract; editing exposes the
  human-fixable subset, not raw JSON surgery.
- **Do**: Edit forms from the design-system components; validation in the Model before any
  effect.
- **Check**: Scenarios below.
- **Act**: If users routinely edit fields beyond the exposed subset, extend the forms — the
  edit-intent type is the single place to grow.

#### Scenario: Relabel reflects everywhere

- **WHEN** a user renames node `mod_Auth`'s label and confirms
- **THEN** the canvas label, detail panel, breadcrumb (if selected) and search results show
  the new label

#### Scenario: Deleting a hub warns with its blast radius

- **WHEN** a user deletes a node with 47 incident edges
- **THEN** the confirmation names the 47 edges, and on confirm the node and exactly those
  edges disappear

#### Scenario: Invalid edits are rejected client-side

- **WHEN** a user submits a node edit that empties the label
- **THEN** the form rejects it with a message and no mutation or log entry is produced

### Requirement: Connected-mode mutation via the cypher write subset

In connected mode with an available write surface, the studio SHALL apply each accepted edit
through `/api/cypher/mutate` using the write clause subset specified by `cypher-mutation`,
SHALL treat the server's response as authoritative (re-rendering from the server state on
success, surfacing the server's error and reverting the optimistic change on failure), and
SHALL disable editing affordances — stating why — when the server is read-only.

- **Plan**: The mutation surface, its read-only gating and model reconciliation are already
  specified server-side; the studio is a client of that contract, not a second write path.
- **Do**: Edit-intent → cypher statement translation in one module; optimistic apply +
  reconcile.
- **Check**: Scenarios below.
- **Act**: If translation gaps appear (edits inexpressible in the subset), extend
  `cypher-mutation` by its own change — never bypass it.

#### Scenario: Edit round-trips through the server

- **WHEN** a connected user with write surface renames a node
- **THEN** the studio issues the corresponding mutate statement, and the rename persists
  across a studio reload against the same server

#### Scenario: Server rejection reverts

- **WHEN** the server rejects a mutation (read-only context or clause error)
- **THEN** the optimistic change is reverted, the server's error is shown, and the edit log
  records nothing

### Requirement: File-mode edit log and export

In file mode the studio SHALL accumulate accepted edits as an ordered local edit log keyed to
the graph identity, apply them to the in-memory graph for all studio features, persist the
log across sessions, and export on demand: the edited graph as a contract-shaped
`graph.json`, and the log itself as a JSON changelog. Loading a file whose identity matches a
stored log SHALL offer — never force — replaying it.

- **Plan**: Offline editing must survive reload and leave with the user; silent replay
  against a re-exported (changed) graph is the corruption case the identity key prevents.
- **Do**: Edit log in the Model, persisted per identity; export via the existing download
  port.
- **Check**: Scenarios below.
- **Act**: If logs grow unwieldy, add log compaction (same-field edits squash) — semantics
  first, size later.

#### Scenario: Offline edits survive reload

- **WHEN** a file-mode user makes three edits and reloads the studio
- **THEN** reopening the same file offers the stored log, and accepting restores all three
  edits

#### Scenario: Exported graph carries the edits

- **WHEN** the user exports after editing
- **THEN** the downloaded `graph.json` parses under the contract shape and contains the
  edited values

### Requirement: Undo and redo

The studio SHALL provide unbounded-within-session undo and redo over accepted edits, in both
modes: in file mode by reverting the log; in connected mode by issuing the inverse mutation
(recorded at accept time) through the same write surface. Undo/redo SHALL be keyboard-bound
and SHALL show what was undone.

- **Plan**: TEA makes edit history a fold over intents; connected mode needs stored inverses
  because the server holds the state.
- **Do**: Each edit-intent carries its inverse; one history zipper serves both modes.
- **Check**: Scenarios below.
- **Act**: If an inverse cannot be constructed for some future edit type, make that edit
  non-undoable *explicitly* in the confirmation rather than silently.

#### Scenario: Undo a deletion restores edges

- **WHEN** a user deletes a node with its 5 edges, then undoes
- **THEN** the node and all 5 edges are restored (in connected mode, via inverse mutations
  accepted by the server)

#### Scenario: Redo reapplies

- **WHEN** the user redoes after the undo above
- **THEN** the deletion is reapplied and the canvas matches the pre-undo state
