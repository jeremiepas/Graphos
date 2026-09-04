# 17 — Cypher

> `graphos cypher "<query>" [--graph PATH] [--budget N] [--json]`

Read-only openCypher / GQL (ISO/IEC 39075) subset over the in-memory property graph.

---

## Flow

```
┌───────────────────────────────────────────────────────────────┐
│                      CYPHER FLOW                              │
│                                                               │
│  graphos cypher "MATCH (n:Function) RETURN n" --budget 50    │
│       │                                                       │
│       ▼                                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Load graph.json → Graph + GraphIndex (warm, once)     │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Parse (megaparsec) → CypherQuery AST                  │  │
│  │  → reject out-of-subset constructs (position + name)   │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Evaluate: pattern match → WHERE → project →           │  │
│  │  DISTINCT → ORDER BY → SKIP → LIMIT → budget cap      │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  CypherResult { columns, rows, truncated }                   │
└───────────────────────────────────────────────────────────────┘
```

---

## Supported subset

| Clause | Supported |
|--------|-----------|
| `MATCH` | Node and relationship patterns, variable-length `*m..n`, direction (`->`, `<-`, `<->`) |
| `WHERE` | `=`, `<>`, `<`, `>`, `<=`, `>=`, `IN`, `STARTS WITH`, `CONTAINS`, `=~` (regex), `AND` / `OR` / `NOT`, `IS NULL` |
| `RETURN` | Expressions, `DISTINCT`, `ORDER BY`, `SKIP`, `LIMIT`, `count()` |

Write clauses (`CREATE`, `MERGE`, `SET`, `REMOVE`, `DELETE`) are **recognized** openCypher grammar but **rejected on this read-only surface** with an error pointing at the mutation surface. The graph is never mutated here. See [18 — Cypher Write](18-cypher-write.md) for the manipulation subset.

---

## Property-graph mapping

The mapping is fixed and deterministic:

| Cypher concept | Graphos source |
|----------------|----------------|
| Node **label** | `nodeKind` (e.g. `Function`, `Type`) |
| Relationship **type** | `edgeRelation` (e.g. `calls`, `imports`) |
| Node **properties** | the remaining node fields |
| Relationship **properties** | the remaining edge fields |

### Node properties

| Property | Meaning |
|----------|---------|
| `id` | Node ID |
| `label` | Display label |
| `file_type` | File type |
| `source_file` | Source file path |
| `line_start` / `line_end` | Line range |
| `signature` | Entity signature |
| `community` | Community ID |
| `degree` | Node degree |
| `is_bridge` | Articulation-point flag |
| `text` | Snippet (the signature, when present) |

### Relationship properties

| Property | Meaning |
|----------|---------|
| `id` | Edge ID |
| `source` / `target` | Endpoint node IDs |
| `weight` | Edge weight |
| `confidence` | Edge confidence |

A property a node/edge does not declare resolves to **null**: the comparison evaluates as for a null value (the row is excluded), and no error is raised.

---

## Examples

### Label filter + relationship hop

```
graphos cypher "MATCH (a:Function)-[:calls]->(b:Function) RETURN a, b"
```

### Variable-length path

```
graphos cypher "MATCH (a)-[:imports*1..3]->(b) RETURN b"
```

### Regex `WHERE`

```
graphos cypher "MATCH (n:Function) WHERE n.source_file =~ 'src/services/.*' RETURN n"
```

### Count

```
graphos cypher "MATCH (n:Function) RETURN count(n)"
```

### JSON output

```
graphos cypher "MATCH (n:Function) RETURN n" --json
```

```json
{
  "columns": ["n"],
  "rows": [[{"id": "fn-001", "label": "parse", "source_file": "src/Parse.hs", "community": 3, "text": "parse :: Text -> ParseResult"}]],
  "truncated": false
}
```

---

## Output

### Text Mode (default)

```
Results (1 rows)
n
{"id":"fn-001","label":"parse","source_file":"src/Parse.hs","community":3,"text":"parse :: Text -> ParseResult"}
```

### JSON Mode (`--json`)

```json
{
  "columns": ["n"],
  "rows": [[{"id": "fn-001", "label": "parse", "source_file": "src/Parse.hs", "community": 3, "text": "parse :: Text -> ParseResult"}]],
  "truncated": false
}
```

`truncated` is `true` when the result was capped by the budget.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |
| `--budget N` | 2000 | Row budget for output |
| `--json` | off | Output as JSON |
| `--label-width N` | 120 | Max label width before elision |
| `--edges semantic\|all` | semantic | Edge filtering mode |

---

## MCP tool

The same surface is exposed as the MCP tool `cypher_query`:

```json
{"name": "cypher_query", "arguments": {"query": "MATCH (n:Function) RETURN n", "budget": 2000}}
```

It reuses the warm graph + index (no per-call rebuild) and returns `{"columns": [...], "rows": [[...]], "truncated": bool}`.

---

## Prerequisite

Requires an existing `graph.json`. Run the full pipeline first.
