# 14 — Symbols

> `graphos symbols <name>`

Exact identifier lookup — bypasses fuzzy scoring for the common case of "I know the exact name".

---

## Flow

```
┌───────────────────────────────────────────────────────────────┐
│                     SYMBOLS FLOW                              │
│                                                               │
│  graphos symbols CliCommand                                  │
│       │                                                       │
│       ▼                                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Load graph.json → LabeledGraph + GraphIndex          │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│                       ▼                                       │
│  ┌─────────────────────────────────────────────────────────┐  │
│  │  Exact lookup: case-sensitive first,                  │  │
│  │  then case-insensitive fallback                       │  │
│  │  → no fuzzy scoring, no BFS traversal                │  │
│  └────────────────────┬────────────────────────────────────┘  │
│                       │                                       │
│              ┌────────┴────────┐                              │
│              ▼                 ▼                              │
│  ┌─────────────────┐   ┌──────────────────┐                  │
│  │  Found          │   │  Not Found         │                  │
│  │  → all matches │   │  → "No symbol found"│                 │
│  │    with id,    │   │  → did-you-mean     │                  │
│  │    file, line, │   │    suggestions      │                  │
│  │    kind, degree│   │                     │                  │
│  └─────────────────┘   └───────────────────┘                  │
└───────────────────────────────────────────────────────────────┘
```

---

## Lookup Semantics

1. **Case-sensitive match** against the identifier token index and full labels
2. If no case-sensitive hit, **case-insensitive fallback**
3. If still no match: explicit "not found" + did-you-mean suggestions
4. Multiple matches at different locations are all listed

**No fuzzy scoring. No BFS traversal.** This is for when you know the exact name.

---

## Output

### Text Mode (default)

```
Symbols found: 2

0.50  parse [parse-001] (src/A.hs)
0.50  parse [parse-002] (src/B.hs)
```

Or when not found:

```
No symbol found.
Did you mean: Parsr, Parser?
```

### JSON Mode (`--json`)

```json
{
  "found": [{"id": "parse-001", "label": "parse", "score": 0.5, "source_file": "src/A.hs", "community": null}],
  "not_found": false,
  "suggestions": []
}
```

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--graph PATH` | `graphos-out/graph.json` | Path to graph.json |
| `--budget N` | 2000 | Token budget for output |
| `--json` | off | Output as JSON |
| `--label-width N` | 120 | Max label width before elision |
| `--edges semantic\|all` | semantic | Edge filtering mode |

---

## Prerequisite

Requires an existing `graph.json`. Run the full pipeline first.