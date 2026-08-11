# Design — Cluster Composition

## Context

This change adds per-community composition metadata (code/doc/other counts, dominant kind,
mixed ratio, cross-type edge count) to `graph.json` and surfaces it in the HTML viewer + LLM
labeling prompt. Three small decisions: where to compute, where to store, and how to render.

## Decision 1 — Where to compute: post-Leiden in pipeline

`computeCompositions` runs in `UseCase/Pipeline/Core.hs` **after** Leiden produces
`CommunityMap`, alongside the existing `analyzeGraph` step that computes `nodeDegree` and
`nodeIsBridge`. It's a pure O(N + E) aggregation:

```haskell
computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition
computeCompositions g commMap =
  Map.fromList
    [ (cid, compositionFor cid members)
    | (cid, members) <- Map.toList commMap
    ]
  where
    compositionFor cid members =
      let codeCount = length [() | nid <- members
                                 , Just n <- [Map.lookup nid (gNodes g)]
                                 , nodeFileType n == CodeFile]
          docCount  = length [() | nid <- members
                                 , Just n <- [Map.lookup nid (gNodes g)]
                                 , nodeFileType n `elem` [DocFile, PaperFile, OfficeFile]]
          otherCount = length members - codeCount - docCount
          kinds = [nodeKind n | Just n <- map (flip Map.lookup (gNodes g)) members
                              , Just k <- [nodeKind n]]
          dominantKind = mostFrequent kinds
          mixedRatio = if max codeCount docCount == 0
                       then 0.0
                       else fromIntegral (min codeCount docCount)
                            / fromIntegral (max codeCount docCount)
          crossEdges = countCrossTypeEdges g cid members
      in CommunityComposition codeCount docCount otherCount dominantKind mixedRatio crossEdges
```

`countCrossTypeEdges` iterates the community's edges and counts those with `type ==
"references"` where one endpoint is `CodeFile` and the other is `DocFile`/`PaperFile`/
`OfficeFile`.

## Decision 2 — Where to store: inline in `graph.json`

Unlike embeddings (which are large and go to a sidecar — see `semantic-edge-inference`),
composition is **one small record per community**. At 1,000 communities × ~80 bytes, that's
~80KB — trivially inline in `graph.json`.

```
   graph.json
   ├─ nodes: [...]
   ├─ edges: [...]
   ├─ communities: {...}
   ├─ compositions: {                     ← this change (small, inline)
   │     "483": { "code": 12, "doc": 4, "other": 0, "dominant_kind": "function",
   │              "mixed_ratio": 0.33, "code_doc_edges": 3 },
   │     ...
   │   }
   └─ embeddings_path: "..."              ← semantic-edge-inference (separate change)
```

Additive `Maybe` field on `Graph`: `gCompositions :: Maybe (Map CommunityId CommunityComposition)`. Legacy graphs load as `Nothing` → treated as empty map by consumers.

## Decision 3 — HTML badge rendering

The badge renders at two places in `Infrastructure/Export/HTML.hs`:

1. **Community dots (overview mode)**: as a tooltip on each dot — hover shows
   `🔧 12 / 📄 4 / 🌉 3`.
2. **Community drill-down header**: as a static badge next to the community label —
   `Community 483: Authentication  🔧 12 / 📄 4 / 🌉 3`.

The badge reads from the embedded `compositions` JSON in the HTML payload (already shipped
as part of `graph.json` streaming into the page). No extra HTTP call.

```
   ┌─────────────────────────────────────────────────┐
   │  Community 483: Authentication  🔧 12 / 📄 4 / 🌉 3 │
   │                                                 │
   │  [depth selector: Overview | Community | Full]  │
   │                                                 │
   │  • fn_verifyToken    • sec_JWT_validation       │
   │  • fn_refreshToken   • sec_Auth_flow            │
   │  • mod_Auth          • ...                       │
   └─────────────────────────────────────────────────┘
```

Emoji choice: `🔧` (code), `📄` (docs), `🌉` (cross-type bridges). If emoji rendering is
unreliable in the target browsers, fall back to text: `[C:12 / D:4 / X:3]`. Configurable
in the HTML template.

## Decision 4 — Labeling prompt update

### Current prompt

```
You are a code architecture analyst. Given these communities of related code nodes,
assign a concise 2-4 word label that describes each community's purpose.

Community 483 (cohesion: 0.72, size: 16):
  Top nodes: verifyToken, AuthMiddleware, refreshToken, sec_JWT_validation, sec_Auth_flow
```

Problem: flat list, LLM can't tell code from docs, names the cluster after the most frequent
token (often a code identifier).

### New prompt

```
You are a code-and-knowledge architecture analyst. Given these communities of related nodes
(code and documentation), assign a concise 2-4 word label that names the CONCEPT that
unifies each community — not the most frequent word.

Community 483 (cohesion: 0.72, size: 16, composition: 12 code + 4 docs, 3 code↔doc links):
  Top code nodes: verifyToken, AuthMiddleware, refreshToken
  Top doc nodes:  'JWT validation', 'Auth flow'
```

Three changes:
1. **"code-and-knowledge"** frames the task as mixed-corpus.
2. **Composition line** (`12 code + 4 docs, 3 code↔doc links`) tells the LLM this is mixed
   and the concept should bridge both sides.
3. **Split top nodes** by `(code)`/`(doc)` so the LLM sees doc headings as natural-language
   anchors for the concept name.

### Edge cases

- **Pure-code cluster**: only "Top code nodes:" line; no "Top doc nodes:" line; composition
  reads `"10 code + 0 docs"`. Preamble still says "code-and-knowledge" (honest — the LLM
  names the code concept).
- **Pure-doc cluster**: only "Top doc nodes:" line; composition reads `"0 code + 8 docs"`.
- **No composition available** (legacy graph): fall back to today's flat list — don't tag,
  don't split. Graceful degradation.
- **`nodeKind = Nothing`** on some nodes: those nodes still appear in top nodes; the
  `(code)`/`(doc)` tag is based on `nodeFileType`, not `nodeKind`, so it always works.

## What this design does NOT decide

- **`--mixed-only` filter flag** — that's `explorer-queries`, not this change. This change
  only computes + persists + surfaces `ccMixedRatio`; the filter consumes it.
- **`graphos cluster <id>` subcommand** — also `explorer-queries`. This change provides the
  data; that change provides the command.
- **Composition-aware observability** — no new metrics; composition is static metadata.