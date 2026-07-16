# 11 — Community Labeling

> `graphos <path> --label`

Generate human-readable community names using an LLM, replacing the default TF-IDF-based labels.

---

## Flow

```
┌──────────────────────────────────────────────────────────────┐
│              COMMUNITY LABELING FLOW                        │
│                                                              │
│  Input: Graph + CommunityMap + CohesionMap                 │
│                                                              │
│  1. Batch communities (Domain.Labeling.batchCommunities)    │
│     → Groups of N communities per LLM call                 │
│                                                              │
│  2. Generate prompt per batch (Domain.Labeling.labelPrompt)│
│     → Lists community members + cohesion + stats           │
│                                                              │
│  3. Call LLM (Infrastructure.LLM.OpenAI.callLLM)            │
│     → OpenAI-compatible endpoint (OpenAI, Ollama, etc.)    │
│                                                              │
│  4. Parse labels from response                              │
│     → Map CommunityId → Text label                          │
│                                                              │
│  5. Optional: Push labels to Neo4j (--neo4j)               │
│     → MERGE Community nodes with label property             │
│                                                              │
│  Output: Map CommunityId → "Parser AST" / "Auth Chain"    │
└──────────────────────────────────────────────────────────────┘
```

---

## Why This Workflow Exists

Leiden community detection produces community IDs (integers) but no human-readable names. Without labels, navigating a graph with 3,000+ communities is impractical. The default TF-IDF labeling produces reasonable but mechanical names. LLM labeling produces **contextual** names that reflect what the community actually does.

Example:
- TF-IDF: `"Parse Config Validate"` 
- LLM: `"Configuration Parsing & Validation"`

---

## Labeling Configuration

```yaml
# graphos.yaml
labeling:
  model: "llama3.2"         # Ollama model name or OpenAI model
  endpoint: "http://localhost:11434/v1"  # OpenAI-compatible endpoint
  batch_size: 20            # Communities per LLM call
  temperature: 0.3           # Low for consistent labels
```

---

## Cost Considerations

LLM labeling makes one API call per batch of communities. For 3,000 communities with batch_size=20, that's 150 calls. Using a local Ollama model (e.g., `llama3.2`) is free. Using OpenAI GPT-4 costs approximately $0.50–$1.00 for a full labeling run.

---

## When to Use

| Scenario | Use --label |
|----------|-------------|
| Exploring a foreign codebase | Yes — readable community names |
- Generating HTML visualization | Yes — labels in sidebar |
| Pushing to Neo4j | Yes — named communities in graph DB |
| Quick internal run | No — TF-IDF is fast and free |

---

## Prerequisite

Requires a graph with community assignments (run full pipeline without `--no-cluster` first).