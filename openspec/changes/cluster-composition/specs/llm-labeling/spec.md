# llm-labeling

Delta — composition-aware labeling prompt.

## MODIFIED Requirements

### Requirement: Composition-aware labeling prompt

The `labelPrompt` function SHALL tag each top node with its corpus kind (`(code)` or `(doc)`)
based on `nodeFileType` and SHALL include a composition summary line in the prompt. The prompt
preamble SHALL frame the task as mixed code-and-knowledge analysis and SHALL instruct the LLM
to name the concept that unifies the community rather than the most frequent word. When
compositions are absent (legacy graph without `compositions`), `labelPrompt` SHALL fall back
to today's flat list format (no tags, no split, no composition line) — graceful degradation.

#### Scenario: Mixed cluster prompt
- **WHEN** `labelPrompt` is called for a community with 12 `CodeFile` + 4 `DocFile` members
  and compositions are available
- **THEN** the prompt contains a composition line like
  `"Community 483 (cohesion: 0.72, size: 16, composition: 12 code + 4 docs, 3 code↔doc links):"`
  and the top nodes are split into `"Top code nodes:"` and `"Top doc nodes:"` lines

#### Scenario: Pure-code cluster prompt
- **WHEN** `labelPrompt` is called for a community with only `CodeFile` nodes and
  compositions are available
- **THEN** the prompt shows `"Top code nodes:"` and no `"Top doc nodes:"` line; the
  composition line reads `"composition: 10 code + 0 docs"`

#### Scenario: Preamble names the unifying concept
- **WHEN** the prompt is rendered for any community with compositions available
- **THEN** the preamble contains the word "concept" or "unifies" (instructing the LLM to name
  the shared concept, not the most frequent token)

#### Scenario: Legacy graph falls back to flat prompt
- **WHEN** `labelPrompt` is called and compositions are absent (`Nothing` / empty)
- **THEN** the prompt uses today's flat list format (single "Top nodes:" line, no tags, no
  composition line) and the existing preamble — no error