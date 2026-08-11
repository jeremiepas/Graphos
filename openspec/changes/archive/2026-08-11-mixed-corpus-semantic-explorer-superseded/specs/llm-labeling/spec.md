# llm-labeling

Delta — composition-aware labeling prompt.

## MODIFIED Requirements

### Requirement: Composition-aware labeling prompt

The `labelPrompt` function SHALL tag each top node with its corpus kind (`(code)` or `(doc)`)
based on `nodeFileType` and SHALL include a composition summary line in the prompt. The prompt
preamble SHALL frame the task as mixed code-and-knowledge analysis and SHALL instruct the LLM
to name the concept that unifies the community rather than the most frequent word.

#### Scenario: Mixed cluster prompt
- **WHEN** `labelPrompt` is called for a community with 12 `CodeFile` + 4 `DocFile` members
- **THEN** the prompt contains a composition line like
  `"Community 483 (cohesion: 0.72, size: 16, composition: 12 code + 4 docs, 3 code↔doc links):"`
  and the top nodes are split into `"Top code nodes:"` and `"Top doc nodes:"` lines

#### Scenario: Pure-code cluster prompt
- **WHEN** `labelPrompt` is called for a community with only `CodeFile` nodes
- **THEN** the prompt shows `"Top code nodes:"` and no `"Top doc nodes:"` line; the composition
  line reads `"composition: 10 code + 0 docs"`

#### Scenario: Preamble names the unifying concept
- **WHEN** the prompt is rendered for any community
- **THEN** the preamble contains the word "concept" or "unifies" (instructing the LLM to name
  the shared concept, not the most frequent token)