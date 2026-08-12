---
description: "Task 4 — Composition-aware labeling prompt"
---
---
description: "Plan: update labelPrompt to split nodes by type, add composition line, update preamble, and graceful fallback"
---

# Task 4 — Composition-aware labeling prompt — PLAN

**Task slug**: `04-composition-aware-labeling-prompt`
**Attempt**: 1
**Status**: pending

## Summary

Update `labelPrompt` to produce a composition-aware prompt: split top nodes into "Top code nodes:" and "Top doc nodes:" lines, add composition summary line, update preamble to "code-and-knowledge architecture analyst" with concept-unifying instruction. When compositions are absent (legacy graph), fall back to current flat format.

## Detail

### Scope

- **`labelPrompt` update** in `src/Graphos/Domain/Labeling.hs`:
  - Accept optional `Map CommunityId CommunityComposition` parameter (or read from `Graph` if compositions are wired)
  - When compositions available:
    - Tag each top node with `(code)` or `(doc)` based on `nodeFileType`
    - Split into `"Top code nodes:"` and `"Top doc nodes:"` lines (omit empty section)
    - Add composition line: `"composition: N code + M docs, K code↔doc links"`
    - Update preamble: `"code-and-knowledge architecture analyst"` + `"name the CONCEPT that unifies"`
  - When compositions absent: fall back to existing flat list (single "Top nodes:", no tags, no composition line)
- **`labelCommunities` / `labelBatch` wiring** in `src/Graphos/UseCase/Label.hs`:
  - Pass `gCompositions` from graph to `labelPrompt`
  - If `gCompositions = Nothing`, `labelPrompt` uses fallback path

### Spec Scenarios (from specs/llm-labeling/spec.md)

| Spec ID | Scenario | Task Coverage |
|---|---|---|
| LL-1 | Mixed cluster prompt: 12 CodeFile + 4 DocFile | Both "Top code nodes:" and "Top doc nodes:" lines; composition line present |
| LL-2 | Pure-code cluster prompt | Only "Top code nodes:" line; no "Top doc nodes:"; composition reads "N code + 0 docs" |
| LL-3 | Preamble names the unifying concept | Preamble contains "concept" or "unifies" |
| LL-4 | Legacy graph falls back to flat prompt | Single "Top nodes:" line, no tags, no composition line, existing preamble |

### Check Criteria (defined BEFORE code)

**Tests to run:**
```bash
cabal test
# Focus on Labeling tests:
cabal test --test-options="-tag labeling"   # if tags used
```

**Spec scenario gates:**

| ID | Test name pattern | PASS condition | FAIL condition |
|---|---|---|---|
| LL-1 | `prompt.*mixed.*cluster` or `prompt.*split` | Prompt contains `"Top code nodes:"` AND `"Top doc nodes:"` AND `"composition:"` | Missing any of these sections |
| LL-2 | `prompt.*pure.*code` | Prompt contains `"Top code nodes:"`; does NOT contain `"Top doc nodes:"`; composition line present with `0 docs` | `"Top doc nodes:"` line present |
| LL-3 | `prompt.*preamble.*concept` | Prompt text contains "concept" (case-insensitive) OR "unifies" (case-insensitive) | Neither word found in preamble |
| LL-4 | `prompt.*legacy.*fallback` | Prompt contains `"Top nodes:"` (single line, not split); no `(code)` or `(doc)` tags; no `"composition:"` line | Split format or tags present on legacy |

**Prompt structure gates (code-level assertions):**
- For mixed community: `lines prompt` should contain a line matching regex `"composition:\\s*\\d+ code \\+ \\d+ docs"`
  - PASS: regex matches
  - FAIL: no match → composition line missing or malformed
- For pure-code: `"Top code nodes:"` present, `"Top doc nodes:"` absent
  - PASS: exact presence/absence
  - FAIL: wrong sections present
- Tag format: each code node should appear as `nodeName (code)` or just `nodeName` depending on implementation
  - Note: spec says tag with `(code)` or `(doc)` — verify exact format in implementation

**Fallback correctness gate:**
- When called with empty/`Nothing` compositions, output must be IDENTICAL to the current prompt format
  - PASS: diff of old prompt vs new prompt (with `Nothing`) is empty
  - FAIL: any deviation from current format

**Exact FAIL boundaries:**
- If preamble says "code-and-knowledge" but LLM still names clusters after code identifiers → prompt not effective enough → FAIL (prompt quality)
- If `"Top doc nodes:"` appears even for pure-code communities → format violation → FAIL
- If composition line shows wrong counts (e.g., wrong edge count) → incorrect data → FAIL
- If `labelPrompt` crashes on `Nothing` compositions → no graceful degradation → FAIL

### Affected Modules

- `src/Graphos/Domain/Labeling.hs` — update `labelPrompt` signature and implementation
- `src/Graphos/UseCase/Label.hs` — pass `gCompositions` to `labelPrompt`
- Test module for Labeling (existing or new)

### Prerequisites

- Task 2 must be complete: `gCompositions :: Maybe (Map CommunityId CommunityComposition)` on `Graph`
- `labelPrompt` currently accepts graph/community data and produces flat prompt
- `nodeFileType` accessible from node data available in prompt context

### Risks

- **Risk**: `labelPrompt` signature change — adding optional `Map CommunityId CommunityComposition` parameter requires updating all callers. Use default value or `Maybe` to minimize breakage.
- **Risk**: Prompt change affects LLM behavior — labels may change for existing communities. This is the intended outcome (better concept naming). Document as a behavioral change.
- **Medium risk**: Mixed community prompt must handle edge case where a community has 0 code or 0 doc nodes (e.g., pure-doc: only "Top doc nodes:" line).
- **Low risk**: Fallback to flat format is a simple conditional branch.

## Result

Pending implementation.
