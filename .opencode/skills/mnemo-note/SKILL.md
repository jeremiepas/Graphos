---
name: mnemo-note
description: Add, list or confirm a memo (note) on a mnemo delivery workflow. Trigger on "add a note", "add a memo", or listing/confirming delivery-log notes.
---

# mnemo note (add note / memo)

Append a memo to the delivery log on a step, a gate or the issue.

```bash
mnemo delivery note WORKFLOW TARGET --kind KIND --text TEXT [--status STATUS] [--ref KEY] [--supersedes NOTE] [--space SPACE]
mnemo delivery notes WORKFLOW [TARGET] [--space SPACE]                       # list memos in order
mnemo delivery confirm WORKFLOW NOTE --by NAME [--space SPACE]               # a person confirms a memo
```

- **`note`** appends one memo — an answer, a proof, context, a decision or a question — linked to its evidence. The memo is never edited or deleted; the command prints its identifier and never confirms one.
- **`notes`** lists the memos of a workflow (or one target) in the order recorded: id, target, kind, status, text, evidence, time, who confirmed it, what superseded it. Lists only.
- **`confirm`** appends a confirmation of one memo, naming who confirmed it and when. Only a person confirms — an agent never runs `confirm`.

**Delivery feature**: never for `graphos`.
