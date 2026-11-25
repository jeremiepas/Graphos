---
name: mnemo-answer
description: Record a person's answer to a question step of a mnemo run. Trigger when a human answers a question step so the run resumes.
---

# mnemo answer

```bash
mnemo answer RUN STEP --by NAME --answer JSON-OR-FILE [--space SPACE]
```

- Records a person's answer to a question step, and when; it must follow the step's output schema.
- The run goes on at its next resume. No model is called.

**Run layer, not the delivery board**: `mnemo answer` belongs to the workflow/run layer (it resumes a run at a question step, no delivery config involved), so it is usable against `graphos`. Only `mnemo delivery` is never run against `graphos` — see AGENTS.md.
