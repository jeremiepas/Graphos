---
name: mnemo-approve
description: Record a human approval of a mnemo run step. Trigger when a person approves a human step so the run resumes.
---

# mnemo approve

```bash
mnemo approve RUN STEP --by NAME [--space SPACE]
```

- Records who approved a human step of a run, and when; the run goes on at its next resume.
- Human-in-the-loop: only a person records approvals — an agent never runs this.

**Run layer, not the delivery board**: `mnemo approve` is verified working against `graphos` (approves the human step of a run; recording a person's verdict on their explicit instruction). Only `mnemo delivery` is never run against `graphos` — see AGENTS.md.
