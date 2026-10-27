# task-logs

Per-issue working notes. One folder per issue identifier, e.g. `task-logs/AVI-1348/`.

Each folder holds a `notes.md` capturing the running reasoning, decisions, and
blockers for that issue so a future heartbeat can resume without replaying the
whole comment thread.

Structure of a note:

```md
# AVI-XXXX <title>

- Status: <one line>
- Current focus: <what to do next>
- Blockers: <first-class issue ids or "none">
- Decisions:
  - <decision> — <rationale>
```
