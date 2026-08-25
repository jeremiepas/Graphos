# 5.P Plan: Validate the change and confirm load

## Scope
Run OpenSpec validation and status checks, then provide the user with restart/load verification instructions.

## Affected areas
None — read-only validation.

## Check criteria
(a) `openspec validate --change add-goal-coding-loop` reports no violations.
(b) `openspec status --change add-goal-coding-loop` shows `proposal`, `specs`, `design`, `tasks` all `done`.
(c) User confirms (or agent reports, if it can introspect) that `goal-orch` and `/goal` are available after restart.

## Risk
OpenCode caches agent files; a restart may be needed to see the new agent/command.
