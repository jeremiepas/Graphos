# Task 4 — Add enterShell hook to create orchestrator output directory — DO

**Task slug**: `04-add-enterShell-hook`
**Attempt**: 1
**Status**: in-progress

## Summary

Added `enterShell` hook that runs `mkdir -p graphos-out/orchestrator` to ensure the log directory exists before the orchestrator service writes any output.

## Detail

### Implementation
- Added `mkdir -p graphos-out/orchestrator` to `enterShell` hook
- Directory creation runs on every shell activation
- Idempotent operation, safe to run multiple times

### Key decisions
- Placed at top of `enterShell` hook to ensure directory exists before any other operations
- Used `mkdir -p` for idempotency and safety

### Concrete changes
- Modified file: `devenv.nix` (updated `enterShell` hook)
