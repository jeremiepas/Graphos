branch: feature/AVI-1975-merge-gate-pr-200-blockedBy-avi1910 (Graphos PR #200, feature/AVI-1731-definition-of-done)
status: blocked — awaiting review lane AVI-1911 to reach `done`. Current state: in_progress / DIRTY. No merge performed.
PR #200 head: feature/AVI-1731-definition-of-done (+365/-1, state=DIRTY).
Issue AVI-1876 daily intake watchdog (parent) ensures every open PR carries a live review lane and creates missing lanes same day; the `blockedBy` relationship to AVI-1911 is recorded here durably.
Next: merge-gatekeeper re-evaluates each daily run (13:00 Europe/Paris); once AVI-1911 = done, gate opens.
