#!/usr/bin/env bash
# Overnight batch: run every generated graphos change workflow in mnemo,
# serially, against the `graphos` space. Continues past failures and logs
# everything to /tmp/opencode/graphos-overnight.log.
set +e
export PATH="$HOME/.local/bin:$PATH"
cd "$HOME/Documents/perso/Graphos" || exit 2

LOG=/tmp/opencode/graphos-overnight.log
: > "$LOG"

echo "=== overnight batch start $(date -Is) ===" >> "$LOG"

# Wait for the in-flight split-graphos-studio run (a9c0) to leave "running".
# Its delegate steps share the single delegate slot; we do not start another
# run until the slot is free. A run that is "waiting" (human gate) or
# "failed"/"abandoned" does not block us.
while mnemo-graphos status 2>/dev/null | grep -q "[[:space:]]running[[:space:]]"; do
  sleep 30
done
echo "=== slot free at $(date -Is) ===" >> "$LOG"

for f in $(ls openspec/changes/*/workflow.yaml | sort); do
  slug=$(basename "$(dirname "$f")")
  [ "$slug" = "split-graphos-studio" ] && continue
  echo "=== $slug start $(date -Is) ===" >> "$LOG"
  mnemo-graphos workflow import "$f" >> "$LOG" 2>&1
  mnemo-graphos run "$slug" >> "$LOG" 2>&1
  echo "=== $slug exit=$? $(date -Is) ===" >> "$LOG"
done

echo "=== overnight batch done $(date -Is) ===" >> "$LOG"
