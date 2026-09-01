#!/usr/bin/env bash
set -euo pipefail

# provision-workspace.sh — Create an isolated git worktree for an OpenSpec change.
#
# Usage:
#   scripts/provision-workspace.sh <ISSUE_ID> <CHANGE_NAME>
#
# Creates a git worktree at .paperclip/worktrees/{ISSUE}-{CHANGE_NAME}/
# checked out on branch {ISSUE}-{CHANGE_NAME}, based on the main branch.
# Initializes the OpenSpec change directory with schema and created date.
#
# Exit codes:
#   0  — worktree created successfully
#   1  — invalid arguments
#   2  — worktree already exists
#   3  — main branch not found
#   4  — git worktree creation failed

ISSUE_ID="${1:-}"
CHANGE_NAME="${2:-}"

if [[ -z "$ISSUE_ID" || -z "$CHANGE_NAME" ]]; then
  echo "Usage: $0 <ISSUE_ID> <CHANGE_NAME>" >&2
  exit 1
fi

# Sanitize change name: only alphanumeric, hyphens, underscores
if [[ ! "$CHANGE_NAME" =~ ^[a-zA-Z0-9_-]+$ ]]; then
  echo "Error: CHANGE_NAME must be alphanumeric (hyphens/underscores allowed)" >&2
  exit 1
fi

BRANCH_NAME="${ISSUE_ID}-${CHANGE_NAME}"
WORKTREE_NAME="${ISSUE_ID}-${CHANGE_NAME}"
WORKTREE_DIR=".paperclip/worktrees/${WORKTREE_NAME}"

# Ensure .paperclip/worktrees directory exists
mkdir -p .paperclip/worktrees

# Check if worktree already exists
if [[ -d "$WORKTREE_DIR" ]]; then
  echo "Error: Worktree already exists at $WORKTREE_DIR" >&2
  exit 2
fi

# Check if branch already exists
if git branch --list "$BRANCH_NAME" | grep -q "^\\*\\? *$BRANCH_NAME$"; then
  echo "Error: Branch $BRANCH_NAME already exists" >&2
  exit 2
fi

# Check if main branch exists
if ! git rev-parse --verify main >/dev/null 2>&1; then
  echo "Error: main branch not found" >&2
  exit 3
fi

# Create the git worktree
git worktree add -b "$BRANCH_NAME" "$WORKTREE_DIR" main 2>/dev/null

if [[ $? -ne 0 ]]; then
  echo "Error: Failed to create worktree at $WORKTREE_DIR" >&2
  exit 4
fi

# Initialize the OpenSpec change directory in the worktree
CHANGES_DIR="$WORKTREE_DIR/openspec/changes/$CHANGE_NAME"
mkdir -p "$CHANGES_DIR"

# Create .openspec.yaml with schema and creation date
cat > "$CHANGES_DIR/.openspec.yaml" <<EOF
schema: spec-driven
created: $(date +%Y-%m-%d)
EOF

# Create initial tasks.md if it doesn't exist
if [[ ! -f "$CHANGES_DIR/tasks.md" ]]; then
  cat > "$CHANGES_DIR/tasks.md" <<EOF
# Tasks for $ISSUE_ID: $CHANGE_NAME

## Status
- [ ] Initialize change directory
- [ ] Review implementation tasks
EOF
fi

echo "$WORKTREE_DIR"
