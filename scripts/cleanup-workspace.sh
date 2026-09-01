#!/usr/bin/env bash
set -euo pipefail

# cleanup-workspace.sh — Remove an isolated git worktree created by provision-workspace.sh.
#
# Usage:
#   scripts/cleanup-workspace.sh <ISSUE_ID> <CHANGE_NAME>
#
# Removes the git worktree at .paperclip/worktrees/{ISSUE}-{CHANGE_NAME}/
# and deletes the corresponding branch.
# Also removes the OpenSpec change directory if it exists.
#
# Exit codes:
#   0  — worktree removed successfully
#   1  — invalid arguments
#   2  — worktree does not exist

ISSUE_ID="${1:-}"
CHANGE_NAME="${2:-}"

if [[ -z "$ISSUE_ID" || -z "$CHANGE_NAME" ]]; then
  echo "Usage: $0 <ISSUE_ID> <CHANGE_NAME>" >&2
  exit 1
fi

BRANCH_NAME="${ISSUE_ID}-${CHANGE_NAME}"
WORKTREE_NAME="${ISSUE_ID}-${CHANGE_NAME}"
WORKTREE_DIR=".paperclip/worktrees/${WORKTREE_NAME}"

# Check if worktree exists
if [[ ! -d "$WORKTREE_DIR" ]]; then
  echo "Worktree does not exist at $WORKTREE_DIR"
  exit 2
fi

# Remove the git worktree
git worktree remove -f "$WORKTREE_DIR" 2>/dev/null

if [[ $? -eq 0 ]]; then
  echo "Removed worktree: $WORKTREE_DIR"
else
  echo "Error: Failed to remove worktree at $WORKTREE_DIR" >&2
  exit 1
fi

# Clean up the branch if it exists
if git branch --list "$BRANCH_NAME" | grep -q "^\\*\\? *$BRANCH_NAME$"; then
  git branch -D "$BRANCH_NAME" 2>/dev/null
  echo "Removed branch: $BRANCH_NAME"
fi
