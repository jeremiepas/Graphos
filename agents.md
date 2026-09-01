# Agents

## OpenSpec Change Agent
1. **Think**: Use `openspec-explore` to investigate requirements and design.
2. **Apply**: Use `openspec-apply-change` to implement the changes.

## Workspace Development Workflow

Each agent works in a Git worktree linked to a branch. See the [decision tree](docs/decision-tree.md) for the full workflow.

- **Workspace location**: `.worktrees/<worktree-name>` — each worktree is linked to a branch
- **Creating a workspace**: `git worktree add .worktrees/<name> -b <branch> <base-branch>`
- **Development**: Work in the worktree directory, commit locally as needed
- **Finishing development**: Push the branch to `jeremie` remote (not commit at editor paperclip)
- **Cleanup**: Remove worktree when done: `git worktree remove .worktrees/<name>`
- **CI monitoring**: After pushing, verify CI is green via `gh run list --branch <branch>`