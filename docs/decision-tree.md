# Decision Tree — Workspace Workflow

```
Need to work on a task?
│
├─ No active worktree for this task?
│  └─ Run: git worktree add .worktrees/<name> -b <branch> <base-branch>
│
├─ Working in a worktree?
│  └─ Commit locally as needed (worktree shares the same repo)
│
├─ Done developing?
│  ├─ Push branch to jeremie remote: git push jeremie <branch>
│  ├─ Verify CI: gh run list --branch <branch>
│  └─ Clean up: git worktree remove .worktrees/<name>
│
└─ Need to switch tasks?
   └─ Remove old worktree, create new one for the new task
```

## Quick Reference

| Step | Command |
|------|---------|
| Create worktree | `git worktree add .worktrees/<name> -b <branch> <base-branch>` |
| List worktrees | `git worktree list` |
| Push branch | `git push jeremie <branch>` |
| Check CI | `gh run list --branch <branch>` |
| Remove worktree | `git worktree remove .worktrees/<name>` |
