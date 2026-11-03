# GITFLOW.md — Branch/merge model (merge perspective)

- `main` is the single integration + production branch; **no `develop`**; do not create or target `develop` for new work.
- Per issue AVI-XXX: `git worktree add .worktrees/AVI-XXX -b feature/AVI-XXX main` (worktrees live in the Graphos repo); PRs target `main`.
- Existing worktrees under `.worktrees/` and `.paperclip/worktrees/` belong to other issues — do not touch them.
- The board may override; revisit GitFlow only if release isolation becomes the constraint.

## Merge authority
Only `merge-gatekeeper` merges into `main`. No direct pushes to `main`.

## Release-tag push
After merging a release PR, immediately push an annotated tag `vX.Y.Z` named in the release issue and record SHA + release URL.
- Post-merge: re-check remaining open PRs and file rebase tasks for new conflicts.
