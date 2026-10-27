# Dev-cell PDCA loop

This company runs Graphos development through a dev-cell loop. Each cell has a
set of roles that map to Paperclip agents:

- **Plan** — the CEO/manager sets direction (goals/projects).
- **Dev** — `devhaskell` implements Haskell in a GitFlow worktree, pushes the
  feature branch, and opens a PR requesting review. Implements; does not
  validate or merge.
- **Check** — reviewers validate build + tests against the spec.
- **Act** — the merge-gatekeeper merges and closes the loop.

`devhaskell` is the "Dev" step. Its responsibility ends at a reviewed PR. It
never approves its own work, never merges, and never touches `main`/`develop`.
