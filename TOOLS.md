# TOOLS.md — `gh` merge tooling, git verification, Paperclip lane API, graphos/devenv gates

- `gh pr merge --merge`, `gh pr view/list`, `gh run view --branch`; git read-only verification.
- Paperclip `GET /api/issues/<id>` to read lane status/blockers.
- `scripts/post-github-review.sh <pr-number> <APPROVE|REQUEST_CHANGES|COMMENT> [body]` — materializes a review verdict on GitHub via REST API (`POST /repos/{owner}/{repo}/pulls/{n}/reviews event=APPROVE/REQUEST_CHANGES/COMMENT`). 
  - `haskell-reviewer` posts the code-review approval; `merge-gatekeeper` relies on the GitHub approval check.
- `graphos view` + `devenv task run ci:*` to interpret verifier/validator reports (speccheck / intentcheck / impact).
