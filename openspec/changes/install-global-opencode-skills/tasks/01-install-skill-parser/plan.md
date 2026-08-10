# Task 1 — Add `install-skill` CLI parser and wire to program entrypoint — PLAN

**Task slug**: `01-install-skill-parser`
**Attempt**: 1
**Status**: pending

## Summary

Introduce a new top-level `install-skill` subcommand with a required `--target` option, limited to `opencode` for now, and route it to an Infrastructure handler without changing existing command behavior.

## Detail

### Scope
- Add `InstallSkillTarget` type with one constructor `OpencodeTarget` in the CLI parser module.
- Add `install-skill` parser branch with required `--target <TARGET>` option.
- Route the parsed subcommand to a new `runInstallSkill` handler in the program entrypoint.
- Keep changes minimal and consistent with existing `graphos init` parser patterns.

### Check Criteria (defined before code)
- C1: `graphos --help` lists `install-skill` as a subcommand and `--target` as its option.
- C2: `graphos install-skill` (missing `--target`) exits with a usage error that mentions `--target`.
- C3: `graphos install-skill --target foo` exits with an error naming valid targets (`opencode`).
- C4: `graphos install-skill --target opencode` reaches the new handler (verified by a smoke test or unit test stub).
- C5: `cabal build --flag dev` produces zero warnings.

### Affected Modules
- `src/Graphos/CLI/Parser.hs` (or equivalent)
- `src/Graphos/Program.hs` (or equivalent entrypoint)
- Possibly `src/Graphos/CLI/Types.hs` if command types live there

### Prerequisites
- Existing parser structure and command ADT are known.

### Risks
- Routing change could accidentally affect `init` or other commands if the ADT pattern match is incomplete. Mitigation: keep pattern matches exhaustive and let GHC warn.
