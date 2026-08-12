# Task 1 — Add `install-skill` CLI parser and wire to program entrypoint — DO

**Task slug**: `01-install-skill-parser`
**Attempt**: 1
**Status**: in-progress

## Summary

Define the new `install-skill` subcommand with a closed `--target` enum, add it to the CLI parser, and route it to a new Infrastructure handler in the program entrypoint.

## Detail

### What will be implemented
- Add `InstallSkillTarget` data type in the CLI types module (or `Parser` module if types are colocated):
  ```haskell
data InstallSkillTarget = OpencodeTarget
  deriving (Eq, Show, Read, Bounded, Enum)
  ```
- Add a parser for `--target <TARGET>` using `strOption` with `eitherReader` over the bounded enum, mirroring the existing `--agents` target parser style.
- Add an `InstallSkill InstallSkillTarget` constructor to the command ADT (or equivalent subcommand representation).
- Add the `install-skill` subcommand to the top-level `subparser`, with help text explaining it installs user-level assistant skills.
- In the program entrypoint, add a branch for `InstallSkill target` that calls `runInstallSkill target`.

### Key decisions
- Use a closed enum so invalid targets are rejected by the parser with a clear message listing valid values.
- Keep `install-skill` as a separate top-level command rather than an `init` flag to emphasize the global-install semantics.

### Concrete changes
- `src/Graphos/CLI/Parser.hs`: new `InstallSkillTarget` + `installSkillParser`.
- `src/Graphos/CLI/Types.hs` (if exists): add `InstallSkill InstallSkillTarget` to command ADT.
- `src/Graphos/Program.hs`: new branch routing to `runInstallSkill`.

## Result

Implementation pending `/opsx-apply`. This `do.md` records the planned approach.
