# Install prompt

Download `graphos-skill.zip`, open Claude Code in any directory, and paste the prompt below. Replace the path on the first line if the zip is somewhere else.

---

Install the graphos skill from `~/Downloads/graphos-skill.zip` and set graphos up with the same config as the person who shared it.

1. Unzip it into a temporary directory (not into `~/.claude/skills` directly). Read `graphos/install.sh` and `graphos/config/graphos.global.yaml` before running anything, and tell me in two lines what the script will change.
2. If `~/.config/graphos/graphos.yaml` already exists, show me how it differs from `graphos/config/graphos.global.yaml` and ask whether to replace it (the installer's `--force-config` backs the old one up first). Don't replace it without my answer.
3. Run `graphos/install.sh` (add `--force-config` only if I said yes, or `--skip-binary` if `graphos` is already installed and runs). If it stops because `patchelf`, a shared library or `jq` is missing, give me the exact `sudo` command to run with `!` and re-run the installer once I have.
4. Verify:
   - `graphos lservers` runs.
   - `~/.claude/skills/graphos/SKILL.md` exists, and nothing else named `graphos*` sits in `~/.claude/skills/`.
   - A tiny test build works: two TypeScript files in a temp directory where one imports the other, built with the project template `config/graphos.project.yaml` and the command from the skill's "Build" section. The output `graph.json` should contain `Module` and `Function` nodes and an `imports` edge, which you can check with the skill's `jq` recipe. Delete the temp directory afterwards.
5. Report what was installed and where, which config is now active, and anything skipped. Remind me to start a new Claude Code session so the skill loads.

---

## Manual install (no Claude)

```bash
unzip graphos-skill.zip -d /tmp && /tmp/graphos/install.sh
```

Requirements: Linux x86_64 (other platforms: build from source with `cabal install exe:graphos`, GHC 9.10), `curl`, `sha256sum`. On non-Nix distros also `patchelf` plus `libgmp`, `libffi`, `libnuma`, `libelf`, `libdw`. Optional: `jq`, and Docker for the Neo4j stack in the Graphos repo.
