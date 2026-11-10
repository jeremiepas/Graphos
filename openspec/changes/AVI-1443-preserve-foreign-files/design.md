## Context

`withStagedOutput` (UseCase/Pipeline/Staging.hs) runs a full build into a
staging directory, then swaps it into the final output directory via directory
renames:

```
old <out>                -> <out>.prev-<ts>-<pid>   (backup)
staging (fresh build)    -> <out>                    (promote)
<out>.prev-*...          -> deleted                  (discard)
```

`carryOverState` (the pre-swap step) previously moved only the four persistent
state subdirectories from `<out>` into staging:

| Entry carried (old)  | Survives swap? |
|----------------------|----------------|
| `cache/`             | yes            |
| `memory/`            | yes            |
| `debug/`             | yes            |
| `traces/`            | yes            |
| any other file/dir   | **no**         |

The rename-and-delete therefore destroyed every foreign entry on every staged
rebuild.

## Decisions

### D1. Preserve by default: carry over every non-owned entry

**Approach.** Enumerate `<out>` and move every entry that is not a Graphos-owned
regenerated artifact into staging before the swap.

```
ownedArtifacts :: [FilePath]   -- regenerable Graphos artifacts
carryOverState oldOut staging =
  move (entries of oldOut not in ownedArtifacts) into staging
```

**Why chosen.** The non-interactive rebuild cannot ask the user whether an
unknown file is safe to delete, so the least-surprising, least-destructive
behavior is to carry it over. It also future-proofs the swap: any future foreign
entry (a new user file, a plugin output) survives automatically, and any future
Graphos artifact is trivially owned by adding it to `ownedArtifacts`.

**Alternatives considered.**
- *Refuse to rebuild when `<out>` holds foreign files.* Safe, but blocks a very
  common workflow (building into a dir that also holds user files) and was
  rejected by the issue as too surprising for a non-interactive run.
- *Carry over only a known persistent-state allowlist.* Leaves new foreign
  files deleted; exactly the class of bug this change removes.
- *Copy foreign files instead of moving.* Unnecessary: the staging directory is
  promoted verbatim on the swap, so a move is strictly cleaner and avoids
  duplicating bytes of large caches.

### D2. Owned = regenerable artifacts; owned directories are not carried over verbatim

Graphos regenerates its artifacts inside staging during the build, so owned
entries are left in the old output and replaced on the swap. This matters for
owned **directories** such as `obsidian/`: carrying them over verbatim would
merge the fresh build's output with stale files from the previous run. The owned
set is intentionally a short list of Graphos-owned names; everything else is
treated as foreign.

| Owned entry            | Written fresh in staging? | Carried over? |
|------------------------|---------------------------|---------------|
| `graph.json`           | yes                       | no            |
| `GRAPH_REPORT.md`      | yes                       | no            |
| `graph.html`           | yes (unless --no-viz)     | no            |
| `community_graph.json` | yes                       | no            |
| `embeddings.json`      | yes (unless --no-embeddings) | no         |
| `graph.checkpoint.json`| yes                       | no            |
| `index.json`           | yes                       | no            |
| `pipeline.checkpoint.json` | cleared on success (so it is never in the fresh staging dir) | no |
| `obsidian/`            | yes (unless --no-obsidian)| no            |
| `graph.cypher`         | yes (unless --neo4j)      | no            |
| `memgraph.cypher`      | yes (unless --memgraph)   | no            |
| any other entry        | no                        | **yes**       |

### D2b. The staged copy always wins over a carried-over duplicate

Because the allowlist is maintained by hand, a Graphos artifact added in the
future without updating `ownedArtifacts` would otherwise be *reverted* to the
previous run's contents: `renameFile` overwrites its destination, so the stale
copy would land on top of the fresh one. `carryOverState` therefore skips any
name that already exists in staging. The allowlist stays the primary mechanism
(carrying over a stale owned artifact is still wrong even when nothing
collides), but the guard makes the failure mode "stale artifact ignored" instead
of "fresh artifact silently replaced".

### D3. Location in the clean-architecture stack

`Staging.hs` is `UseCase.Pipeline`. The change touches only I/O bookkeeping
(directory enumeration, rename); it introduces no domain logic and no new
UseCase entry point. The `ownedArtifacts` list documents the UseCase's contract
with Infrastructure export (which writes those exact filenames). No Domain or
Infrastructure change is required.

## Verification

- `devenv shell -- cabal build graphos` (or `cabal build --flag dev`) compiles the
  changed `Staging.hs` under `-Wall -Werror`.
- `devenv shell -- cabal test graphos-test` — the `Graphos.UseCase.StagingSpec`
  cases cover foreign-file survival (files and directories), owned-directory
  replacement, unknown-entry preservation, stale `pipeline.checkpoint.json`
  not being resurrected, the staged copy winning over a carried-over duplicate,
  and `memory/`/`debug/`/`traces/` survival across a staged rebuild.
- Manual smoke: build into `<out>`, drop a foreign file into `<out>`, rebuild
  with `--fresh -o <out>`, assert the foreign file remains and the owned
  artifacts are updated.
