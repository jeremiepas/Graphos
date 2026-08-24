## Why

Graphos contains several self-contained modules that have value beyond the
project itself and that fill gaps in the Haskell package ecosystem. Currently
they are buried inside the `graphos` Cabal library with no public visibility
and no way for other Haskell tools to depend on them. Hackage searches confirm:

- **Community detection:** zero Haskell packages exist on Hackage/Hoogle.
- **OpenCypher AST + evaluator:** the only `cypher` package is from 2012,
  abandoned, REST-only, and incompatible with modern GHC.
- **LLM context-graph selection:** novel niche, no existing library.
- **LSP code-extraction client:** `lsp-client` (0.4.0) is a generic session
  library; no library is purpose-built for extracting code structure (symbols
  → nodes, callHierarchy → edges).
- **Unified graph export (Neo4j / Obsidian / GraphML / JGF):** no unified
  graph-export library exists.

Extracting these into **separate public Hackage packages** — each in its own
GitHub repository with its own self-hosted CI runner — gives the Haskell
ecosystem reusable building blocks, raises Graphos's visibility, and forces
clean interface boundaries that improve the main project's architecture.

## What Changes

- Create a **Haskell library template** in the homelab repository
  (`templates/haskell-lib-template/`) with: Cabal 3.0 library scaffold,
  `devenv.nix` (GHC 9.10 + HLS), MIT license, GitHub Actions CI workflow
  (self-hosted K3s runner), Hackage release workflow, hspec-discover test
  harness.
- Create a **scaffolding script** (`scripts/create-haskell-lib.sh`) that
  copies the template, replaces placeholders, creates a public GitHub repo via
  `gh`, and pushes.
- Extract **6 libraries** as separate public repos under `jeremiepas/`:

  | Package | Source modules | Intra-Graphos deps |
  |---------|---------------|-------------------|
  | `graphos-types` | `Domain/Types.hs` + `Types/*` | none (foundation) |
  | `graphos-leiden` | `Domain/Community.hs` + `Community/Label.hs` | `graphos-types` |
  | `graphos-cypher` | `Domain/Query/Cypher/*` (AST, Parser, Eval, Mapping) | `graphos-types` |
  | `graphos-context` | `Domain/Context.hs` + `UseCase/SelectContext` + `FormatContext` | `graphos-types` |
  | `graphos-lsp-extract` | `Infrastructure/LSP/*` (Protocol, Transport, Client, Capabilities, Extraction, ServerMap) | none |
  | `graphos-graph-export` | `Infrastructure/Export/*` (Neo4j, GraphML, Obsidian, JSON/JGF) | `graphos-types` |

- Deploy **6 self-hosted GitHub Actions runners** on the homelab K3s cluster
  (one per repo) via existing `scripts/create-runner.sh`.
- Update `graphos.cabal` to depend on the published Hackage packages and
  **remove** the corresponding source modules from `src/`.

## Capabilities

### New Capabilities
- `haskell-lib-template`: a reusable devenv + Cabal + CI + Hackage scaffold
  for Haskell library projects, living in the homelab repository.
- `haskell-lib-scaffold-script`: a single-command script that creates a new
  public Haskell library repo from the template (GitHub repo + initial push).
- `graphos-types`: a published Hackage package containing the shared
  Node/Edge/Graph/Relation/Confidence types used by Graphos and its satellite
  libraries.
- `graphos-leiden`: a published Hackage package implementing the Leiden
  community-detection algorithm (pure, vector-based).
- `graphos-cypher`: a published Hackage package providing an OpenCypher
  subset AST, parser, and evaluator.
- `graphos-context`: a published Hackage package for LLM context-graph
  selection and token-budgeted formatting.
- `graphos-lsp-extract`: a published Hackage package providing an LSP client
  specialized for code-structure extraction.
- `graphos-graph-export`: a published Hackage package unifying Neo4j Cypher,
  GraphML, Obsidian, and JGF exporters.

### Modified Capabilities
- The `graphos` library package replaces internal source modules with Hackage
  dependencies; its public API and CLI behaviour are unchanged.

## Impact

- **Homelab:** new `templates/haskell-lib-template/` directory; new
  `scripts/create-haskell-lib.sh`; 6 new runner deployments under
  `worker-github/`.
- **Graphos `src/`:** the extracted module directories are removed once the
  Hackage deps are wired in. The remaining Graphos code imports the published
  packages instead.
- **`graphos.cabal`:** build-depends gains `graphos-types`, `graphos-leiden`,
  `graphos-cypher`, `graphos-context`, `graphos-lsp-extract`,
  `graphos-graph-export`; exposed-modules list shrinks accordingly.
- **External consumers:** any tool can now `cabal install graphos-leiden`
  without pulling in the entire Graphos codebase.
- **Migration risk:** mitigated by extracting in dependency order
  (types → dependents) and verifying `cabal build --flag dev && cabal test`
  after each wiring step.