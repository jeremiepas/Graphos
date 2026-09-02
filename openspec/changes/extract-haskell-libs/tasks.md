## 1. Phase A — Template + scaffolding (homelab)

- [ ] 1.1 Create `templates/haskell-lib-template/` in homelab with: `template.cabal` (Cabal 3.0, library-only, `NAME`/`VERSION` placeholders), `cabal.project` (`packages: .`), `devenv.nix` (GHC 9.10 + cabal + HLS + hspec-discover), `devenv.yaml`, `.envrc`, MIT `LICENSE`, `README.md` (badges + scaffold doc), `CHANGELOG.md` (keep-a-changelog), `.gitignore` (`dist-newstyle/`, `.devenv/`, `.direnv/`), `src/MyLib.hs` (minimal exposed module), `tests/Spec.hs` (hspec-discover entry)
- [x] 1.2 Create `.github/workflows/ci.yml` in template: `runs-on: [self-hosted, kubernetes]`, steps: checkout, `devenv shell` (or nix-shell), `cabal build`, `cabal test`
- [ ] 1.3 Create `.github/workflows/hackage.yml` in template: trigger on tag `v*`, steps: `cabal sdist`, `cabal upload --publish` (using `HACKAGE_USERNAME`/`HACKAGE_PASSWORD` secrets)
- [ ] 1.4 Create `scripts/create-haskell-lib.sh` in homelab: args `<name> <description>`, copies template to `../<name>/`, replaces `NAME`→name / `VERSION`→0.1.0.0 / `DESCRIPTION`→description, `git init`, `gh repo create jeremiepas/<name> --public --source=. --push`, prints next steps
- [ ] 1.5 Verify: run `scripts/create-haskell-lib.sh test-lib "Test"` in a temp dir, confirm repo builds with `devenv shell -c 'cabal build && cabal test'`, then delete test repo via `gh repo delete jeremiepas/test-lib --yes`
- [ ] 1.6 Deploy 6 K3s runners via `scripts/create-runner.sh`: `graphos-types`, `graphos-leiden`, `graphos-cypher`, `graphos-context`, `graphos-lsp-extract`, `graphos-graph-export` (each gets `deployment-<name>.yaml` + `<name>-secret` + HPA under `worker-github/`)

## 2. Phase B — graphos-types (foundation)

- [ ] 2.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-types "Shared types for Graphos and satellite libraries"`
- [ ] 2.2 Copy `src/Graphos/Domain/Types.hs` + `src/Graphos/Domain/Types/*.hs` (Node, Edge, Graph, Pipeline, Analysis, Ingest, Writer, GraphFile) into `src/Graphos/Types/` (or `src/Graphos/Types.hs` single-module if simpler)
- [ ] 2.3 Rewrite imports: replace `Graphos.Domain.Types.*` with the new module names; remove any Graphos-specific imports (Prelude, etc.) or copy minimal helpers
- [ ] 2.4 Update `.cabal`: name `graphos-types`, exposed-modules list, build-depends (`base`, `aeson`, `text`, `containers`, `deepseq`, `time`)
- [ ] 2.5 Copy + adapt relevant tests (`Domain/TypesSpec`, `Domain/ConfigSpec`) into `tests/`
- [ ] 2.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 2.7 Tag `v0.1.0.0`, push, confirm Hackage workflow uploads; `cabal install graphos-types` in clean shell

## 3. Phase C — graphos-lsp-extract (no graphos-types dep, parallel with Phase B)

- [ ] 3.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-lsp-extract "LSP client specialized for code-structure extraction"`
- [ ] 3.2 Copy `src/Graphos/Infrastructure/LSP/{Protocol,Transport,Client,Capabilities,CapabilityParse,Extraction,ServerMap}.hs` into `src/Graphos/LSP/`
- [ ] 3.3 Rewrite imports: remove all `Graphos.*` imports (Protocol + Transport have none; Client re-exports internal sub-modules — rename to `Graphos.LSP.*`); adapt `CapabilityParse` and `Extraction` if they reference Domain types (replace with local minimal types or move shared bits to `graphos-types`)
- [ ] 3.4 Update `.cabal`: name `graphos-lsp-extract`, build-depends (`base`, `aeson`, `bytestring`, `process`, `text`, `containers`, `stm`, `async`, `network`)
- [ ] 3.5 Copy + adapt tests (`LSP.ClientSpec`, `LSP.TransportSpec`)
- [ ] 3.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 3.7 Tag `v0.1.0.0`, publish to Hackage

## 4. Phase C — graphos-leiden (depends on graphos-types)

- [ ] 4.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-leiden "Leiden community detection for Haskell graphs"`
- [ ] 4.2 Copy `src/Graphos/Domain/Community.hs` + `src/Graphos/Domain/Community/Label.hs` into `src/Graphos/Leiden/`
- [ ] 4.3 Rewrite imports: `Graphos.Domain.Types` → `Graphos.Types`; `Graphos.Domain.Graph` → import only the `Graph(..)`, `neighbors`, `gNodes`, `gEdges` record/functions (either from `graphos-types` if moved there, or define a minimal `Graph` record in this package and convert at the boundary)
- [ ] 4.4 Update `.cabal`: build-depends gains `graphos-types >= 0.1 && < 0.2`, plus `vector`, `containers`, `deepseq`, `aeson`, `text`
- [ ] 4.5 Copy + adapt tests (`Domain.CommunitySpec`, `Domain.Community.LabelSpec`)
- [ ] 4.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 4.7 Tag `v0.1.0.0`, publish to Hackage

## 5. Phase C — graphos-cypher (depends on graphos-types)

- [ ] 5.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-cypher "OpenCypher subset AST, parser, and evaluator"`
- [ ] 5.2 Copy `src/Graphos/Domain/Query/Cypher/{AST,Parser,Eval,Mapping}.hs` into `src/Graphos/Cypher/`
- [ ] 5.3 Rewrite imports: `Graphos.Domain.Types` → `Graphos.Types`; update module names to `Graphos.Cypher.*`
- [ ] 5.4 Update `.cabal`: build-depends gains `graphos-types >= 0.1 && < 0.2`, plus `megaparsec`, `containers`, `text`, `aeson`
- [ ] 5.5 Copy + adapt tests (`Cypher.ParserSpec`, `Cypher.EvalSpec`, `Cypher.MappingSpec`)
- [ ] 5.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 5.7 Tag `v0.1.0.0`, publish to Hackage

## 6. Phase C — graphos-context (depends on graphos-types)

- [ ] 6.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-context "LLM context-graph selection and token-budgeted formatting"`
- [ ] 6.2 Copy `src/Graphos/Domain/Context.hs` + `src/Graphos/UseCase/SelectContext.hs` + `src/Graphos/UseCase/FormatContext.hs` into `src/Graphos/Context/`
- [ ] 6.3 Rewrite imports: `Graphos.Domain.Types` → `Graphos.Types`; `Graphos.Domain.Types.Node` → `Graphos.Types.Node`; `Graphos.Domain.Graph` → minimal `Graph` interface from `graphos-types` or local record
- [ ] 6.4 Update `.cabal`: build-depends gains `graphos-types >= 0.1 && < 0.2`, plus `text`, `containers`, `aeson`
- [ ] 6.5 Copy + adapt tests (`Domain.ContextSpec`, `SelectContextSpec`, `SelectContextNoiseSpec`, `FormatContextSpec`, `FormatContextBudgetSpec`, `FormatContextHintsSpec`, `ContextNoiseRegressionSpec`)
- [ ] 6.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 6.7 Tag `v0.1.0.0`, publish to Hackage

## 7. Phase C — graphos-graph-export (depends on graphos-types)

- [ ] 7.1 Scaffold repo: `scripts/create-haskell-lib.sh graphos-graph-export "Unified graph export: Neo4j Cypher, GraphML, Obsidian, JGF"`
- [ ] 7.2 Copy `src/Graphos/Infrastructure/Export/{Neo4j,GraphML,Obsidian,JSON}.hs` into `src/Graphos/Export/`
- [ ] 7.3 Rewrite imports: `Graphos.Domain.Types` → `Graphos.Types`; `Graphos.Domain.Graph` → `Graphos.Types.Graph` (or minimal interface); `Graphos.Domain.Community` → `Graphos.Leiden` (depends on `graphos-leiden`); `Graphos.Domain.Community.Label` → `Graphos.Leiden.Label`
- [ ] 7.4 Update `.cabal`: build-depends gains `graphos-types >= 0.1 && < 0.2`, `graphos-leiden >= 0.1 && < 0.2`, plus `aeson`, `bytestring`, `text`, `containers`, `directory`, `process`
- [ ] 7.5 Copy + adapt tests (`ExportSpec`, `HTMLSpec` if JGF-related)
- [ ] 7.6 Verify: `devenv shell -c 'cabal build && cabal test'`
- [ ] 7.7 Tag `v0.1.0.0`, publish to Hackage

## 8. Phase D — Graphos integration

- [ ] 8.1 Add to `graphos.cabal` build-depends: `graphos-types >= 0.1 && < 0.2`, `graphos-leiden >= 0.1 && < 0.2`, `graphos-cypher >= 0.1 && < 0.2`, `graphos-context >= 0.1 && < 0.2`, `graphos-lsp-extract >= 0.1 && < 0.2`, `graphos-graph-export >= 0.1 && < 0.2`
- [ ] 8.2 Rewrite imports in remaining Graphos modules: replace `Graphos.Domain.Types` → `Graphos.Types`, `Graphos.Domain.Community` → `Graphos.Leiden`, `Graphos.Domain.Query.Cypher.*` → `Graphos.Cypher.*`, `Graphos.Domain.Context` → `Graphos.Context`, `Graphos.Infrastructure.LSP.*` → `Graphos.LSP.*`, `Graphos.Infrastructure.Export.*` → `Graphos.Export.*`
- [ ] 8.3 Remove extracted source files from `src/Graphos/`: `Domain/Types.hs`, `Domain/Types/`, `Domain/Community.hs`, `Domain/Community/`, `Domain/Query/Cypher/`, `Domain/Context.hs`, `Infrastructure/LSP/`, `Infrastructure/Export/{Neo4j,GraphML,Obsidian,JSON}.hs`
- [ ] 8.4 Update `exposed-modules` and `other-modules` in `graphos.cabal` to drop removed modules
- [ ] 8.5 Verify: `cabal build --flag dev` passes with zero warnings (-Werror)
- [ ] 8.6 Verify: `cabal test` passes all existing specs (Types, Community, Cypher, Context, LSP, Export, Pipeline, Query, etc.) with no regressions
- [ ] 8.7 Update `README.md` architecture diagram to reflect the 6 published dependencies
- [ ] 8.8 Update `openspec/config.yaml` context block to list the 6 packages in the tech stack

## 9. Phase E — Verification + documentation

- [ ] 9.1 In a clean shell (no Graphos checkout): `cabal install graphos-leiden graphos-cypher graphos-context graphos-lsp-extract graphos-graph-export` — all 5 install without pulling `graphos`
- [ ] 9.2 Verify each Hackage page renders docs (Hackage build report)
- [ ] 9.3 Add a `CHANGELOG.md` entry in Graphos: "Extracted 6 libraries to Hackage (graphos-types, graphos-leiden, graphos-cypher, graphos-context, graphos-lsp-extract, graphos-graph-export)"
- [ ] 9.4 Verify homelab `worker-github/` has 6 new runner deployments and all pods are `Running`