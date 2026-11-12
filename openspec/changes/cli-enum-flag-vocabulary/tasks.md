# Tasks — CLI enum flag vocabulary

Check criteria are written before the code they gate (anti-bias guard). Groups 1–2
are foundation, 3–5 are the rewiring, 6–7 integration.

## 1. Token tables in Domain

- [ ] 1.1 Export `edgeDensityTokens :: [(String, EdgeDensity)]` from `Graphos.Domain.Types.Pipeline` with `sparse|normal|dense|maximum`
- [ ] 1.2 Export `neo4jPushModeTokens` (`full|subgraph|community`) and `memgraphPushModeTokens` (`full|subgraph|community`) from the same module
- [ ] 1.3 Export the alias tables (constructor spellings) alongside each primary table
- [ ] 1.4 Check: tables are pure values, no IO import added to Domain; `cabal build --flag dev` green under `-Wall -Werror`

## 2. Reader combinators

- [ ] 2.1 Add `src/Graphos/CLI/Readers.hs` with `data EnumFlag a = EnumFlag { efName, efMetavar, efTokens, efAliases }`
- [ ] 2.2 Implement `enumReader :: EnumFlag a -> ReadM a` — case-insensitive lookup over primaries then aliases
- [ ] 2.3 Implement `enumHelp :: EnumFlag a -> String` (renders `a|b|c`) and `enumOption :: EnumFlag a -> Mod OptionFields a` (sets metavar)
- [ ] 2.4 Implement the error string: `--flag: unknown value 'x'. Expected one of: a, b, c`
- [ ] 2.5 Add `boundedDoubleReader :: String -> Double -> Double -> ReadM Double` next to the existing `boundedIntReader`, and move `boundedIntReader` into the new module
- [ ] 2.6 Register the module in `graphos.cabal` `exposed-modules`
- [ ] 2.7 Check: unit tests for `enumReader` (primary hit, alias hit, case folding, miss message) green

## 3. Rewire the five broken enum options

- [ ] 3.1 `--edge-density` on `pipelineOpts` and `mergeOpts` → `enumOption`, default `Normal` unchanged
- [ ] 3.2 `--neo4j-push-mode` on `pipelineOpts` and `--mode` on `pushOpts` → `enumOption`, defaults `SubgraphPush` unchanged
- [ ] 3.3 `--memgraph-push-mode` on `pipelineOpts` and `--mode` on `pushMemgraphOpts` → `enumOption`, defaults `MemgraphSubgraph` unchanged
- [ ] 3.4 Check: `graphos --edge-density sparse .` and `graphos --neo4j-push-mode full .` both parse; `--edge-density Sparse` still parses; `graphos --help` shows `--edge-density DENSITY`

## 4. Fold the already-correct readers into the same mechanism

- [ ] 4.1 `granularityReader` → `EnumFlag` (`fine|function|file`); keep `granularityReader` exported if other call sites depend on it
- [ ] 4.2 `detectModeReader` → `EnumFlag` (`exclude|collapse|off`)
- [ ] 4.3 `edgeModeReader` → `EnumFlag` (`semantic|all`), shared by `commonQueryOptsP` and `researchOpts`
- [ ] 4.4 `--cluster-algorithm` → `EnumFlag` (`leiden|bunch`) wrapping `parseAlgorithmName`
- [ ] 4.5 Add `enumFlagRegistry` to `Graphos.CLI.Parser` exports, listing every `EnumFlag` in the CLI
- [ ] 4.6 Check: existing `ParserSpec` cases for `--edges bogus` (rejects) and `--edges all|semantic` (accepts) still pass unchanged

## 5. Numeric bounds and metavars

- [ ] 5.1 Port-valued options (`--port`, `--metrics`) → `boundedIntReader` `1..65535`, metavar `PORT`
- [ ] 5.2 Count-valued options with a `>= 1` bound (per the design bound table) → `boundedIntReader`, metavar `N`
- [ ] 5.3 Options where `0` is a documented sentinel (`--max-nodes`, `--max-label-chars`, `--budget`, `--label-width`, `--depth`, `--boundary-hops`) → `>= 0`, metavar `N`
- [ ] 5.4 `--resolution` → `boundedDoubleReader` `> 0`, metavar `FLOAT`; timeout options get metavar `SECONDS`
- [ ] 5.5 Check: `--threads 0`, `--threads=-1`, `serve --port 99999`, `--resolution 0` all rejected with the range named; `--max-nodes 0` still accepted

## 6. Tests

- [ ] 6.1 Registry-driven test: for each `EnumFlag`, every primary token parses
- [ ] 6.2 Registry-driven test: for each `EnumFlag`, every primary token appears in the rendered `--help` body (via `renderHelp`/`parserHelp`, as the existing semantic-edges test does)
- [ ] 6.3 Registry-driven test: an unknown token fails with a message containing every primary token
- [ ] 6.4 Registry-coverage test: every enum-valued option in the parser has a registry entry
- [ ] 6.5 Alias tests: `Sparse`, `FullPush`, `MemgraphSubgraph` parse to the same values as their lowercase primaries; aliases absent from help
- [ ] 6.6 Default-preservation test: `parsePipeline []`, `parseWith pushOpts []`, `parseWith pushMemgraphOpts []`, `parseServe []` produce the pre-change defaults
- [ ] 6.7 Bounds tests per the design bound table, including the `0`-sentinel cases
- [ ] 6.8 Check: `cabal test` green; no existing `ParserSpec` expectation modified except where a metavar string is asserted

## 7. Documentation and reference

- [ ] 7.1 `renderCommandReference`: print `sparse|normal|dense|maximum` for `--edge-density` and the push-mode vocabularies, staying within the 60-line budget
- [ ] 7.2 Fix `docs/workflows/01-full-pipeline.md` and `docs/workflows/09-merge.md` — `--edge-density` is token-valued, not `N`/`0.0`
- [ ] 7.3 Align `docs/workflows/12-neo4j-push.md` and `13-memgraph-push.md` push-mode tables on `full | subgraph | community`
- [ ] 7.4 Confirm `README.md` line on `--edge-density` matches the reader (it already does — verify, do not rewrite)
- [ ] 7.5 Check: `cabal build --flag dev && cabal test` green; manual run of the four verification commands in design.md §Verification
