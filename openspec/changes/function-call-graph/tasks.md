## 1. Baseline and check criteria (before any code)

- [ ] 1.1 Add the two-file TypeScript fixture of the proposal under `test-corpus/`, with the expected call pairs listed at the top of the spec that uses it. Check: fixture builds; the list has 3 pairs (`verdict → helper`, `wrap → verdict`, `K.run → verdict`).
- [ ] 1.2 Record the baseline in the change's `README.md`: node and edge counts of the fixture at `function` and `fine`, and of one real repository at `function`. Check: the table matches M1 and M2 of the design, or states the difference.
- [ ] 1.3 Confirm the state of `extraction-semantic-enrichment`: archived or not, and whether the main `extraction` spec holds the per-language call node types. Check: one line in `README.md`; if it is not archived, note that this change must archive after it.
- [ ] 1.4 Write the failing specs: converter (function-granularity edges, equal node ids, function/fine agreement), build stage (cross-file, renamed import, outside corpus, ambiguous), queries (callers, callees, depth, other relations ignored, ambiguous name, no call edges, empty result). Check: `cabal test` lists them, red or pending.

## 2. Call edges at function granularity

- [ ] 2.1 Make `callEdgePairs` descend into opaque bodies while keeping the nearest emitted definition as the source. Check: converter specs pass; the node-id-equality spec passes.
- [ ] 2.2 Add one Java and one Python case to the converter specs. Check: each emits its same-file call edge at `function` granularity.
- [ ] 2.3 Change the extraction fingerprint so cached extractions from before the change are not reused. Check: a spec shows the fingerprint differs from the previous constant.

## 3. Cross-file callee resolution

- [ ] 3.1 Read how the Build use case merges import targets, then fix the shape of the pending call record and where the pass runs; write it in `README.md`. Check: the note names the functions touched.
- [ ] 3.2 Record pending calls in the extractor for callees bound by an import, including the original name of a renamed import. Check: converter spec asserts the records for the fixture.
- [ ] 3.3 Resolve the records in the build stage and write the final edges. Check: the four build-stage scenarios pass; `graph.json` of the fixture has no pending-call data.

## 4. Queries

- [ ] 4.1 Add the pure relation-and-direction traversal to `UseCase/Query.hs`. Check: query specs pass.
- [ ] 4.2 Add `callers` and `callees` to `CLI/Parser.hs` and `app/Main.hs` with the common query options, `--depth` and `--path`. Check: `--help` works on both; the uniform-flag spec of the query family covers them.
- [ ] 4.3 Implement the `no_call_edges` outcome for text and JSON. Check: both scenarios pass.
- [ ] 4.4 Add both commands to the command reference in `docs/`. Check: the docs drift guard passes.

## 5. Verification and close-out

- [ ] 5.1 `cabal build --flag dev` and `cabal test` pass. Check: test summary line in `README.md`.
- [ ] 5.2 Rebuild the repository of 1.2 at `function` granularity. Record node count, `calls` edge count, share of `calls` edges ending on a function or method node, build time, community count and modularity, against 1.2. Check: node count is equal.
- [ ] 5.3 Add the `CHANGELOG.md` entry. Check: entry present under Unreleased.
- [ ] 5.4 Comment on #297, #78 and #164 with the measured outcome. Check: each links the pull request.
