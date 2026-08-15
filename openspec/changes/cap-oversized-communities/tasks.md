## 1. Domain / Community

- [ ] 1.1 Add `maxCommunityFraction` to clustering config with a validating constructor
- [ ] 1.2 Implement pure `capCommunitySizes` post-pass (recursive re-cluster of oversized communities)
- [ ] 1.3 Implement last-resort partition fallback with bounded recursion (guarantees termination)
- [ ] 1.4 QuickCheck properties: no output community exceeds cap; termination; normal input unchanged

## 2. Domain / Analysis

- [ ] 2.1 Read node generated/vendored flag; exclude such nodes from god-node computation
- [ ] 2.2 Exclude flagged nodes from bridge-node computation
- [ ] 2.3 Tests: centrality excludes flagged nodes

## 3. Config & CLI

- [ ] 3.1 Add `--max-community-fraction` and `--no-community-cap` flags
- [ ] 3.2 Map config `clustering.maxCommunityFraction`

## 4. UseCase integration

- [ ] 4.1 Wire `capCommunitySizes` into the Cluster stage after Leiden
- [ ] 4.2 Emit WARNING for each oversized community before splitting

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test` green including splitter properties
- [ ] 5.3 Smoke run on a blob-containing graph confirms no community exceeds the cap and god-nodes are real code
