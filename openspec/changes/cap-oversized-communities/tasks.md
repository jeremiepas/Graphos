## 1. Domain / Community

- [x] 1.1 Add `maxCommunityFraction` to clustering config with a validating constructor
- [x] 1.2 Implement pure `capCommunitySizes` post-pass (recursive re-cluster of oversized communities)
- [x] 1.3 Implement last-resort partition fallback with bounded recursion (guarantees termination)
- [x] 1.4 QuickCheck properties: no output community exceeds cap; termination; normal input unchanged

## 2. Domain / Analysis

- [x] 2.1 Read node generated/vendored flag; exclude such nodes from god-node computation
- [x] 2.2 Exclude flagged nodes from bridge-node computation
- [x] 2.3 Tests: centrality excludes flagged nodes

## 3. Config & CLI

- [x] 3.1 Add `--max-community-fraction` and `--no-community-cap` flags
- [x] 3.2 Map config `clustering.maxCommunityFraction`

## 4. UseCase integration

- [x] 4.1 Wire `capCommunitySizes` into the Cluster stage after Leiden
- [x] 4.2 Emit WARNING for each oversized community before splitting

## 5. Verification

- [x] 5.1 `cabal build --flag dev` with `-Werror`
- [x] 5.2 `cabal test` green including splitter properties
- [x] 5.3 Smoke run on a blob-containing graph confirms no community exceeds the cap and god-nodes are real code
