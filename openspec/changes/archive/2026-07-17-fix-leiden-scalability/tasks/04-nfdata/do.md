# Do: Honest NFData for LeidenState

- Field-forcing `NFData LeidenState` instance: unboxed vectors seq'd (NF at WHNF), boxed neighbor vector and sigma-tot IntMap forced structurally via `rnf`.
- Added clustering deep-evaluation smoke spec to `CommunitySpec.hs`.
- Added `deepseq >= 1.4` to the test-suite `build-depends` in `graphos.cabal`.
