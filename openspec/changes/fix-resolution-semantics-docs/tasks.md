## 1. Empirical verification

- [ ] 1.1 Run a `--cluster-only` resolution sweep (0.5 / 1.0 / 2.0) on a known large graph
- [ ] 1.2 Record largest-community size per value; determine the true direction of effect

## 2. Documentation

- [ ] 2.1 Correct the `--resolution` help text to match measured behavior
- [ ] 2.2 Update README/workflow clustering section accordingly
- [ ] 2.3 Add the dense-subgraph caveat referencing the community-size cap

## 3. Runtime echo

- [ ] 3.1 Log effective resolution at INFO before clustering

## 4. Optional code correction

- [ ] 4.1 If the mapping is provably inverted relative to intent, fix the resGamma mapping
- [ ] 4.2 Keep default (1.0) behavior stable; note any behavior change
- [ ] 4.3 Tests: resolution mapping direction

## 5. Verification

- [ ] 5.1 `cabal build --flag dev` with `-Werror`
- [ ] 5.2 `cabal test` green
- [ ] 5.3 Confirm help text and measured sweep agree
