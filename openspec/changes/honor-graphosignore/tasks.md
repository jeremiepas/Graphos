## 1. Investigation

- [ ] 1.1 Add a failing test reproducing `.graphosignore` at scan root not excluding a nested file
- [ ] 1.2 Trace current base-dir resolution and path normalization in `Infrastructure.FileSystem.Ignore`

## 2. Fix matching

- [ ] 2.1 Resolve `.graphosignore` from the scan-root `PATH` argument
- [ ] 2.2 Normalize candidate paths to scan-root-relative, `/`-separated form before matching
- [ ] 2.3 Ensure `**`, `*`, leading-`/` anchoring, and `#` comments behave per gitignore semantics
- [ ] 2.4 Expand Hspec tests: double-star, anchored, comment, negative cases, Windows separators

## 3. CLI

- [ ] 3.1 Add repeatable `--ignore GLOB` flag (optparse-applicative)
- [ ] 3.2 Merge CLI patterns with file patterns into one matcher

## 4. Observability

- [ ] 4.1 Log `Loaded N ignore patterns from <file>` at INFO
- [ ] 4.2 Log `Ignored M files` after Detect

## 5. Docs & Verification

- [ ] 5.1 Document ignore-file location and match semantics in README/CLI help
- [ ] 5.2 `cabal build --flag dev` and `cabal test` green
- [ ] 5.3 Smoke run: `graphos ./source --ignore '**/lib.rs'` yields zero nodes for matched files
