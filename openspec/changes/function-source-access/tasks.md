## 1. Baseline and check criteria (before any code)

- [ ] 1.1 Add a Haskell fixture under `test-corpus/` covering the five declaration scenarios of the spec, with the expected `(name, start, end, signature)` rows listed at the top of the spec that uses it. Check: the list has one row per declaration of the fixture.
- [ ] 1.2 Record the baseline in the change's `README.md`: Haskell function nodes of this repository with and without the three fields, and the node and edge counts. Check: the numbers match M1 of the design, or state the difference.
- [ ] 1.3 Define the sanity check for declaration ranges (start ≤ end, no overlap inside a file, first line contains the name) as a spec helper. Check: it runs on the fixture and is red on `main`.
- [ ] 1.4 Write the failing specs: declaration positions and signatures, id stability, range selection and context clipping, each reporting outcome, JSON shape. Check: `cabal test` lists them, red or pending.

## 2. Haskell line range and signature

- [ ] 2.1 Make `parseHaskellDecls` return first line, last line and signature for each declaration. Check: position and signature specs pass.
- [ ] 2.2 Copy the values into the node builders of `UseCase/Extract/Haskell.hs`. Check: the id-stability spec passes; the fixture's nodes carry the three fields.
- [ ] 2.3 Change the extraction fingerprint so cached Haskell extractions from before the change are not reused. Check: a spec shows the fingerprint differs.

## 3. Source command

- [ ] 3.1 Add the pure range selection, numbering and clipping, and the result type with its outcomes (ok, stale, no range, missing file, short file). Check: pure specs pass.
- [ ] 3.2 Add the file-range reader as a port function with its Infrastructure implementation. Check: the temporary-directory spec passes.
- [ ] 3.3 Add `graphos source` to `CLI/Parser.hs` and `app/Main.hs` with `--graph`, `--root`, `--context`, `--json`, using `resolveNodeArg` and the shared not-found and ambiguous renderers. Check: `--help` works; the five command scenarios pass.
- [ ] 3.4 Implement the `body_hash` comparison when the node has one. Check: the stale scenario passes; a node without the field prints no warning.
- [ ] 3.5 Add the command to the command reference in `docs/`. Check: the docs drift guard passes.

## 4. Verification and close-out

- [ ] 4.1 `cabal build --flag dev` and `cabal test` pass. Check: test summary line in `README.md`.
- [ ] 4.2 Rebuild the graph of this repository. Record Haskell function nodes with the three fields, the share that passes the sanity check of 1.3, and node-id and edge equality against 1.2. Check: ids and edges are equal.
- [ ] 4.3 Run `graphos source lspInitialize --root .` on that graph. Check: the output is the function's lines in `Infrastructure/LSP/Protocol.hs`.
- [ ] 4.4 Add the `CHANGELOG.md` entry and comment on #298 with the measured outcome. Check: entry present under Unreleased; the issue links the pull request.
