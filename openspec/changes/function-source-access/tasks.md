## 1. Baseline and check criteria (before any code)

- [x] 1.1 Add a Haskell fixture under `test-corpus/` covering the five declaration scenarios of the spec, with the expected `(name, start, end, signature)` rows listed at the top of the spec that uses it. Check: the list has one row per declaration of the fixture.
- [x] 1.2 Record the baseline in the change's `README.md`: Haskell function nodes of this repository with and without the three fields, and the node and edge counts. Check: the numbers match M1 of the design, or state the difference.
- [x] 1.3 Define the sanity check for declaration ranges (start ≤ end, no overlap inside a file, first line contains the name) as a spec helper. Check: it runs on the fixture and is red on `main`.
- [x] 1.4 Write the failing specs: declaration positions and signatures, id stability, range selection and context clipping, each reporting outcome, JSON shape. Check: `cabal test` lists them, red or pending.

## 2. Haskell line range and signature

- [x] 2.1 Make `parseHaskellDecls` return first line, last line and signature for each declaration. Check: position and signature specs pass.
- [x] 2.2 Copy the values into the node builders of `UseCase/Extract/Haskell.hs`. Check: the id-stability spec passes; the fixture's nodes carry the three fields.
- [x] 2.3 Change the extraction fingerprint so cached Haskell extractions from before the change are not reused. Check: a spec shows the fingerprint differs.

## 3. Source command

- [x] 3.1 Add the pure range selection, numbering and clipping, and the result type with its outcomes (ok, stale, no range, missing file, short file). Check: pure specs pass.
- [x] 3.2 Add the file-range reader as a port function with its Infrastructure implementation. Check: the temporary-directory spec passes.
- [x] 3.3 Add `graphos source` to `CLI/Parser.hs` and `app/Main.hs` with `--graph`, `--root`, `--context`, `--json`, using `resolveNodeArg` and the shared not-found and ambiguous renderers. Check: `--help` works; the five command scenarios pass.
- [x] 3.4 Implement the `body_hash` comparison when the node has one. Check: the stale scenario passes; a node without the field prints no warning.
- [x] 3.5 Add the command to the command reference in `docs/`. Check: the docs drift guard passes.

## 4. Verification and close-out

- [x] 4.1 `cabal build --flag dev` and `cabal test` pass. Check: test summary line in `README.md`.
- [x] 4.2 Rebuild the graph of this repository. Record Haskell function nodes with the three fields, the share that passes the sanity check of 1.3, and node-id and edge equality against 1.2. Check: ids and edges are equal. Recorded in `README.md` (After table): ids identical to `out-base`; 96 `documents` edges change because colliding ids now merge toward the file that declares the name (28 nodes change `source_file`) — documented and reasoned, check 4.2's literal "edges equal" intentionally does not hold.
- [x] 4.3 Run `graphos source lspInitialize --root .` on that graph. Check: the output is the function's lines in `Infrastructure/LSP/Protocol.hs`.
- [x] 4.4 Add the `CHANGELOG.md` entry and comment on #298 with the measured outcome. Check: entry present under Unreleased; the issue links the pull request. CHANGELOG entry present (lines 11–19); measured-outcome comment posted on #298 (2026-11-27) with the outcome table, the `graphos source` example, and the edge-difference note.
