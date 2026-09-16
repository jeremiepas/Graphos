## 1. Atomic write primitive

- [x] 1.1 Add `writeFileAtomic` helper (temp file in target dir, fsync, rename)
- [x] 1.2 fsync the directory entry after rename
- [x] 1.3 Tests: interrupted write leaves prior file intact; successful write replaces content

## 2. Apply to exporters

- [x] 2.1 Route `graph.json` writes through the atomic primitive
- [x] 2.2 Route checkpoint, `GRAPH_REPORT.md`, and other artifacts through it
- [x] 2.3 Ensure temp files share the target filesystem

## 3. Staged rebuild

- [x] 3.1 Write full-rebuild outputs into a staging directory
- [x] 3.2 Swap staging into `graphos-out/` via rename only on success
- [x] 3.3 Clean up staging on failure; leave existing output untouched
- [x] 3.4 Test: failed rebuild preserves existing output

## 4. Startup validation

- [x] 4.1 Validate existing `graph.json` (parse + minimal shape) on load
- [x] 4.2 Emit a clear error with path and recovery hint on corruption
- [x] 4.3 Tests: corrupt graph reported; valid graph loads clean

## 5. Verification

- [x] 5.1 `cabal build --flag dev` with `-Werror`
- [x] 5.2 `cabal test` green
- [x] 5.3 Interrupt-during-write smoke test confirms output directory stays valid
