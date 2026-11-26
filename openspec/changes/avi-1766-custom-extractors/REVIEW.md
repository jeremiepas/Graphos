# Review gate — avi-1766-custom-extractors

Scope: tasks 9.1 (architecture) and 9.2 (security) of
`openspec/changes/avi-1766-custom-extractors/tasks.md`, against the change
specs, `design.md`, and the implementation on
`feature/AVI-1766-custom-extractors` (PR #287).

## 9.1 Architecture review

Verdict: **changes requested and applied; architecture accepted after them.**

The layering holds: domain schema (`Domain/Config`, `Domain/Types`), pure
resolvers (`Infrastructure/Extract/Resolve`), command runner
(`Infrastructure/Extract/Command`), runtime grammar loader
(`Infrastructure/Extract/TreeSitter/RuntimeGrammar`), orchestration and
fingerprinting (`UseCase/Extract/Core`), and wiring remain separated. The
command and tree-sitter paths reuse the existing merge/stub/error pipeline
instead of introducing a second graph builder. Per-file failure isolation
(`isolateFileExtraction`) keeps one failing extractor from aborting a build.

Findings fixed in this review:

1. **Content-only cache could serve the wrong path.** The extraction cache key
   is `sha256(content <> fingerprint)` and deliberately ignores the path
   (`Infrastructure/FileSystem/Cache`). Per-file products are *not*
   path-independent: node ids and `source_file` are path-derived, and command
   output embeds both. Two byte-identical files therefore shared a slot and
   could receive records attributed to the other path. The pipeline now mixes
   the source path into the fingerprint it hands to the cache
   (`fileCacheFingerprint` in `UseCase/Extract/Core`), applied to every
   tree-sitter, command and stub load/save. The cache module itself stays
   content-addressed.
2. **`source_of_truth` was stamped against the process CWD.** `loadModuleIndexes`
   reads it against the project input root, but `extractionConfigFingerprint`
   stamped the raw relative path. With a CWD different from the input root the
   fingerprint tracked a different file (usually `missing`), so editing the
   index could leave a stale cache entry. The stamp now uses the same
   project-root base (`absAgainst (cfgInputPath config)`).
3. **Resolver path-base documentation was wrong.** `resolve.roots` and
   `source_of_truth` are read against the project/input root, not the config
   file's directory. Corrected in `Infrastructure/Config.hs` (generated
   template), `Domain/Config/Resolve.hs` (field haddock) and `graphos.yaml`.

Known limitation (recorded, not fixed here): optional `definitions.scm` /
`imports.scm` / `calls.scm` queries are discovered, compiled and executed, but
their captures are currently advisory (`void` in
`Infrastructure/Wiring.hs`); graph records still come from the shared
tree-sitter traversal. Mapping captures to additional nodes/edges for
languages the shared traversal does not model is a follow-up. Cache
invalidation for query edits is already correct (size+mtime stamps).

## 9.2 Security review

Verdict: **accepted after one guard added.**

- **argv / no shell.** `Command.invoke` uses `System.Process.proc` with an
  explicit argv; placeholders expand per path into separate argv elements, so
  no user or corpus string is interpreted by a shell. Verified in
  `Infrastructure/Extract/Command.hs` and its suite.
- **Gating before spawn.** `gateCommandFile` rejects disabled command
  extractors, sensitive paths (`isSensitive`) and ignore-pattern matches before
  `invoke`; batch mode filters the file list the same way, so a sensitive path
  is never passed to the process.
- **Resource bounds.** Every invocation has a wall-clock timeout; stdout is
  capped at 64 MiB and stderr at 64 KiB; truncation and timeout terminate the
  child and become per-file errors. Malformed JSON is a per-file warning, never
  an exception.
- **ABI guard.** `loadRuntimeGrammar` opens with `RTLD_NOW|RTLD_LOCAL`, resolves
  only the derived `tree_sitter_<lang>` entry symbol, rejects null languages
  and ABIs outside 9–11, and unloads on every failure path. All failures fall
  back to stub extraction.
- **Sensitive runtime inputs.** Added the missing sensitivity check: a
  grammar path matching the sensitive-file policy is refused before `dlopen`
  (and therefore before any code from it runs), matching the spec's
  "Security guard for runtime grammars".
- **Minor, not changed:** an output stream of exactly the 64 MiB cap is
  reported as truncated (off-by-one probe). The failure mode is a recorded
  per-file error, not unsafe behavior; left as a low-priority cleanup.

## Verification

Executed in the repository root with the zlib library path required by the
linker:

```
LIBRARY_PATH=/nix/store/2kdz3m7ic8w226pcvkz1dlg169v91p6a-zlib-1.3.2/lib \
  cabal build all
LIBRARY_PATH=/nix/store/2kdz3m7ic8w226pcvkz1dlg169v91p6a-zlib-1.3.2/lib \
  cabal test all --test-show-details=direct
```

Result: build green; `1962 examples, 0 failures, 3 pending`; suite
`graphos-test: PASS`.

Regression coverage added: per-file fingerprints differ across paths and a
content-identical file at another path is a cache miss
(`UseCase/Extract/CacheSoundnessSpec`), the source-of-truth stamp follows the
input root, and a sensitive grammar path is refused
(`Infrastructure/Extract/TreeSitter/RuntimeGrammarSpec`).

## 9.3 Promotion

The verification above is recorded here and on PR #287. The Paperclip
run-id status PATCH could not be issued from this environment: no
`PAPERCLIP_API_URL`, run id, task id, API key or `scripts/paperclip-*` helper is
present. Promotion to `done` requires the orchestrator to replay the PATCH with
its run id.
