# Tasks - Custom extractors (AVI-1766 / issue #119)

## 1. Domain types and config schema

- [x] 1.1 Add ExtractCommand to ExtractorMode with JSON mappings
- [x] 1.2 Extend ExtractorConfig with command/runtime-grammar fields
- [x] 1.3 Add ResolveKind/ResolveConfig and resolve map on GraphosConfig
- [x] 1.4 Add ExtractionError and extractionErrors field to Extraction
- [x] 1.5 Check: cabal build --flag dev green under -Wall -Werror

## 2. Config loader validation

- [x] 2.1 Validate extractor entries; reject inconsistent/missing fields
- [x] 2.2 Validate resolve entries; unknown kind fails load
- [x] 2.3 Document new keys in graphos.yaml example and templates
- [x] 2.4 Check: Hspec config cases; cabal test green

## 3. Command extractor infrastructure

- [x] 3.1 Create Infrastructure/Extract/Command.hs runner (System.Process.proc, timeout, no shell)
- [x] 3.2 Implement {file}/{files} substitution and batch mode
- [x] 3.3 Decode stdout to Node/Edge, prefix ids, merge, record errors, stub fallback
- [x] 3.4 Wire ignore/sensitive gating and allow_command_extractors
- [x] 3.5 Check: unit tests + property test id uniqueness

## 4. Runtime tree-sitter grammar loader

- [x] 4.1 Create Infrastructure/Extract/TreeSitter/RuntimeGrammar.hs with dynamic loader + ABI guard
- [x] 4.2 Resolve entry symbol, return Ptr Language, warn+stub on failure
- [x] 4.3 Plumb through grammarForFile/extractor while reusing convert/imports
- [x] 4.4 Load optional definitions/imports/calls .scm; disable missing kind with warning
- [x] 4.5 Check: ABI guard tests and optional fixture test

## 5. Dispatch, fingerprint, cache

- [x] 5.1 Update partitionByExtractor and extract entrypoints for command route
- [x] 5.2 Keep runtime grammars on tree-sitter path selecting loader
- [x] 5.3 Extend extractionConfigFingerprint with command templates, grammar path/mtime, queries, resolve
- [x] 5.4 Ensure cache save/load for new paths
- [x] 5.5 Check: cache-soundness suite; no stale cache across config change

## 6. Declarative import resolution

- [x] 6.1 Implement resolver kinds driven by ResolveConfig
- [x] 6.2 Package/PSR4/ModulePath/PathPrefix resolvers
- [x] 6.3 Config precedence over built-in with warning
- [x] 6.4 Thread resolve through extract
- [x] 6.5 Check: Hspec scenarios + override warning

## 7. Error recording and reporting

- [x] 7.1 Append ExtractionError on failures without aborting
- [x] 7.2 Summarise per-extractor errors in log/report
- [x] 7.3 Check: integration fixture with one failing extractor completes and records error

## 8. Tests and docs

- [x] 8.1 Add Hspec suites CommandExtractorSpec, RuntimeGrammarSpec, ResolveConfigSpec
- [x] 8.2 Update README/workflow docs 05-path, 16-lsp-discovery and example
- [x] 8.3 Validate openspec change
- [x] 8.4 Check: cabal build/test green

## 9. Review gate

- [x] 9.1 head-of-graphos architecture review
- [x] 9.2 security-expert review (argv, ignore, ABI)
- [ ] 9.3 Record verification; promote done via run-id PATCH — verification recorded in REVIEW.md; Paperclip run-id PATCH unavailable in the delegate environment
