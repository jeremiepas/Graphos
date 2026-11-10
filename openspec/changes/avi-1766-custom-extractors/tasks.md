# Tasks - Custom extractors (AVI-1766 / issue #119)

## 1. Domain types and config schema

- [ ] 1.1 Add ExtractCommand to ExtractorMode with JSON mappings
- [ ] 1.2 Extend ExtractorConfig with command/runtime-grammar fields
- [ ] 1.3 Add ResolveKind/ResolveConfig and resolve map on GraphosConfig
- [ ] 1.4 Add ExtractionError and extractionErrors field to Extraction
- [ ] 1.5 Check: cabal build --flag dev green under -Wall -Werror

## 2. Config loader validation

- [ ] 2.1 Validate extractor entries; reject inconsistent/missing fields
- [ ] 2.2 Validate resolve entries; unknown kind fails load
- [ ] 2.3 Document new keys in graphos.yaml example and templates
- [ ] 2.4 Check: Hspec config cases; cabal test green

## 3. Command extractor infrastructure

- [ ] 3.1 Create Infrastructure/Extract/Command.hs runner (System.Process.proc, timeout, no shell)
- [ ] 3.2 Implement {file}/{files} substitution and batch mode
- [ ] 3.3 Decode stdout to Node/Edge, prefix ids, merge, record errors, stub fallback
- [ ] 3.4 Wire ignore/sensitive gating and allow_command_extractors
- [ ] 3.5 Check: unit tests + property test id uniqueness

## 4. Runtime tree-sitter grammar loader

- [ ] 4.1 Create Infrastructure/Extract/TreeSitter/RuntimeGrammar.hs with dynamic loader + ABI guard
- [ ] 4.2 Resolve entry symbol, return Ptr Language, warn+stub on failure
- [ ] 4.3 Plumb through grammarForFile/extractor while reusing convert/imports
- [ ] 4.4 Load optional definitions/imports/calls .scm; disable missing kind with warning
- [ ] 4.5 Check: ABI guard tests and optional fixture test

## 5. Dispatch, fingerprint, cache

- [ ] 5.1 Update partitionByExtractor and extract entrypoints for command route
- [ ] 5.2 Keep runtime grammars on tree-sitter path selecting loader
- [ ] 5.3 Extend extractionConfigFingerprint with command templates, grammar path/mtime, queries, resolve
- [ ] 5.4 Ensure cache save/load for new paths
- [ ] 5.5 Check: cache-soundness suite; no stale cache across config change

## 6. Declarative import resolution

- [ ] 6.1 Implement resolver kinds driven by ResolveConfig
- [ ] 6.2 Package/PSR4/ModulePath/PathPrefix resolvers
- [ ] 6.3 Config precedence over built-in with warning
- [ ] 6.4 Thread resolve through extract
- [ ] 6.5 Check: Hspec scenarios + override warning

## 7. Error recording and reporting

- [ ] 7.1 Append ExtractionError on failures without aborting
- [ ] 7.2 Summarise per-extractor errors in log/report
- [ ] 7.3 Check: integration fixture with one failing extractor completes and records error

## 8. Tests and docs

- [ ] 8.1 Add Hspec suites CommandExtractorSpec, RuntimeGrammarSpec, ResolveConfigSpec
- [ ] 8.2 Update README/workflow docs 05-path, 16-lsp-discovery and example
- [ ] 8.3 Validate openspec change
- [ ] 8.4 Check: cabal build/test green

## 9. Review gate

- [ ] 9.1 head-of-graphos architecture review
- [ ] 9.2 security-expert review (argv, ignore, ABI)
- [ ] 9.3 Record verification; promote done via run-id PATCH
