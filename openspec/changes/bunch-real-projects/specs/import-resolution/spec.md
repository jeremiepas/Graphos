## ADDED Requirements

### Requirement: Corpus-aware import target resolution

After extraction and before graph build, the system SHALL resolve every
`imports` target that per-file resolution (`resolveImport`) left as an
extension-less path, a directory, or an `external:` specifier, against the set
of extracted file paths of the corpus. It SHALL apply, in order:

1. **Extension-less and directory targets.** `p` resolves to `p.<ext>` for the
   extensions of the importing file's language family (TS/JS:
   `.ts .tsx .js .jsx .mjs .cjs .mts .cts .vue .svelte`), then to
   `p/index.<ext>`.
2. **Root aliases.** `@/x`, `~/x`, `#/x` and `$lib/x` resolve by stripping the
   alias root, then by a unique path-suffix match of `x`. An explicit code
   extension in `x` is ignored for matching.
3. **tsconfig / jsconfig `paths`.** When a `tsconfig.json` or `jsconfig.json`
   with `compilerOptions.paths` is in the corpus, a specifier matching one of
   its patterns resolves through the mapped location. Otherwise a specifier
   containing `/` resolves by a unique path-suffix match.
4. **JVM fully-qualified names.** For `.java`, `.kt` and `.scala` importers,
   `a.b.C` resolves by a unique suffix match of `a/b/C`. A trailing member
   name or `*` is dropped first.

A target SHALL resolve only when exactly one corpus file matches. A target with
zero or several matches SHALL stay unchanged. Scoped package specifiers
(`@scope/name`), `node:` builtins and bare package names that match no corpus
file SHALL stay `external:`. A resolved edge SHALL keep its `EdgeId`. An
`External` node left with no incoming edge SHALL be dropped. Resolution SHALL
read only paths and configuration files of the corpus, and SHALL execute
nothing. (PRD §3.2, §3.3)

#### Scenario: extension-less relative import
- **WHEN** `src/app/a.ts` imports `./b` and `src/app/b.ts` exists
- **THEN** the `imports` edge targets the module node of `src/app/b.ts`, not an `External` node

#### Scenario: Next.js root alias
- **WHEN** `app/page.tsx` imports `@/lib/utils` and the corpus has exactly one `lib/utils.ts`
- **THEN** the edge targets `lib/utils.ts`; and a scoped package `@remix-run/react` stays `external:@remix-run/react`

#### Scenario: SvelteKit `$lib` with an extension
- **WHEN** `src/routes/+page.js` imports `$lib/api.js` and `src/lib/api.js` exists
- **THEN** the edge targets `src/lib/api.js`

#### Scenario: Angular tsconfig paths
- **WHEN** `tsconfig.json` maps `app/*` to `src/main/webapp/app/*` and a file imports `app/config/error.constants`
- **THEN** the edge targets `src/main/webapp/app/config/error.constants.ts`

#### Scenario: JVM class import
- **WHEN** `ArticleApi.java` imports `io.spring.core.article.Article` and the corpus has `src/main/java/io/spring/core/article/Article.java`
- **THEN** the edge targets that file; `org.springframework.web.bind.annotation.RestController` stays external

#### Scenario: ambiguous suffix stays external
- **WHEN** a specifier `utils` would match both `src/a/utils.ts` and `src/b/utils.ts`
- **THEN** the target is left unchanged

#### Scenario: measured corpus baseline
- **WHEN** the pipeline builds `taxonomy` (Next.js, `@/` aliases) and `jhipster-sample-app` (tsconfig `app/*`) from `dataset-discovery`
- **THEN** taxonomy has at least 100 files joined by file-to-file `imports` edges (0 before this change) and jhipster gains at least 200 internal `imports` edges
