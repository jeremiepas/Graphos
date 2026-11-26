# Supported File Types

Extraction is **tree-sitter-first by default** — no language server to install,
works offline, and covers every language below out of the box. LSP is an opt-in
override per extension (see [Tree-sitter vs. LSP](../explanation/architecture.md)
and the LSP notes in the [README](../../README.md)).

## Code (tree-sitter by default)

| Tier | Extensions | What you get |
|------|-----------|------------|
| Full symbols + resolved `imports` + `calls` | `.go` `.rs` | Class/function/method nodes, repo-resolved import edges to real files, name-matched call edges |
| Full symbols + `calls`, imports unresolved | `.ts` `.tsx` `.js` `.jsx` | Symbol nodes and call edges; import edges exist but target a specifier-derived placeholder, not a repo-verified file |
| Full symbols + resolved `imports`, no `calls` | `.rb` `.java` `.kt` `.kts` | Repo-resolved import edges (Kotlin resolves against `src/main/kotlin`, then Java interop); the call-matching mechanism only recognizes the `call_expression`/`new_expression` node names that Go/Rust/TS/JS grammars use — Ruby's/Java's/Kotlin's call nodes are named differently, so no call edges |
| Full symbols + placeholder `imports`, no `calls` | `.php` `.pl` `.pm` `.psgi` `.cs` | Class/method/field nodes plus import edges read off the unquoted specifier (`use Foo\Bar;`, `using System.Text;`) — the target is a specifier-derived placeholder, not a repo-verified file |
| Full symbols, no `calls`, no resolved imports | `.py` `.pyw` | Class/method/field nodes only — same `call_expression` naming gap, and no dedicated import resolver |
| Full symbols, Haskell's own import resolver, no `calls` | `.hs` `.lhs` | Module-qualified imports resolved via Haskell-specific logic, not the generic tree-sitter import path |
| Structure/data only | `.json` `.yaml` `.yml` `.sql` `.md` `.rst` `.adoc` | File- or document-level nodes, no symbols, no edges beyond containment |
| C/C++ family | `.cpp` `.hpp` `.cc` `.cxx` `.hh` `.hxx` | Full symbols via the `cpp` grammar; `#include` becomes an `Import` node but isn't resolved to a verified file |
| Grammar missing → stub only | `.c` `.h` `.nix` | One empty node per file ([#158](https://github.com/jeremiepas/Graphos/issues/158)) |
| No default extractor → stub unless you configure LSP | `.mjs` `.cjs` `.scala` `.swift` `.lua` `.zig` `.ex` `.exs` `.m` `.mm` `.jl` `.vue` `.svelte` `.dart` `.ps1` | Empty stub node unless you set `mode: lsp` for the extension in `graphos.yaml` and install the matching server |

`calls` edges are syntactic, name-based matching against a fixed pair of AST node
names — not a semantic call graph, and currently only triggers for the four
languages above whose grammars happen to use those exact node names. A language
server's `callHierarchy` (opt-in LSP mode) is the only genuinely semantic
cross-file call graph Graphos can produce, for any language with a server.

## Adding a language without patching Graphos

Any extension can be bound in `graphos.yaml` instead of the compiled-in table:

```yaml
extractors:
  ".pl":
    mode: command
    command: ["my-perl-extractor", "{file}"]   # argv, no shell; {files} for batch
    batch: false
    timeout: 60
  ".php":
    mode: tree-sitter
    grammar_path: ~/.local/lib/tree-sitter-php.so   # ABI 9-11
    queries: ~/.config/graphos/queries/php/         # optional .scm dir

resolve:
  ".java": { kind: package, roots: ["src/main/java"] }
```

A command extractor prints graph.json-shaped `nodes` and `edges`; Graphos
validates every record, prefixes ids per file, records a per-file error on
failure, and stub-extracts that file without aborting the run. Runtime grammar
and command paths are fingerprinted, so a replaced `.so`, an edited query, or a
changed command invalidates the cache. Command extractors honour
ignore/sensitive filtering and the `allow_command_extractors` gate.

## Non-code

| Type | Extensions | What actually happens |
|------|-----------|------------|
| Docs | `.md` `.txt` `.rst` `.adoc` `.org` | Structural parsing — headers, `#tags`, `[[wikilinks]]`. Code↔doc links are deterministic (co-location, symbol mentions, path references) — **no LLM is involved**. |
| Office | `.docx` `.pptx` `.xlsx` `.doc` `.ppt` | Converted toward markdown and extracted structurally. `.doc`/`.ppt` support is minimal. Treat this path as best-effort, not a primary use case yet. |
| Papers | `.pdf` | Text extraction via `pdftotext` (poppler) plus regex-based structure heuristics (titles, numbered sections). Three granularity modes: `small` (titles only), `medium` (+ sections, default), `large` (+ paragraphs). No citation parsing. |
| Images | `.png` `.jpg` `.jpeg` `.webp` `.gif` | Real LLM vision analysis — **disabled by default**; set `vision.enabled: true` and point it at an OpenAI-compatible vision endpoint. |

There is no audio/video transcription today, despite what older copies of the
README claimed — don't configure for it.

## Related

- [Architecture](../explanation/architecture.md) — extractor adapters and the
  tree-sitter/LSP split.
- [Workflow 16 — agent scaffolding](../workflows/16-agent-scaffolding.md) —
  generating a starter configuration.
