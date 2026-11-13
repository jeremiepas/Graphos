# Design — tree-sitter semantic enrichment

## 0. Foundation: child field names in `TSNodeInfo`

The Hackage `tree-sitter-0.9.0.3` binding's `Node` struct already carries
`nodeFieldName :: !CString`, and `ts_node_copy_child_nodes` (src/bridge.c,
`ts_tree_cursor_current_field_name`) fills it for every copied child. `Core.hs`
currently discards it.

Change: add `tsnFieldName :: Maybe String` to `TSNodeInfo`; populate in
`toNodeInfo` via `safeNodeType`-style peek (NULL ⇒ `Nothing`). One helper:

```haskell
childByField :: String -> TSNodeInfo -> Maybe TSNodeInfo
```

Everything else in this change is written against `childByField` with the
existing positional heuristics as fallback, so grammars without fields (or old
fixtures built from hand-rolled `TSNodeInfo` in tests) keep working.

Cost: 1 extra `peekCString` per child during the walk — negligible against
parse time; no extra FFI entry points needed.

## 1. Declaration names

`extractIdentifier` order becomes:

1. `childByField "name"` → its text (covers java, c-sharp, go, rust, php, ruby,
   python, typescript, c/cpp `function_declarator` via recursion into the
   `declarator` field).
2. Existing per-type fallbacks (`namespace_declaration` qualified_name,
   field/variable declarators).
3. Existing identifier-type scan — but when both a `type_identifier` and an
   `identifier` child are present, the `identifier` wins (kills the
   return-type-as-name failure on grammars/fixtures without fields).

`type_identifier` leaves `apiSurfaceTypes` (AVI-1440: references never emit
nodes). This also removes the empty-id node family (`""`, `"#L12"`, …)
observed on the Java fixture.

Collision honesty: fixing names removes the mass collisions, but the remap
`Map.fromList [(rnBaseId r, fid)]` still maps *all* duplicate base ids to the
last final id, silently rewiring `contains` for overloads. Fix alongside:
remap `contains` sources/targets per (baseId, line) pair, not per baseId —
each raw edge already knows its endpoint's line via the RawNode it came from.

## 2. `calls` edges — language matrix

**Implementation note (post-implementation revision):** this was built as one
flat, *larger* `callTypes` union list, not a per-grammar `callTypesFor`
dispatch table keyed by grammar name as drafted below. Tree-sitter node-type
strings are each grammar's own vocabulary; none of the verified names below
collides across grammars with a non-call meaning, so a single file's AST only
ever matches the entries belonging to its own grammar regardless. The flat
list is equally correct and avoids threading a new grammar-name parameter
through `tsNodesToExtraction` and every one of its ~15 call sites (production
and test). The per-grammar framing below is kept because it's still the
correct way to *read* the matrix — grouped by grammar — not because the code
is structured that way.

Replace the flat `callTypes` with a per-grammar table (keyed by the grammar
name `Grammar.hs` already assigns). Verification method: this session extracted
every Hackage-pinned grammar tarball (the exact versions in `graphos.cabal`)
and read `src/node-types.json` directly — the authoritative field/type source,
a stronger bar than AVI-1765's grep-for-type-name-presence. C# has no
`node-types.json` in this repo (only `parser.c`/`scanner.c` are vendored); its
row is verified only at the AVI-1765 bar (type-name string present in
`parser.c`) — field names are **unconfirmed** and must fall back to positional
scan until independently verified (task 2.1).

| grammar | call node types | callee field(s) | verified |
|---|---|---|---|
| typescript / tsx | `call_expression`, `new_expression` | `function` / `constructor` | node-types.json (tree-sitter-typescript 0.5.0.3) |
| javascript | `call_expression`, `new_expression` | `function` / `constructor` | same grammar as typescript |
| python | `call` | `function` | node-types.json (tree-sitter-python 0.9.0.4) |
| ruby | `method_call` (primary), `call` (bare no-arg/no-paren form) | `method` on both; `method_call`'s `method` field may itself be a `call` subtree (`obj.foo(x)`) — apply the rightmost-name rule recursively | node-types.json (tree-sitter-ruby 0.5.0.4). **Correction**: earlier draft said plain `call` is the invocation node; it is not — `call` is `receiver.name` *without* parens/args (ambiguous with attribute access), `method_call` is the real invocation and wraps `call` for the dotted form. |
| go | `call_expression` | `function` | node-types.json (tree-sitter-go 0.5.0.3) |
| rust | `call_expression`; `macro_invocation` deferred (field `macro` has no further structure to pick a callee name from without its own lookup — skip in v1) | `function` | node-types.json (tree-sitter-rust 0.1.0.2) |
| c / cpp | `call_expression` | `function` | not re-extracted this pass; already used unchanged from AVI-1447 |
| java (+ .kt via java grammar) | `method_invocation`, `object_creation_expression` | `method_invocation.name`; `object_creation_expression.type` | node-types.json (tree-sitter-java 0.7.0.3) |
| c-sharp (vendored, no node-types.json) | `invocation_expression`, `object_creation_expression` (type-name presence only, grep on `parser.c`) | **unconfirmed** — positional/type-scan fallback only until task 2.1 independently verifies field ids from the symbol/field tables in `parser.c` | type names grepped (1 hit each); fields NOT verified |
| php (0.5.0.2, pinned) | `function_call_expression`, `member_call_expression`, `scoped_call_expression`, `object_creation_expression` | `function_call_expression.function`; `member_call_expression.name` (receiver in `object`); `scoped_call_expression.name` (scope in `scope`); `object_creation_expression` has **no fields at all** — callee name falls back to positional scan of its first `name`/`qualified_name`/`class_constant_access_expression` child | node-types.json (tree-sitter-php 0.5.0.2) |
| perl (vendored, no node-types.json) | not verified this pass — grep `parser.c` for a call-like production before implementing (task 2.1) | verify | unverified |
| haskell | — (function application; out of scope) | — | n/a |
| json / yaml / sql / nix / markdown | — | — | n/a |

Callee name extraction: take the callee field's subtree; if it is a member /
qualified expression, take the *rightmost* name-like leaf (`foo.bar(x)` ⇒
`bar`, `new a.b.C()` ⇒ `C`). The current "first identifier in preorder"
(which returns the receiver `foo`) remains only as last-resort fallback for
grammars/fields not covered above (C#, Perl, PHP's `object_creation_expression`).

Resolution context: `collectCallsCtx` currently indexes only root children.
Extend it to walk the whole tree collecting every emitted definition name →
base id (methods inside classes, functions inside namespaces/impls/mods).
Lookup order stays: imports first, then local definitions; unresolvable ⇒
drop edge (unchanged). Receiver-qualified calls resolve by method name alone
(best-effort, confidence 0.9 as today); `this.helper()` / `self.helper()` /
bare `helper()` therefore all hit the member index.

Nested-call suppression from AVI-1447 is kept as-is (only the outermost call
inside a definition emits).

## 3. Annotations / decorators / attributes

Per-grammar annotation carriers, verified the same way as §2 (direct
`node-types.json` read of the pinned tarball, except C#):

| grammar | nodes | name/args source | where |
|---|---|---|---|
| java | `marker_annotation` (field `name`), `annotation` (fields `name`+`arguments`) | fields, directly | confirmed: `modifiers` node's `children` list explicitly allows `annotation`/`marker_annotation` |
| python | `decorator` | **no named fields at all** (only `arguments` exists as a field, and only on the call form) — name comes from a positional scan of the decorator's first identifier/call subtree | children of `decorated_definition` wrapper (`definition` field holds the decorated declaration; the decorator itself is a preceding sibling) |
| typescript | `decorator` | **no named fields** — same positional-scan requirement as Python | `class_declaration` exposes a `decorator` field (plural use via repetition); method/function decorators are preceding siblings, not a field |
| rust | `attribute_item` | **no named fields** — content is an internal `meta_item`/token tree; name/args need a text-level parse (split on first `(`) | already whitelisted as a node type in `apiSurfaceTypes`; attaches as a preceding sibling of the item it decorates |
| c-sharp | `attribute_list` (type-name presence only; fields unconfirmed, see §2) | positional scan, pending task 2.1 | leading children of the declaration |
| php (0.5.0.2, pinned) | **none** — grepped the full pinned grammar's `node-types.json` for any type containing `attr`: zero matches, and zero occurrences of `"attribute"` in `parser.c`. This grammar version predates PHP 8 attribute (`#[...]`) grammar support entirely. | n/a | n/a |

**Correction from the original draft**: PHP annotation support is **removed
from this change's scope** — not merely "unverified", but confirmed absent in
the pinned grammar. A PHP 8 `#[Route("/x")]` attribute will not parse into any
usable node with `tree-sitter-php` 0.5.0.2; closing this gap requires bumping
the pinned PHP grammar version first (separate change — check whether a newer
Hackage `tree-sitter-php` release vendors PHP 8 attribute grammar before
proposing the bump). PHP docblock annotations (`/** @Route(...) */`, common in
older Symfony/Fineract-adjacent PHP) are comment text, not an AST node, and are
also out of scope here (would need regex-over-comment-node extraction, a
different mechanism from everything else in this stream).

Emission model (decision): **metadata-first, no node explosion.**

- On the annotated definition node: `nodeExtra.annotations = [{name, args}]`
  (args = normalized source text of the argument list, budget 120 chars each,
  max 16 annotations per node).
- One `references` edge (confidence 1.0) from the definition to the
  annotation's *imported* node when the annotation name resolves through the
  file's imports (e.g. `javax.ws.rs.GET` External node) — this is what makes
  "every @Scheduled in the repo" a one-hop query. No edge when unresolved.
- Distillation of well-known annotations into flat queryable keys on
  `nodeExtra`: `route` (@Path/@RequestMapping/@GetMapping…), `httpVerb`
  (@GET/@POST/…/@GetMapping…), `cron`/`fixedDelay` (@Scheduled), `topics`
  (@KafkaListener/@RabbitListener), `configKeys` (@Value `${…}` keys). The
  distillation table is data, not code paths — one list of
  (annotation name, extra key, arg-extraction rule).

Signature change: for definition nodes with a body, `nodeSignature` becomes
the *header* — text from node start to the body's start (body child located
via field `body`, fallback: first `{`/`:`-block child) — normalized and
truncated as today. Annotations thus always survive the 200-char budget and
bodies stop leaking into signatures. Module/import signature rules unchanged.

**Ruby exception, verified**: `method` has no `body` field — its field list is
only `name`+`parameters`; the statements are unnamed `children`. The header
cut for Ruby methods must use the fallback path (first non-field block of
statements) from the start; there is no field-based shortcut for this one
grammar.

## 4. Notable strings

Context-gated only — a bare string literal never emits anything. Gates:

- **Env**: argument of an env-read call, per-grammar table —
  `System.getenv`, `os.environ[...]`/`os.getenv`, `process.env.X` (member
  access, not call), `std::env::var`, `ENV[...]`, `getenv`, `$_ENV[...]`.
  Emits node `env:<NAME>` (kind `Env`).
- **Config key**: `${…}` placeholder inside any string in an annotation arg or
  env-adjacent call; dotted keys in `@Value` / `@ConfigurationProperties`.
  Emits `config:<key>` (kind `Config`).
- **Topic/queue**: first string arg of listener/producer annotations already
  distilled in stream 3. Emits `topic:<name>` (kind `Topic`).

Each emits the canonical node (id is corpus-global, no dirHash — these are
rendezvous nodes, same identity from every file) plus a `References` edge from
the enclosing definition (confidence 0.9). Caps: ≤ 32 notable strings per
file, name length 1–80, after dedupe; over-cap ⇒ drop + debug log. Works at
every granularity: the scan runs on the raw AST (like `callEdgePairs`), not on
emitted nodes, so opaque function bodies still yield edges.

SQL-table-name mining from string fragments is explicitly deferred (high
false-positive risk; revisit once Env/Config prove out on Fineract).

## 4a. Dependency on AVI-1447 (found during verification, not previously noted)

`openspec/changes/AVI-1447-calls-edge-emission/` is still open — not archived
— even though its code (`Calls` relation, `mkCallEdge`, the TS-only `callTypes`
list) already shipped to `main` (confirmed in `Convert.hs`). Its ADDED
requirement "Tree-Sitter emits `calls` edges for call/new expressions" has
therefore never been merged into `openspec/specs/extraction/spec.md` — the
canonical spec's only `calls`-adjacent text today is in a still-pending change
folder, not the archive.

This change's `specs/extraction/spec.md` delta is written as **MODIFIED**
against that same requirement name, which only resolves correctly if AVI-1447
is archived first (so the base spec actually contains the requirement being
modified). Until then the delta is really an ADDED requirement wearing a
MODIFIED label. Action: archive AVI-1447 before or as part of landing this
change's stream 2; if archived first, no further action needed — the names
match exactly on purpose.

## 5. Verification strategy

- Fixture corpus `tests/fixtures/semantic-enrichment/<lang>/` — one small
  annotated service per grammar (route + env read + intra-class call +
  cross-file call), mirroring the Java file used for diagnosis.
- Per-stream unit specs in `tests/Graphos/Infrastructure/Extract/TreeSitter/`.
- End-to-end rerun on Fineract (the originating corpus): record in the change
  README — methods labeled by name (0 return-type labels on a sample), calls
  edge count > 0, count of nodes with `route`/`httpVerb`, count of `env:`/
  `config:` nodes. Compare graph size before/after (node count must not grow
  by more than the annotation/config nodes added).
- `cabal test` targeted at the tree-sitter spec modules (full suite currently
  hangs on BoundedDeltaCommunitySpec's random-graph property — known issue,
  skip it).
