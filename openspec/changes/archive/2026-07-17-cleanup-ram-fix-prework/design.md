## Context

A design review of `fix-runtime-ram-crash` (fully designed, 0/32 tasks done) surfaced three pieces of debris that inflate its risk and scope:

1. `Graphos.Infrastructure.Observability` (631 lines) is compiled but imported by nothing — `Observability.SDK` is the live copy. The RAM change's task 6 currently instructs bounding stores in *both* files.
2. `Domain.Types.Pipeline` imports `Infrastructure.Observability.SDK` for `OtelConfig`/`defaultOtelConfig` — a Domain→Infrastructure dependency violating the project's layering rule, and coupling Domain to the exact module task 6 rewrites.
3. `Node` carries 5 LEGACY fields referenced across 23 files. Task 7 (compact Node) must handle all 17 fields; removing legacy fields first gives it a 12-field baseline and an immediate ~40-120 bytes/node win.

Usage audit findings that constrain the design:
- `nodeCapturedAt` is genuinely used: `Domain.Context` sets it to the conversation timestamp for conversation-derived nodes.
- `nodeSourceLocation` is genuinely read: `UseCase.FormatContext` renders it as a location suffix; Neo4j/Memgraph exporters emit it as `source_location`.
- `nodeSourceUrl`, `nodeAuthor`, `nodeContributor` have no meaningful producers (only `= Nothing` construction sites and pass-through export).

Constraints:
- Domain layer must remain pure and Infrastructure-free.
- `cabal test` must pass unchanged except tests that assert legacy fields.
- Graph structure (node/edge/community counts) must be identical before/after.
- This change must land before `fix-runtime-ram-crash` implementation begins.

## Goals / Non-Goals

**Goals:**
- Remove all dead observability code; leave exactly one implementation (SDK.hs).
- Restore Domain layer purity (zero Infrastructure imports under `src/Graphos/Domain/`).
- Reduce `Node` to the 12 canonical fields with live data migrated, not lost.
- Shrink `fix-runtime-ram-crash` tasks 6 and 7 scope.

**Non-Goals:**
- Any memory-bounding work (spans/histograms/dtBuffer caps) — that is `fix-runtime-ram-crash` D5.
- Fixing the O(n²) `dtBuffer` append in SDK.hs — belongs with the RAM change's task 6 rewrite.
- Compact Node representation (bit-fields, ShortText) — that is D6 of the RAM change.
- `NodeId` newtype migration (existing TODO) — separate, wider change.
- Any behavior or output change beyond the documented legacy JSON keys.

## Decisions

### D1: Delete `Observability.hs` outright (no deprecation period)

**Decision**: Remove `src/Graphos/Infrastructure/Observability.hs` and its `graphos.cabal` entry in one commit.

**Alternatives considered**:
- A: Deprecate with a warning pragma, delete later — pointless; zero importers means zero migration burden
- B: Merge unique parts into SDK.hs — audit shows SDK.hs is a superset/duplicate; nothing unique to preserve
- C: **Delete now** — compiler proves safety (any hidden importer fails the build)

**Rationale**: `rg 'import Graphos.Infrastructure.Observability'` shows only `.SDK` imports (Main, Pipeline, Domain.Types.Pipeline). The build itself is the safety net.

**Layer**: Infrastructure (removal), `graphos.cabal`

### D2: Move `OtelConfig` to `Graphos.Domain.Config`

**Decision**: Relocate `OtelConfig(..)` and `defaultOtelConfig` from `Infrastructure.Observability.SDK` into `Graphos.Domain.Config` (which already holds pipeline-adjacent config types). SDK.hs re-imports from Domain. SDK.hs MAY re-export them for backward compatibility of existing importers.

**Alternatives considered**:
- A: New module `Domain.Types.Observability` — viable, but Domain.Config already exists and holds config types; avoid module proliferation
- B: Duplicate the type in Domain and convert at the boundary — duplication invites drift
- C: **Move to Domain.Config + re-export from SDK** — single definition, no import churn for SDK users

**Rationale**: `OtelConfig` is pure configuration data (no IO), so it belongs in Domain by the project's own rules. Re-exporting from SDK.hs keeps `app/Main.hs` and `UseCase.Pipeline` imports working, minimizing diff.

**Layer**: Domain.Config (type moves in), Infrastructure.Observability.SDK (imports + re-exports), Domain.Types.Pipeline (import flips to Domain.Config)

### D3: Migrate `nodeCapturedAt` into `nodeExtra`

**Decision**: In `Domain.Context` (conversation node construction), write the timestamp into `nodeExtra` as `{"capturedAt": <timestamp>}` (merging with any existing extra object) instead of `nodeCapturedAt`.

**Alternatives considered**:
- A: Drop timestamps entirely — loses real data used for conversation provenance
- B: New dedicated field `nodeTimestamp` — re-adds a Maybe field, defeating the purpose
- C: **Store in `nodeExtra`** — the field's documented purpose ("LEGACY: use nodeExtra"), extensible, no schema growth

**Rationale**: The legacy comment already prescribes this. `nodeExtra :: Maybe Value` exists precisely for sparse metadata. Consumers of conversation timestamps (if any external) read JSON — documented as a breaking key move (`captured_at` → `extra.capturedAt`).

**Layer**: Domain.Context (producer), tests (assertions)

### D4: Derive location display from line fields

**Decision**: `UseCase.FormatContext` renders the location suffix from `nodeLineStart`/`nodeLineEnd` (e.g., `:10` or `:10-25`). Neo4j/Memgraph exporters drop the `source_location`/`source_url` properties (line fields are already exported).

**Alternatives considered**:
- A: Keep computing a `sourceLocation` string at construction and store it — redundant with line fields, wastes a Text per node
- B: **Derive at render time** — one representation, formatted where displayed
- C: Keep the export properties fed from line fields — Cypher output already includes line numbers; duplicating them as a string property is noise

**Rationale**: `nodeSourceLocation` was a stringly-typed duplicate of `nodeLineStart`. Rendering is a display concern; deriving at the two read sites (FormatContext, exporters if desired) is trivial and allocation-free until needed.

**Layer**: UseCase.FormatContext, Infrastructure.Export.Neo4j, Infrastructure.Export.Memgraph

### D5: Remove fields in a single sweep, compiler-driven

**Decision**: Delete the 5 fields from the `Node` record, its `ToJSON`/`FromJSON` instances, and let GHC errors enumerate every construction/read site (~23 files). Fix mechanically: construction sites drop `= Nothing` lines; the few real reads were migrated in D3/D4 first.

**Alternatives considered**:
- A: Field-by-field removal over 5 PRs — 5× the build/test cycles for no added safety
- B: **Single sweep after D3/D4 land** — the compiler guarantees completeness; real usages already migrated
- C: Keep fields but stop serializing — memory win lost, schema still lies

**Rationale**: Haskell record removal is compiler-verified refactoring at its safest. Ordering matters: D3/D4 first (behavior-preserving migrations, testable in isolation), then the sweep is purely mechanical.

**Layer**: Domain.Types.Node + every construction site (UseCase.Extract*, Query, Conversation, SelectContext, Domain.Context, Domain.Analysis, Infrastructure.LSP.Extraction, TreeSitter.Convert, exporters, MCP, Main, tests)

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| Hidden dynamic use of `Observability.hs` (TH, plugin, cabal flag) | None found by grep; `cabal build && cabal test` after deletion is the gate |
| External JSON consumers read `captured_at`/`source_location` | Documented **BREAKING** in proposal; keys were LEGACY-marked; migration note: `extra.capturedAt`, line fields |
| Old cache files contain legacy keys | Aeson `FromJSON` ignores unknown keys by default — verify with a fixture test |
| `nodeExtra` merge in D3 clobbers existing extra data | Merge objects (`Object` union) rather than replace; unit test covers node with pre-existing extra |
| SDK re-export of `OtelConfig` masks the layering fix | Acceptable transitional shim; `Domain.Types.Pipeline` must import from Domain.Config directly (checked by grep gate) |
| 23-file sweep introduces typos in unrelated record fields | Compiler catches missing/extra fields; `cabal test` + graph.json structural diff catch semantic drift |

## Verification Strategy (Check)

1. **Build gate**: `cabal build -j` clean with `-Wall` (no new warnings).
2. **Layering gate**: `rg 'import Graphos.Infrastructure' src/Graphos/Domain/` returns zero matches.
3. **Death certificate**: `Observability.hs` absent; `rg 'Graphos.Infrastructure.Observability($| |\()' --glob '!**/SDK.hs'` finds only `.SDK` references.
4. **Field gate**: `rg 'nodeSourceLocation|nodeSourceUrl|nodeCapturedAt|nodeAuthor|nodeContributor' src app tests` returns zero matches.
5. **Test suite**: `cabal test` passes; new tests: conversation timestamp in `nodeExtra`, extra-merge behavior, legacy-key cache fixture parse, FormatContext line-derived location.
6. **Structural regression**: `graphos .` on a fixture codebase before/after — identical node/edge/community counts; node JSON diff shows only the removed legacy keys and `extra.capturedAt` moves.

## Iteration & Rollback (Act)

- **If deletion breaks the build** (hidden importer): the importer is the finding — decide whether it should use SDK.hs, then proceed. Do not resurrect the dead module.
- **If conversation features regress**: D3 is isolated to `Domain.Context` + FormatContext; revert those commits independently of the field sweep.
- **If external consumers complain about JSON keys**: add a `--legacy-json` compat flag as a follow-up change; do not re-add fields to the type.
- **On success**:
  - Update `fix-runtime-ram-crash` design.md/tasks.md: task 6 rescopes to SDK.hs only; task 7 baseline becomes the 12-field Node (both explicitly reference this change).
  - Add "Domain SHALL NOT import Infrastructure" and "prefer `nodeExtra` over new Maybe fields" to `.opencode/context/core/standards/code-quality.md`.

## Migration Plan

Order is dependency-driven; steps 1-2 and 3-4 are independent pairs and can proceed in parallel:

1. D1: delete dead module + cabal entry → build + test.
2. D2: move `OtelConfig` to Domain.Config, flip imports → build + test + layering grep.
3. D3: conversation timestamp → `nodeExtra` (field still present, now unused) → targeted tests.
4. D4: FormatContext/exporters derive location from line fields (field now unread) → targeted tests.
5. D5: remove the 5 fields, compiler-driven sweep → full gates (1-6 above).

Rollback: each step is one revertible commit; no data migrations (caches tolerate both shapes).
