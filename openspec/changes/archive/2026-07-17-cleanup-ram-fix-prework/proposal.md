## Why

The `fix-runtime-ram-crash` change is fully designed but blocked on avoidable risk: its tasks target a 631-line dead module (`Infrastructure.Observability`), a Domain→Infrastructure import violation couples Domain types to the module task 6 will rewrite, and the compact-Node task (task 7) must wade through 5 LEGACY fields spread across 23 files. Removing this debris first shrinks the RAM fix's surface area, eliminates double-work, and yields an immediate memory win (~40-120 bytes/node) for free.

## What Changes

- **Delete dead module** `Graphos.Infrastructure.Observability` (631 lines, compiled but imported by nothing — `Observability.SDK` is the live implementation). Remove from `graphos.cabal`.
- **Fix architecture violation**: `Domain.Types.Pipeline` imports `Infrastructure.Observability.SDK` for `OtelConfig`/`defaultOtelConfig`. Move these types into the Domain layer and invert the import so Infrastructure depends on Domain, not the reverse.
- **Migrate live legacy field usages**:
  - `nodeCapturedAt` (conversation timestamps, set in `Domain.Context`) → `nodeExtra`
  - `nodeSourceLocation` (read by `UseCase.FormatContext` and Neo4j/Memgraph exporters) → derive from `nodeLineStart`/`nodeLineEnd`
- **Remove 5 LEGACY Node fields** (`nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, `nodeContributor`) from `Domain.Types.Node`, all construction sites (~23 files), and JSON instances. **BREAKING** (minor): JSON output drops legacy keys (`source_location`, `source_url`, `captured_at`, `author`, `contributor`) for the small subset of nodes that carried them; conversation timestamps move under `extra`.

## Capabilities

### New Capabilities
- `node-schema`: Canonical Node field set — the 12 non-legacy fields are the only Node schema; legacy data migrates to `nodeExtra` or derived fields
- `architecture-purity`: Domain layer must not import Infrastructure modules; observability configuration types live in Domain
- `observability-consolidation`: Exactly one observability implementation (`Observability.SDK`); no dead parallel copies

### Modified Capabilities

<!-- No existing specs (image-analysis, office-extraction, vision-config) have requirement changes. -->

## Impact

- **Code**: `graphos.cabal` (module removal), `Domain/Types/Node.hs`, `Domain/Types/Pipeline.hs`, `Domain/Context.hs`, `UseCase/FormatContext.hs`, `Infrastructure/Export/Neo4j.hs`, `Infrastructure/Export/Memgraph.hs`, plus ~17 files with mechanical `= Nothing` construction-site removals
- **API**: Node JSON loses 5 legacy optional keys; consumers reading `captured_at` must read `extra.capturedAt`. `graph.json` node/edge counts unchanged.
- **Dependencies**: None added, none removed
- **Downstream**: `fix-runtime-ram-crash` task 6 rescopes to SDK.hs only; task 7 (compact Node) starts from a 12-field Node instead of 17
- **Cache**: Cached extractions containing legacy keys still parse (unknown keys ignored); legacy values not carried forward

## PDCA Cycle

- **Plan**: Remove ~700 lines of dead/legacy code with zero behavior change (except documented legacy JSON keys), verified by full test suite and structural graph.json comparison. Hypothesis: `fix-runtime-ram-crash` tasks 6 and 7 shrink measurably in scope, and per-node memory drops without any new representation work.
- **Do**: Delete dead module, relocate `OtelConfig` to Domain, migrate `nodeCapturedAt`/`nodeSourceLocation` usages, remove the 5 legacy fields everywhere.
- **Check**: `cabal build` with `-Wall` clean, `cabal test` passes, `graphos .` on a test codebase produces identical node/edge/community counts, no `import Graphos.Infrastructure` remains under `src/Graphos/Domain/`, grep confirms zero references to removed fields.
- **Act**: Update `fix-runtime-ram-crash` design/tasks to reflect the reduced scope (task 6 → SDK.hs only; task 7 → 12-field baseline). Add "Domain must not import Infrastructure" to code-quality standards if not already explicit.
