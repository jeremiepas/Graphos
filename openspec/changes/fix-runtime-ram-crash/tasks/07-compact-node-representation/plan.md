<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P ...`, `- [ ] N.D ...`, `- [ ] N.C ...`, `- [ ] N.A ...`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
              A passed task reaches the same done state as in classic SDD.
  RETRY rule: if Act is NOT OK, the task does NOT pass - record the failed
              attempt under "### Attempt history (N)" (KEEP THE TRACE, never
              delete it), then start a NEW P -> D -> C -> A attempt for the same
              task. Repeat until an attempt passes.

  Everything else matches the official spec-driven workflow.
-->

# Task 7 — Compact Node representation — PLAN

**Task slug**: `07-compact-node-representation`
**Attempt**: 1
**Status**: pending

## Summary

Replace remaining `Maybe` fields in `Node` with a packed representation: add `Word64 nodePresentBits` for presence flags, use `Data.Text.Short.Text` for `nodeLabel`, `nodeSourceFile`, and `nodeSignature`. Keep JSON output identical via `ToJSON`/`FromJSON` instances.

## Detail

### Scope

This task modifies:
- `src/Graphos/Domain/Types/Node.hs` — add `Word64 nodePresentBits`, change `nodeLabel`, `nodeSourceFile`, `nodeSignature` from `Text` to `Data.Text.Short.Text`. Keep `nodeExtra :: Maybe Value` unchanged.
- `src/Graphos/Domain/Types.hs` — re-exports unchanged (same public API)
- Indirect: all pattern matches on `Node` fields throughout the codebase must be updated to use `Data.Text.Short` (e.g., `Text.unpack` or `toStrict` conversions)
- `graphos.cabal` — add `text-short` to build-depends

The `Node` record has 12 canonical fields (5 legacy fields removed by `cleanup-ram-fix-prework`), with 7 `Maybe` wrappers. The compact representation:
1. Replace `nodeLabel :: Text` → `nodeLabel :: ShortText`
2. Replace `nodeSourceFile :: Text` → `nodeSourceFile :: ShortText`
3. Replace `nodeSignature :: Text` → `nodeSignature :: ShortText`
4. Add `nodePresentBits :: Word64` — bit 0 = nodeLineStart present, bit 1 = nodeLineEnd present, bit 2 = nodeSignature present, bit 3 = nodeCommunityId present, bit 4 = nodeKind present, bit 5 = nodeDegree present, bit 6 = nodeIsBridge present, bit 7 = nodeExtra present
5. Keep `nodeExtra :: Maybe Value` — unchanged so `nodeExtraCapturedAt` and `setNodeExtraCapturedAt` helpers remain valid

`ToJSON` instance converts `ShortText` to `Text` for serialization. `FromJSON` instance converts `Text` to `ShortText` during deserialization. JSON output is structurally identical.

### Check Criteria

**Spec scenarios satisfied:**

| Scenario ID | Spec File | Description |
|---|---|---|
| `compact-nodes/scen:json-round-trip` | `specs/compact-nodes/spec.md` | `fromJSON (toJSON node) == node` for representative nodes |
| `compact-nodes/scen:memory-reduction` | `specs/compact-nodes/spec.md` | 100k nodes in `Map NodeId Node` occupies < 25MB (or < 20MB per task plan) |
| `compact-nodes/scen:nodeextra-nothing` | `specs/compact-nodes/spec.md` | Simple nodes with all optional fields Nothing have `nodeExtraRecord` = Nothing |
| `compact-nodes/scen:short-label-storage` | `specs/compact-nodes/spec.md` | Short labels stored as `ShortText` without heap allocation |
| `compact-nodes/scen:long-label-fallback` | `specs/compact-nodes/spec.md` | Labels > 100 chars still stored correctly |

**Specific tests/gates:**

1. **Unit test — JSON round-trip**: `fromJSON (toJSON node) == node` for representative nodes:
   - Simple node (no optional fields)
   - Node with line numbers
   - Node with signature
   - Node with community ID and kind
   - Node with all optional fields
   - Node with non-null `nodeExtra` (Value)
2. **Unit test — ShortText handling**: Verify `ShortText` is used internally for label/source/signature, and `toJSON` produces same JSON string as before.
3. **Memory profiling**: `+RTS -s` shows 100k nodes occupy < 20MB in `Map NodeId Node`.
4. **Integration test**: Full pipeline run produces identical `graph.json` (structural comparison — same node count, edge count, community structure).
5. **Build gate**: `cabal test` passes with exit code 0. All existing Node-related tests must pass (may need to update pattern matches).

**PASS conditions:**
- `fromJSON (toJSON node) == node` for all representative nodes
- 100k nodes in Map occupy < 20MB (per task plan; spec says < 25MB)
- All existing Hspec + QuickCheck tests pass
- Full pipeline produces structurally identical `graph.json` to pre-change baseline
- `cabal test` returns exit code 0

**FAIL boundaries:**
- If `fromJSON (toJSON node) /= node` for any node, the JSON round-trip is broken — must fix serialization
- If `text-short` cannot handle very long labels (> 100 chars), verify it still works (it handles arbitrary length; may lose inlining benefit but not correctness)
- If pattern matches break throughout the codebase, ensure all `Text` usages are converted to `ShortText` (use `toStrict` or `fromText`)

### Affected Modules

- `src/Graphos/Domain/Types/Node.hs` — Node type definition, ToJSON/FromJSON instances
- `src/Graphos/Domain/Types.hs` — re-exports (may need to re-export ShortText type)
- Indirect: all files that pattern match on `Node` fields — must update to use `ShortText` conversions
- `graphos.cabal` — add `text-short` dependency
- `tests/Graphos/Domain/Types/NodeSpec.hs` — add round-trip tests

### Prerequisites

- `text-short` package is available on Hackage (it is)
- Existing Node type has 12 fields (5 legacy fields already removed)
- `nodeExtraCapturedAt` and `setNodeExtraCapturedAt` helpers exist and must continue working

### Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| `text-short` is a new dependency | Build system change | Lightweight, widely used, no transitive bloat |
| Pattern match changes throughout codebase | Many files to update | Systematic search-and-replace with `ShortText` conversions |
| JSON serialization changes | Consumer breakage | ToJSON/FromJSON must produce identical JSON — verified by round-trip test |
| Very long labels lose inlining benefit | Memory reduction less for long labels | Text.Short handles arbitrary length; inlining only for short strings |

## Result

<!-- For CHECK/ACT: PASS or FAIL, and what happens next.
     If ACT is NOT OK -> "FAIL - see attempt-2/" and start a new P-D-C-A. -->
