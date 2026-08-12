# Task 1 — Domain.Types.Node — ACT

**Task slug**: `01-domain-types-node`
**Attempt**: 1
**Status**: PASS

## Summary

Task 1 PDCA cycle complete. Domain.Types.Node implemented with spec-required types. Backward compatibility maintained via legacy fields.

## Standardized Pattern

Pattern established for subsequent Domain type tasks:
1. `StrictData` pragma on all Domain.Types modules
2. `NFData` instances for all Domain data types
3. Explicit Aeson `ToJSON`/`FromJSON` instances (not Generic-derived) for field name control
4. New fields added alongside legacy fields when migration is cross-cutting
5. `DocumentFile` → `DocFile` rename complete; all 6 FileType constructors in place

## Known Gaps for Follow-up

1. **NodeId newtype migration**: Change `type NodeId = Text` → `newtype NodeId = NodeId Text`. Cross-cutting, affects ~50 modules. Create dedicated task.
2. **Legacy field removal**: Remove 5 old fields (nodeSourceLocation, nodeSourceUrl, nodeCapturedAt, nodeAuthor, nodeContributor) once all consumers migrated to spec fields. Create dedicated task.
3. **Domain.TypesSpec test**: Write dedicated Hspec test for Node, FileType, NodeId construction, Aeson round-trip.

## Next Tasks

- Task 2: Domain.Types.Edge
- Task 3: Domain.Types.Graph
- (NodeId migration: insert after Task 9)