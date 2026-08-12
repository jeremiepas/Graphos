# Task 1 — Domain.Types.Node — PLAN

**Task slug**: `01-domain-types-node`
**Attempt**: 1
**Status**: in-progress

## Summary

Implement `Graphos.Domain.Types.Node` per spec `domain-types/Requirement: Domain.Types.Node`: newtype NodeId, 6 FileType constructors, 12 strict Node fields with Aeson instances. This is the foundational type every other module depends on.

## Scope

- `src/Graphos/Domain/Types/Node.hs` — complete rewrite to match spec
- `tests/Graphos/Domain/TypesSpec.hs` — new test file for Node, FileType, NodeId
- All modules constructing `Node` — must update field names and count

### Current State

Node.hs already partially migrated:
- `FileType`: 6 constructors done (CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile)
- `Node`: 12 spec fields + 5 legacy fields (backward compat migration in progress)
- `NodeId`: still `type NodeId = Text` (newtype deferred to dedicated migration task after Domain types stabilize)
- `NFData` instances added
- `DocumentFile` → `DocFile` rename completed across codebase
- New spec fields (nodeLineStart, nodeCommunityId, nodeDegree, nodeIsBridge, nodeExtra) added as `Nothing` to all Node construction sites
- **Build passes** (with legacy fields retained for backward compat)

### Remaining Work

1. **NodeId newtype migration** (deferred): Change `type NodeId = Text` → `newtype NodeId = NodeId Text` + update all ~50+ modules that use NodeId as bare Text. This is a cross-cutting change that deserves its own task.
2. **Node field migration** (deferred): Remove 5 legacy fields (nodeSourceLocation, nodeSourceUrl, nodeCapturedAt, nodeAuthor, nodeContributor) and migrate all consumers to use spec fields. Also a cross-cutting change.
3. **Domain.TypesSpec test**: Write Hspec test covering Node construction, FileType round-trip Aeson, field strictness verification.

### Spec References

- `domain-types/spec.md` Requirement: Domain.Types.Node
- `domain-types/spec.md` Requirement: Domain.Types.NodeId
- `domain-types/spec.md` Requirement: Domain.Types.FileType

### Affected Modules

Direct:
- `src/Graphos/Domain/Types/Node.hs`

Indirect (Node consumers, already updated for field count):
- `src/Graphos/Infrastructure/Export/Obsidian.hs`
- `src/Graphos/Infrastructure/Export/Memgraph.hs`
- `src/Graphos/Infrastructure/Export/Neo4j.hs`
- `src/Graphos/Infrastructure/LSP/Extraction.hs`
- `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs`
- `src/Graphos/Infrastructure/Server/MCP.hs`
- `src/Graphos/Domain/Analysis.hs`
- `src/Graphos/Domain/Context.hs`
- `src/Graphos/UseCase/Conversation.hs`
- `src/Graphos/UseCase/FormatContext.hs`
- `src/Graphos/UseCase/Infer.hs`
- `src/Graphos/UseCase/Query.hs`
- `src/Graphos/UseCase/SelectContext.hs`
- `src/Graphos/UseCase/Extract/Haskell.hs`
- `src/Graphos/UseCase/Extract/Markdown.hs`
- `tests/Graphos/Domain/AnalysisSpec.hs`
- `tests/Graphos/Domain/CommunitySpec.hs`
- `tests/Graphos/Domain/GraphSpec.hs`
- `tests/Graphos/UseCase/QuerySpec.hs`
- `tests/Graphos/UseCase/FormatContextSpec.hs`
- `tests/Graphos/Domain/ContextSpec.hs`

### Prerequisites

- None (this is the first task in the dependency chain)

### Risks

1. **NodeId newtype is cross-cutting**: Changing from type alias to newtype breaks every call site that uses NodeId as bare Text. Mitigation: defer to dedicated migration task.
2. **Legacy field removal is cross-cutting**: Removing 5 legacy fields breaks ~20 files. Mitigation: keep legacy fields temporarily, migrate in dedicated task.
3. **StrictData + Maybe Value**: `nodeExtra :: !(Maybe Value)` forces NFData for Value which requires aeson instance. Mitigation: aeson provides NFData Value since aeson >= 1.5.

## Check Criteria

**Defined BEFORE code (anti-confirmation-bias guard):**

### C1: Build succeeds with zero warnings
- Command: `nix-shell shell.nix --run "cabal build"`
- PASS: Exit code 0, no warnings
- FAIL: Any error or warning output

### C2: All tests pass
- Command: `nix-shell shell.nix --run "cabal test"`
- PASS: Exit code 0, all test suites pass
- FAIL: Any test failure

### C3: FileType has exactly 6 constructors
- Command: `grep -c "|" src/Graphos/Domain/Types/Node.hs | head -1` or visual inspection of `data FileType`
- PASS: CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile present
- FAIL: Missing or extra constructors

### C4: Node has 12 spec strict fields
- Verification: Read Node data declaration, count `!`-prefixed fields
- PASS: nodeId, nodeLabel, nodeFileType, nodeSourceFile, nodeLineStart, nodeLineEnd, nodeSignature, nodeCommunityId, nodeKind, nodeDegree, nodeIsBridge, nodeExtra — all with `!`
- FAIL: Missing field, non-strict field, wrong field name

### C5: No IO imports in Domain.Types.Node
- Command: `grep -E "import.*IO|import.*System" src/Graphos/Domain/Types/Node.hs`
- PASS: No output (zero IO imports)
- FAIL: Any import of IO or System modules

### C6: Aeson round-trip for FileType and Node
- Test: `decode (encode x) == Just x` for each FileType constructor and a sample Node
- PASS: Round-trip equality holds
- FAIL: Decoding fails or produces different value

### C7: Hspec test file exists with NodeId, FileType, Node coverage
- File: `tests/Graphos/Domain/TypesSpec.hs` (or equivalent)
- PASS: File exists, imports Node/FileType/NodeId, contains at least 3 test groups
- FAIL: File missing or insufficient coverage

## Result

<!-- Will be filled in CHECK step -->