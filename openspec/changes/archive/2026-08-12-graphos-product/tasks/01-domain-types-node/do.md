# Task 1 — Domain.Types.Node — DO

**Task slug**: `01-domain-types-node`
**Attempt**: 1
**Status**: in-progress

## Summary

Implemented `Graphos.Domain.Types.Node` with spec-required types and fields, maintaining backward compatibility with legacy fields.

## What Was Implemented

### FileType (6 constructors — spec complete)
- `CodeFile`, `DocFile`, `PaperFile`, `ImageFile`, `VideoFile`, `AudioFile`
- Aeson instances: `toJSON`/`parseJSON` with string mapping (`"code"`, `"doc"`, `"paper"`, `"image"`, `"video"`, `"audio"`)
- `DocumentFile` renamed to `DocFile` across entire codebase (4 files: `Infer.hs`, `Markdown.hs`, `FormatContext.hs`, `Analysis.hs`, `Context.hs`, `Conversation.hs`, `ContextSpec.hs`)

### Node (12 spec fields + 5 legacy fields — spec partially complete)
- **Spec fields (12)**: `nodeId`, `nodeLabel`, `nodeFileType`, `nodeSourceFile`, `nodeLineStart`, `nodeLineEnd`, `nodeSignature`, `nodeCommunityId`, `nodeKind`, `nodeDegree`, `nodeIsBridge`, `nodeExtra`
- **Legacy fields (5, to be removed in migration task)**: `nodeSourceLocation`, `nodeSourceUrl`, `nodeCapturedAt`, `nodeAuthor`, `nodeContributor`
- All fields strict (`!`), `StrictData` pragma
- `NFData` instances for both FileType and Node
- Aeson `ToJSON`/`FromJSON` instances with all 17 fields

### NodeId (deferred)
- Remains `type NodeId = Text` (type alias)
- `newtype NodeId = NodeId Text` deferred to dedicated migration task per agreed strategy (Option 3)

### Cross-codebase migration
- Added 5 new spec fields (`nodeLineStart`, `nodeCommunityId`, `nodeDegree`, `nodeIsBridge`, `nodeExtra`) as `Nothing` to all ~23 Node construction sites across 15+ files
- Fixed `formatContext` `showFileType` to handle `AudioFile`
- Fixed `Obsidian.hs` Node constructor field count
- Fixed test `testNode` helpers in `AnalysisSpec.hs` and `CommunitySpec.hs`

## Key Decisions

1. **Legacy fields retained**: 5 old fields (`nodeSourceLocation` etc.) kept alongside new spec fields to avoid cascading breakage. Dedicated migration task will remove them.
2. **NodeId stays as type alias**: Breaking change across ~50 modules deferred. Newtype migration is a separate task.
3. **nodeLineStart :: Maybe Int vs nodeSourceLocation :: Maybe Text**: New field uses `Int` (line number), legacy used `Text` ("L1", "L42"). Both coexist for now.
4. **nodeExtra :: Maybe Value**: Extensible metadata field per spec, uses Aeson `Value` type.

## Concrete Changes

Files modified:
- `src/Graphos/Domain/Types/Node.hs` — full rewrite (StrictData, 6 FileType, 17-field Node, NFData, Aeson)
- `src/Graphos/UseCase/Infer.hs` — DocumentFile → DocFile
- `src/Graphos/UseCase/Extract/Markdown.hs` — DocumentFile → DocFile + new Node fields
- `src/Graphos/UseCase/FormatContext.hs` — DocumentFile → DocFile + AudioFile pattern + new Node fields
- `src/Graphos/Domain/Analysis.hs` — DocumentFile → DocFile + new Node fields
- `src/Graphos/Domain/Context.hs` — DocumentFile → DocFile + new Node fields
- `src/Graphos/UseCase/Conversation.hs` — DocumentFile → DocFile
- `src/Graphos/Infrastructure/Export/Obsidian.hs` — Node constructor field count
- All other Infrastructure/UseCase/test files — new Node fields added as Nothing