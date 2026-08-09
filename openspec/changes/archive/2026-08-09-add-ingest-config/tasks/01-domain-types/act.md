# Act — Task 1: Domain Types for IngestConfig

## Learnings

1. **Merge semantics**: The "project overrides global" model works well when project always wins. The previous "differs from default" approach was flawed because plain Haskell records can't distinguish "explicitly set to default" from "not set."

2. **ToJSON/FromJSON consistency**: Using `genericToJSON` with `fieldLabelModifier` for ToJSON but explicit `withObject` for FromJSON caused a snake_case vs camelCase mismatch. Best practice: use explicit instances for both when config file format is fixed.

3. **Warning-as-error**: `-Werror` catches unused imports and overlapping patterns early. Removing redundant patterns (`Nothing Nothing` cases) simplifies code.

4. **Test-driven merge logic**: Writing tests first revealed the contradictory expectations in merge semantics. Tests forced a clear decision: project always wins.

## Actions Taken
- Changed Bool field merge to always prefer project (simpler, more intuitive)
- Fixed ToJSON to use explicit `object` with snake_case keys matching FromJSON
- Removed redundant pattern matches in merge helpers
- Removed unused imports across library and test files

## Next
Proceed to Task 2: IngestIndex v2 with deduplication support.
