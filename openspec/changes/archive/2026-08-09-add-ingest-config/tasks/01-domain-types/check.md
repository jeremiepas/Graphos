# Check — Task 1: Domain Types for IngestConfig

## Verification Results

### Build
- `cabal build` — PASS (110 modules compiled, no errors)
- `cabal test` — PASS (308 examples, 0 failures)

### Spec Compliance
- ✅ `IngestConfig` has all fields from spec: embed, embedModel, embedDimension, merge, deduplicate, resolution, minCommSize, maxLeidenIter, indexPath, url, categories
- ✅ `IngestUrlConfig` has all fields: timeout, userAgent, retry
- ✅ `IngestCategoryConfig` has all fields: embed, granularity
- ✅ `IngestCategories` covers all 6 categories: code, doc, paper, image, video, office
- ✅ `FileEntry` has hash and timestamp for deduplication
- ✅ `ToJSON` / `FromJSON` instances compile and round-trip correctly
- ✅ Merge helpers: project always overrides global
- ✅ `gcIngest` threaded through `GraphosConfig` and `mergeGraphosConfig`
- ✅ Backward compatibility: `icEmbed = False` by default

### Test Coverage
- Default values verified for all fields
- YAML parsing of full config with nested structures
- Merge logic: project overrides global, Maybe field merging, fallback to global
- Category resolution: inherits top-level, overrides when explicit
- Granularity resolution: inherits and overrides correctly
