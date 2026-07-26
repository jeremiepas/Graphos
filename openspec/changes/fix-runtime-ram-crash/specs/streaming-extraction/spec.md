## ADDED Requirements

### Requirement: Batch merge with incremental GC

The extraction pipeline SHALL merge extraction results batch-by-batch into the final aggregate `Extraction`, evaluating the aggregate size and calling `performGC` after each batch completes, instead of accumulating all results in `IORef`s and merging only at the end.

- **Plan**: Bound peak memory during extraction by merging incrementally instead of accumulating.
- **Do**: After each file group (LSP batch, tree-sitter group, doc/image file) completes, merge its `Extraction` into the running aggregate, evaluate the aggregate size, and perform GC.
- **Check**: Peak memory during extraction stays within bounds (measured via `+RTS -s`).
- **Act**: If GC pauses are too long, reduce batch frequency or add explicit `evaluate` before GC.

#### Scenario: Sequential batch merge
- **WHEN** a file group completes extraction and produces an `Extraction`
- **THEN** the system merges the group's `Extraction` into the running aggregate using `mergeExtractions`
- **AND** evaluates the aggregate size via `evaluate (Map.size ...)`
- **AND** calls `performGC` to reclaim memory from completed batch data

#### Scenario: Memory does not accumulate across batches
- **WHEN** extracting from a 50k+ file codebase with multiple extraction groups
- **THEN** peak memory during extraction SHALL NOT exceed 2× the final aggregate size
- **AND** memory usage between batches SHALL decrease after GC

### Requirement: Map-based edge accumulation

Edge accumulation SHALL use `IORef (Map EdgeId Edge)` instead of `IORef ([Edge] -> [Edge])` diff lists, merging edges via `Map.union` with deduplication by `EdgeId`.

- **Plan**: Eliminate O(n) closure chain overhead from diff-list accumulation.
- **Do**: Replace all `[Edge] -> [Edge]` accumulators with `Map EdgeId Edge` accumulators, using `Map.union` for merging.
- **Check**: No diff-list accumulators remain in `UseCase.Extract`. Memory profile shows flat edge accumulation instead of growing chain.
- **Act**: If `Map` insertion overhead is measurable, consider `HashMap` as alternative.

#### Scenario: Edge accumulator deduplicates by EdgeId
- **WHEN** two extractions produce edges with the same `EdgeId`
- **THEN** the `Map.union` merge keeps the second edge (right-biased), deduplicating automatically

#### Scenario: Edge accumulator enables GC of processed batches
- **WHEN** a batch's extraction is merged into the running aggregate
- **THEN** the batch's local `Extraction` becomes unreachable and eligible for GC
- **AND** the `Map EdgeId Edge` accumulator does not hold references to the batch's intermediate data structures