# Contract inventory (AVI-1677, Stage 2)

Status: draft contract frozen at revision of PR #115 (Stage 0 spike).
Scope of this document: define decision IDs, Haskell sources, Lean theorem
obligations, ABI v0 encodings, error codes, ownership, and determinism guarantees
so Stage 1 (OSS bootstrap) and Stage 3 (Lean implementation) have a stable target.

## Decision IDs

- `AVI-1677-D0` (accepted, PR #114 plan rev 4): compile-time bridge, not
  a runtime-loaded plugin.
- `AVI-1677-D1`: Lean is permitted to use Mathlib/external Lean libraries.
- `AVI-1677-D2`: bridge must be a separate public OSS project
  (`graphos-lean-critique` or equivalent).
- `AVI-1677-D3`: Graphos keeps Haskell critique as reference/optional
  fallback during rollout.
- `AVI-1677-D4`: Stage-0 numeric predicate is `surpriseScore` (score arm only);
  natural-language reasons stay on the Haskell side unless a later ABI revision
  moves them.
- `AVI-1677-D5`: ABI v0 returns a scalar `uint64_t` score plus a version query;
  reason generation is reconstructed in Haskell from the same bonus rules in
  v0 and may be replaced by a reason mask in v0.1.
- `AVI-1677-D6`: `fileCategory` / graph traversal / community mapping /
  top-N selection remain in Haskell; Lean owns only crisp numeric predicates.
- `AVI-1677-D7` (accepted, AVI-1681): no new Graphos repository is created. Graphos stays in
  its existing repository. A separate repository is created only for the Lean-Haskell bridge
  (`graphos-lean-critique`), and only because the bridge needs an independently buildable
  Lean 4 project. Graphos consumes it as an external build dependency (Cabal linkage), not as
  a second copy of Graphos.

## Haskell source references

- `src/Graphos/Domain/Analysis.hs`
  - `surpriseScore :: Graph -> Edge -> Map NodeId CommunityId -> Text -> Text -> NodeId -> NodeId -> (Int, [Text])`
  - `surprisingConnections`, `crossFileSurprises`, `crossCommunitySurprises`
  - `suggestQuestions`, `bridgeNodeQuestions`, `lowCohesionQuestions`
  - `fileCategory :: Text -> FileType`
- `src/Graphos/Domain/Types.hs` (referenced for `Edge`, `Relation(..)`, `Confidence`,
  `FileType(..)`) — for field names and constructor names that must survive marshalling.
- `src/Graphos/Domain/Graph.hs` — `gNodes`, `gEdges` traversal and node metadata.
- `Graphos.Domain.Graph.Analysis` — cached connectivity not in Lean scope (v0).

## Lean theorem obligations (Stage 3)

Per `lean-proof-methodology`, each extracted predicate gets a spec theorem. v0
minimum:

1. `confBonus` monotone bucketing: `confX10 < 3 => bonus = 3`; `3 <= confX10 < 8 => bonus = 2`;
   `confX10 >= 8 => bonus = 1`.
2. Component-wise non-negativity of `crossFiletypeBonus`, `crossCommunityBonus`,
   `semBonus`.
3. `surpriseScore` total: finite, deterministic, no partial functions.
4. Round-trip parity with Haskell on pinned fixtures
   `{(0,1,1,0,6),(0,1,1,1,10),(9,0,0,0,1)}` and, for v1, golden fixtures.
5. Stability: `surpriseScore` is invariant under permutation of non-numeric
   inputs outside its ABI parameters.

Proof-only helpers must be noncomputable or excluded from the `@[export]` surface.

## ABI v0 (frozen)

Header: `spikes/avi-1677-lean-critique/include/graphos_lean_critique.h`.

```c
uint64_t graphos_surprise_score(uint8_t confX10,
                               uint8_t crossFiletype,
                               uint8_t crossComm,
                               uint8_t inferred);

uint32_t graphos_lean_critique_abi_version(void);
```

Constants:
- `GRAPHOS_LEAN_CRITIQUE_ABI_VERSION == 0`
- `GRAPHOS_LEAN_CRITIQUE_VERSION` is a preprocessor string (build-time),
  currently `"0.0.0-spike (AVI-1677 stage 0)"`.

Argument encoding:
- `confX10`: confidence `* 10`, saturating clamp to `0..10`, bucketed `<3`, `<8`.
- `crossFiletype`: `1` when endpoints differ in `FileType` (as `fileCategory` grouping),
  else `0`; values other than `0/1` are treated as `0` by Lean.
- `crossComm`: `1` when endpoints are in different communities, else `0`.
- `inferred`: `1` when `edgeRelation == Inferred`, else `0`.
- return: `confBonus + 2*crossFiletype + 1*crossComm + semBonus`, with
  `semBonus = inferred ? round(confBonus * 1.5) : 0` over the three possible
  `confBonus` values (`4,3,2` respectively).

Future reason-mask (v0.1, non-breaking addition): reserve bit values
`0x01` = crosses file types, `0x02` = bridges separate communities,
`0x04` = inferred connection; transport through an extra output parameter
only after Stage 2 is revisited.

## Error codes

v0 defines no fallible entry points beyond implicit argument normalisation.
Stage 3/4 may introduce:

- `GRAPHOS_LEAN_OK = 0`
- `GRAPHOS_LEAN_ERR_INVALID_ARGUMENT = 1`
- `GRAPHOS_LEAN_ERR_VERSION_MISMATCH = 2`
- `GRAPHOS_LEAN_ERR_OUT_OF_MEMORY = 3`

No heap allocation occurs in v0, so no ownership transfer occurs in v0.

## Memory ownership

- v0 passes only scalars; no caller- or Lean-owned buffers are exchanged.
- Future string/array outputs: Lean allocates a buffer, returns a pointer and a
  count, and exposes an explicit `graphos_lean_free(void*)`; the Haskell
  adapter copies into managed Haskell structures and frees before returning.
- The Haskell adapter is the only caller; cabal-built executables must link
  the static `libgraphoslean` archive at build time.

## Determinism / ordering guarantees

- `surpriseScore` is a pure function of `(confX10, crossFiletype, crossComm, inferred)`.
- Haskell-side ordering of candidates is frozen separately:
  `surprisingConnections` uses `selectTopNOn` with key `(Down score, original index)`
  for cross-file surprises, and best-candidate-per-community-pair plus the same
  bounded top-N key for cross-community surprises.
- Any migration to Lean must preserve both score parity and candidate ordering.

## Linkage (Stage 4 summary)

Build producing `Critique.o`:
```
lean -c Critique.c Critique.lean
leanc -c Critique.c -o Critique.o
ar rcs libgraphoslean.a Critique.o
```
Link inputs (from `leanc --print-ldflags`):
`libInit`, `libStd`, `libLean`, `libleanrt`, `libleancpp`, `libgmp`,
`libuv`, plus `stdc++`, `m`, `dl`, `pthread`.

## Files touched by this contract

- `spikes/avi-1677-lean-critique/include/graphos_lean_critique.h`
- `spikes/avi-1677-lean-critique/Critique.lean`
- `spikes/avi-1677-lean-critique/README.md`
- `openspec/changes/AVI-1677-lean4-critique-replacement/tasks.md`
