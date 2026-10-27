## Why

The `--resolution` help text contradicts the tool's actual behavior. `--help`
states *"higher = fewer larger communities (default: 1.0, try 0.3–0.5 for 100k+
nodes)."* Observed on `--cluster-only`: resolution 0.4 made the largest community
**bigger** (43,573 vs 32,351 at default 1.0) and 2.0 left it unchanged (43,604).
The documented 0.3–0.5 guidance actively worsened results. Misleading guidance
sends users the wrong direction and wastes long clustering runs.

## What Changes

- **Correct the `--resolution` help text and README** to match actual Leiden
  semantics as implemented (verified empirically, not assumed).
- Add a **one-line caveat** that resolution cannot split a densely-connected
  subgraph (pointing to the community-size cap as the real remedy).
- Add a **startup INFO log** echoing the effective resolution and its expected
  directional effect, so behavior is self-documenting.
- If investigation shows the implementation—not the docs—is wrong, correct the
  parameter mapping instead; the spec below is written to the observed behavior.

## Capabilities

### New Capabilities
- `resolution-guidance`: accurate documentation and runtime echo of the
  clustering resolution parameter's effect.

### Modified Capabilities
None in this change. Inspection of `openspec/specs/community-detection/spec.md`
found its "Resolution data type and defaults" requirement prescribing an
auto-tune table (`100k+ → γ 0.3–0.5`) that (a) is **not implemented** (the
pipeline uses only `cfgResolution`/`--resolution`, default 1.0) and (b) names
the same range the new guidance forbids when it worsens outcomes. Changing a
behavioral requirement without the empirical sweep would be silent guesswork,
so no `community-detection` delta is included here; `tasks.md` requires
reconciling that accepted spec with the sweep's findings.

## Impact

- **CLI help text** (optparse-applicative parser docs).
- **README / workflow docs** clustering section.
- **UseCase/Cluster**: INFO log of effective resolution.
- Low risk; primarily documentation plus one log line (or a parameter-mapping fix
  if the defect is in code).
