/-
  Graphos formalization — root module.

  This package is a *parallel verification companion* to the Haskell
  implementation, not a runtime dependency: no FFI, no cabal coupling.
  Theorems live in the `GraphosFormalization` namespace and mirror the
  requirements in `docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`.

  Modules:
    * `GraphosFormalization.Basic`        — package skeleton / build smoke check.
    * `GraphosFormalization.Phase1.Modularity` — Phase 1 modularity `Q` and
      INV-Q0 (`modularity_edgesAbsent_zero`, AVI-618 §2).
    * `GraphosFormalization.Phase1.RangeBound` — exact (division-free) INV-Q1
      range bound (`modularity_range_bound`, AVI-618 §3) and the tight
      `K_{2,2}` fixture.
    * `GraphosFormalization.Phase1.PartitionOnly` — INV-Q2
      (`modularity_partitionOnly`, AVI-618 §2): `Q` depends only on the blocks,
      not on member-list order; exact-layer key-order invariance.
    * `GraphosFormalization.Ext.Basic`    — Phase 2 objects and the merge operator.
    * `GraphosFormalization.Ext.Colimit`  — Phase 2 universal property
      (`mergedExtraction_isColimit`, AVI-620 §2).

  Maintenance owner: R&D cell.
-/
import GraphosFormalization.Basic
import GraphosFormalization.Phase1.Modularity
import GraphosFormalization.Phase1.RangeBound
import GraphosFormalization.Phase1.PartitionOnly
import GraphosFormalization.Ext.Basic
import GraphosFormalization.Ext.Colimit
