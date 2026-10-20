/-
  Graphos formalization — root module.

  This package is a *parallel verification companion* to the Haskell
  implementation, not a runtime dependency: no FFI, no cabal coupling.
  Theorems live in the `GraphosFormalization` namespace and mirror the
  requirements in `docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`.

  Modules:
    * `GraphosFormalization.Basic`        — package skeleton / build smoke check.
    * `GraphosFormalization.Ext.Basic`    — Phase 2 objects and the merge operator.
    * `GraphosFormalization.Ext.Colimit`  — Phase 2 universal property
      (`mergedExtraction_isColimit`, AVI-620 §2).

  Maintenance owner: R&D cell.
-/
import GraphosFormalization.Basic
import GraphosFormalization.Ext.Basic
import GraphosFormalization.Ext.Colimit
