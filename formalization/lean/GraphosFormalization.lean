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
    * `GraphosFormalization.Phase1.LocalMove` — INV-Q3
      (`modularity_localMoveNonDecreasing`, AVI-618 §2 / §5 fixture 4): a
      Leiden local-moving pass never decreases `Q`; plus the exact witnesses
      showing the shipped `bestCommunityFor` gate and `localMovingLoop`'s
      `σ_tot` update do not yet implement the sound rule.
    * `GraphosFormalization.Phase1.Determinism` — INV-Q4
      (`modularity_deterministic`, AVI-618 §2 / §5 fixture 7): `Q` is a
      deterministic pure function of `(g, cm)`, folding communities in
      ascending `CommunityId` order, which is the unique order of the map's
      key set (insertion-order independence, canonicalisation stability).
    * `GraphosFormalization.Phase1.FixturesExact` — INV-Q5
      (`modularity_fixturesExact`, AVI-618 §2 / §5 fixtures 1 & 2): the
      single-block fixture has `Q = 0` and the all-singletons fixture has
      `Q = −(1/(2m))²·Σ_v k_v² < 0`, exactly.
    * `GraphosFormalization.Phase1.CrossCheckFixtures` — AVI-618 §6: the seven
      §5 fixtures as concrete `EdgeGraph`/`Partition` data with their exact
      `Qnum` (`crossCheckRows`, AVI-1301), consumed by the
      `modularity-crosscheck` executable and compared against Haskell
      `modularity` in `tests/Graphos/Domain/ModularityCrossCheckSpec.hs`.
    * `GraphosFormalization.Ext.Basic`    — Phase 2 objects and the merge operator.
    * `GraphosFormalization.Ext.Colimit`  — Phase 2 universal property
      (`mergedExtraction_isColimit`, AVI-620 §2).
    * `GraphosFormalization.Ext.Strict`  — Phase 2 consistent case (§3.1):
      strict colimit with unique mediator, binary merge commutation, and
      order-independence of both fold sites
      (`mergedExtraction_strictColimit_orderIndependent`, AVI-620 §3.1).
    * `GraphosFormalization.Ext.Lax`     — Phase 2 conflicting case (§3.2):
      no matching-family cocone (strict universal property fails) and the
      lax colimit with the first/old-wins, non-invertible comparison
      (`mergedExtraction_laxColimit_oldWins`, AVI-620 §3.2, AVI-1304).
    * `GraphosFormalization.Ext.Complexity` — Phase 2 §4 n-way fold
      complexity: the key-visit cost of `foldl' ⊕ emptyExtraction` is at most
      `Σᵢ (|extNodes Eᵢ| + |extEdges Eᵢ|)` — one linear pass
      (`mergedExtraction_nwayFold_linear`, AVI-620 §4, AVI-1306).
    * `GraphosFormalization.Seam.SupportGraph` — the Phase 1/2 **seam**
      (AVI-616 §1.1): the colimit `M` → `buildGraph gDirected M` → `Graph` →
      `neighbors` → simple support graph `G_s`, pinned as a function of `M` so
      that `Q` only sees the symmetric closure of the valid endpoint pairs
      (`buildGraph_supportAdj_merge_swap`, `ac4_seam_modularity_eq`,
      `dangling_resurrection`, AVI-1308).

  Maintenance owner: R&D cell.
-/
import GraphosFormalization.Basic
import GraphosFormalization.Phase1.Modularity
import GraphosFormalization.Phase1.RangeBound
import GraphosFormalization.Phase1.PartitionOnly
import GraphosFormalization.Phase1.LocalMove
import GraphosFormalization.Phase1.Determinism
import GraphosFormalization.Phase1.FixturesExact
import GraphosFormalization.Phase1.CrossCheckFixtures
import GraphosFormalization.Ext.Basic
import GraphosFormalization.Ext.Colimit
import GraphosFormalization.Ext.Strict
import GraphosFormalization.Ext.Lax
import GraphosFormalization.Ext.Complexity
import GraphosFormalization.Seam.SupportGraph
