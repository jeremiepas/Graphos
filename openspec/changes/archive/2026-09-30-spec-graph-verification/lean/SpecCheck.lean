/-
SpecCheck — formal validation of the spec-graph-verification proposal.

The trust chain's deterministic components as proved Lean: the topological
re-checker certifies acyclicity, path certificates are sound by construction,
contradiction-candidate detection is complete for shared-target pairs, and the
adjudication gate blocks only on surfaced candidates.
-/
import SpecCheck.Basic
import SpecCheck.Topo
import SpecCheck.Paths
import SpecCheck.Candidates
import SpecCheck.Scenarios
