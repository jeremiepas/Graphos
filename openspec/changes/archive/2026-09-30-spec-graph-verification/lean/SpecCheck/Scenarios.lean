/-
Scenarios.lean — the spec's scenarios validated executably on fixtures.
Fixture shape mirrors the historical real-world case: two requirements
constraining graph.html (self-contained vs remote mode), a dependency cycle,
an unimplemented requirement, a stale ADR reference.
-/
import SpecCheck.Basic
import SpecCheck.Topo
import SpecCheck.Paths
import SpecCheck.Candidates

namespace SpecCheck.Scenarios
open SpecCheck

def req (id : NodeId) : SpecNode := { id := id, kind := Kind.requirement }

def dec (id : NodeId) (active : Bool) : SpecNode :=
  { id := id, kind := Kind.decision, active := active }

def code (id : NodeId) : SpecNode := { id := id, kind := Kind.code }

def tgt (id : NodeId) : SpecNode := { id := id, kind := Kind.constraintNode }

def e (s : NodeId) (r : Rel) (t : NodeId) : SpecEdge :=
  { src := s, rel := r, tgt := t }

/-- The main fixture. R-selfcontained and R-remote both constrain graph.html
(the historical conflict); R-lod satisfies fn_render (implemented, with
path); R-ghost has no path to code (unimplemented); R-remote references the
superseded ADR-embed. -/
def gMain : SpecGraph :=
  { nodes :=
      [ req "R-selfcontained"
      , req "R-remote"
      , req "R-lod"
      , req "R-ghost"
      , dec "ADR-embed" false
      , dec "ADR-lsp" true
      , code "fn_render"
      , tgt "graph.html"
      ]
  , edges :=
      [ e "R-selfcontained" Rel.constrains "graph.html"
      , e "R-remote" Rel.constrains "graph.html"
      , e "R-lod" Rel.satisfies "fn_render"
      , e "R-remote" Rel.references "ADR-embed"
      , e "R-lod" Rel.dependsOn "R-selfcontained"
      ]
  }

/-- Cycle fixture: A depends B, B refines C, C depends A. -/
def gCycle : SpecGraph :=
  { nodes := [ req "A", req "B", req "C" ]
  , edges :=
      [ e "A" Rel.dependsOn "B"
      , e "B" Rel.refines "C"
      , e "C" Rel.dependsOn "A"
      ]
  }

def specRels : List Rel := [Rel.dependsOn, Rel.refines, Rel.supersedes]


-- Requirement: Certificate-carrying cycle check --------------------------------

/-- Scenario "Cycle is found with a witness": the closed walk A→B→C→A checks
as a witness, and the sort refuses to produce a certificate. -/
example :
    isClosedChain (stepRel gCycle specRels) [ "A", "B", "C", "A" ] = true := by
  decide

example : checkAcyclic gCycle specRels = none := by decide

/-- Scenario "Acyclic verdict carries its order": the main fixture is a DAG —
the check emits an order and the re-checker accepts it (soundness is then
`checkAcyclic_no_cycle`, proved generally in Topo.lean). -/
example : (checkAcyclic gMain specRels).isSome = true := by decide

example :
    (match checkAcyclic gMain specRels with
     | some order => isTopoOrder gMain specRels order
     | none => false)
      = true := by
  decide


-- Requirement: Coverage checks with path certificates --------------------------

/-- Scenario "Implemented verdict is witnessed": R-lod reaches code with a
certificate that re-checks. -/
example : (implementedWitness gMain "R-lod").isSome = true := by decide

example :
    (match implementedWitness gMain "R-lod" with
     | some p => isPathTo (stepRel gMain coverageRels) "R-lod" (isCodeNode gMain) p
     | none => false)
      = true := by
  decide

/-- Scenario "Unimplemented requirement is reported": R-ghost has no path to
any code node. -/
example : "R-ghost" ∈ unimplemented gMain := by decide

example : "R-lod" ∉ unimplemented gMain := by decide


-- Requirement: Contradiction candidates and structural findings ----------------

/-- Scenario "Shared-target pair is surfaced": the historical
self-contained/remote pair on graph.html. -/
example : areCandidates gMain "R-selfcontained" "R-remote" = true := by decide

example : ("R-remote", "R-selfcontained") ∈ candidatePairs gMain := by decide

/-- Requirements without a shared constrained target are not candidates. -/
example : areCandidates gMain "R-lod" "R-remote" = false := by decide

/-- Scenario "Stale supersession is flagged": active R-remote still references
the superseded ADR-embed. -/
example : staleSupersessions gMain = [ ("R-remote", "ADR-embed") ] := by decide


-- Requirement: Gate discipline --------------------------------------------------

def confirmFirst : NodeId × NodeId → Verdict := fun p =>
  if p == ("R-remote", "R-selfcontained") then Verdict.conflict
  else Verdict.compatible

/-- Scenario "Confirmed conflict blocks with trace": exactly the confirmed
pair blocks. -/
example :
    gate confirmFirst (candidatePairs gMain)
      = [ ("R-remote", "R-selfcontained") ] := by
  decide

/-- Scenario "Dismissed candidates do not gate". -/
example :
    gate (fun _ => Verdict.compatible) (candidatePairs gMain) = [] := by decide

/-- `unadjudicated` never blocks. -/
example :
    gate (fun _ => Verdict.unadjudicated) (candidatePairs gMain) = [] := by
  decide

end SpecCheck.Scenarios
