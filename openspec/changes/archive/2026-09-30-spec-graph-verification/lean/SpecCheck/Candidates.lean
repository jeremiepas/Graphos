/-
Candidates.lean — contradiction candidates, stale supersession, and the
adjudication gate.

The graph's promise is completeness of *surfacing* (`areCandidates_complete`);
the model's authority is bounded by the gate (`blocking ⊆ confirmed ⊆
candidates`). No stage can exceed the one before it.
-/
import SpecCheck.Basic

namespace SpecCheck

-- ---------------------------------------------------------------------------
-- Contradiction candidates: shared `constrains` target
-- ---------------------------------------------------------------------------

def isActiveRequirement (g : SpecGraph) (r : NodeId) : Bool :=
  g.nodes.any fun n => n.id == r && n.kind == Kind.requirement && n.active

/-- Targets a requirement constrains. -/
def constrainsTargets (g : SpecGraph) (r : NodeId) : List NodeId :=
  g.edges.filterMap fun e =>
    if e.src == r && e.rel == Rel.constrains then some e.tgt else none

/-- The decision procedure at the heart of the check: are two distinct active
requirements constraining at least one common target? -/
def areCandidates (g : SpecGraph) (r1 r2 : NodeId) : Bool :=
  (r1 != r2)
    && isActiveRequirement g r1
    && isActiveRequirement g r2
    && (constrainsTargets g r1).any (constrainsTargets g r2).contains

/-- Membership bridge for `constrainsTargets`. -/
theorem mem_constrainsTargets (g : SpecGraph) (r t : NodeId)
    (e : SpecEdge) (hmem : e ∈ g.edges) (hsrc : e.src = r)
    (hrel : e.rel = Rel.constrains) (htgt : e.tgt = t) :
    t ∈ constrainsTargets g r := by
  unfold constrainsTargets
  apply List.mem_filterMap.mpr
  refine ⟨e, hmem, ?_⟩
  rw [hsrc, hrel, htgt]
  simp

/-- **Completeness**: whenever two distinct active requirements each hold a
`constrains` edge to the same target, the check surfaces them. The graph can
miss nothing matching the pattern — candidate quality is then the
adjudicator's problem, never silent loss. -/
theorem areCandidates_complete (g : SpecGraph) (r1 r2 t : NodeId)
    (hne : r1 ≠ r2)
    (hr1 : isActiveRequirement g r1 = true)
    (hr2 : isActiveRequirement g r2 = true)
    (e1 : SpecEdge) (he1 : e1 ∈ g.edges) (h1s : e1.src = r1)
    (h1r : e1.rel = Rel.constrains) (h1t : e1.tgt = t)
    (e2 : SpecEdge) (he2 : e2 ∈ g.edges) (h2s : e2.src = r2)
    (h2r : e2.rel = Rel.constrains) (h2t : e2.tgt = t) :
    areCandidates g r1 r2 = true := by
  unfold areCandidates
  have ht1 : t ∈ constrainsTargets g r1 :=
    mem_constrainsTargets g r1 t e1 he1 h1s h1r h1t
  have ht2 : t ∈ constrainsTargets g r2 :=
    mem_constrainsTargets g r2 t e2 he2 h2s h2r h2t
  have hne' : (r1 != r2) = true := by
    simp [bne_iff_ne]
    exact hne
  have hshared : (constrainsTargets g r1).any (constrainsTargets g r2).contains = true := by
    apply List.any_eq_true.mpr
    exact ⟨t, ht1, by simpa using ht2⟩
  simp [hne', hr1, hr2, hshared]

/-- Candidate enumeration over the active requirements (presentation layer
over the decision procedure). -/
def candidatePairs (g : SpecGraph) : List (NodeId × NodeId) :=
  let rs := activeRequirements g
  rs.flatMap fun r1 =>
    rs.filterMap fun r2 =>
      if r1 < r2 && areCandidates g r1 r2 then some (r1, r2) else none

-- ---------------------------------------------------------------------------
-- Stale supersession
-- ---------------------------------------------------------------------------

/-- (active source, superseded decision) pairs where an active artifact still
references an inactive decision. -/
def staleSupersessions (g : SpecGraph) : List (NodeId × NodeId) :=
  g.edges.filterMap fun e =>
    if e.rel == Rel.references
        && (g.nodes.any fun n => n.id == e.src && n.active)
        && (g.nodes.any fun n =>
              n.id == e.tgt && n.kind == Kind.decision && !n.active)
    then some (e.src, e.tgt)
    else none

-- ---------------------------------------------------------------------------
-- The adjudication gate
-- ---------------------------------------------------------------------------

inductive Verdict where
  | conflict
  | compatible
  | duplicate
  | unadjudicated
deriving Repr, DecidableEq

/-- The gate: a pure filter — only pairs the model confirmed as conflicts
block. -/
def gate (adjudicate : NodeId × NodeId → Verdict)
    (candidates : List (NodeId × NodeId)) : List (NodeId × NodeId) :=
  candidates.filter fun p => adjudicate p == Verdict.conflict

/-- The gate can only block on a surfaced candidate — the model cannot invent
findings. -/
theorem blocking_subset (adjudicate : NodeId × NodeId → Verdict)
    (candidates : List (NodeId × NodeId)) (p : NodeId × NodeId)
    (h : p ∈ gate adjudicate candidates) : p ∈ candidates :=
  (List.mem_filter.mp h).1

/-- Every blocking pair really was confirmed. -/
theorem blocking_confirmed (adjudicate : NodeId × NodeId → Verdict)
    (candidates : List (NodeId × NodeId)) (p : NodeId × NodeId)
    (h : p ∈ gate adjudicate candidates) : adjudicate p = Verdict.conflict := by
  have := (List.mem_filter.mp h).2
  simpa using this

/-- No candidates → nothing can block, whatever the model says. -/
theorem no_candidates_no_block (adjudicate : NodeId × NodeId → Verdict) :
    gate adjudicate [] = [] := rfl

/-- Dismissing everything blocks nothing. -/
theorem all_dismissed_no_block (candidates : List (NodeId × NodeId))
    (adjudicate : NodeId × NodeId → Verdict)
    (hdismiss : ∀ p ∈ candidates, adjudicate p = Verdict.compatible) :
    gate adjudicate candidates = [] := by
  unfold gate
  apply List.filter_eq_nil_iff.mpr
  intro p hp
  rw [hdismiss p hp]
  simp

end SpecCheck
