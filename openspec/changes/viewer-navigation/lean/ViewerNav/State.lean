/-
State.lean — the navigable viewer state machine.

Models the viewer.js single-dispatcher state (`depth`, `selection`, `hops`,
`facets`) and the action classification from design Decision 2. `searchResults`
is derived state and deliberately excluded (design Decision 1).
-/

namespace ViewerNav

abbrev NodeId := String
abbrev CommunityId := Nat

/-- The navigable position — the three breadcrumb levels. viewer.js's
`depth`/`selection` pair collapses to exactly these three shapes. -/
inductive Position where
  | overview
  | community (cid : CommunityId)
  | node (cid : CommunityId) (nid : NodeId)
deriving Repr, DecidableEq

/-- View-tuning state: hop radius and active facets. Per the history
requirement, changes here must NOT create history entries. -/
structure Tuning where
  hops : Nat := 2
  facets : List String := []
deriving Repr, DecidableEq

/-- The navigable viewer state = position + tuning. -/
structure ViewerState where
  pos : Position := .overview
  tuning : Tuning := {}
deriving Repr, DecidableEq

/-- Dispatcher actions (viewer.js `dispatch`). -/
inductive Action where
  | goto (p : Position)          -- SET_DEPTH / SET_SELECTION
  | setHops (n : Nat)            -- SET_HOPS
  | toggleFacet (f : String)     -- TOGGLE_FACET / SET_FACETS
deriving Repr, DecidableEq

/-- Design Decision 2: position changes push history; tuning changes replace. -/
def Action.isPositionChange : Action → Bool
  | .goto _ => true
  | _ => false

/-- viewer.js clamps hops to [1, 6]. -/
def clampHops (n : Nat) : Nat := max 1 (min 6 n)

def applyAction (s : ViewerState) : Action → ViewerState
  | .goto p => { s with pos := p }
  | .setHops n => { s with tuning := { s.tuning with hops := clampHops n } }
  | .toggleFacet f =>
      let fs := s.tuning.facets
      { s with tuning := { s.tuning with
          facets := if fs.contains f then fs.filter (· != f) else f :: fs } }

/-- Tuning actions never move the position — the precondition behind the
"facet toggles do not pollute history" requirement. -/
theorem applyAction_nonpos_pos (s : ViewerState) (a : Action)
    (h : a.isPositionChange = false) : (applyAction s a).pos = s.pos := by
  cases a with
  | goto p => simp [Action.isPositionChange] at h
  | setHops n => rfl
  | toggleFacet f => rfl

/-- Hops are always clamped into the UI's legal range. -/
theorem applyAction_hops_range (s : ViewerState) (n : Nat) :
    1 ≤ (applyAction s (.setHops n)).tuning.hops ∧
    (applyAction s (.setHops n)).tuning.hops ≤ 6 := by
  simp [applyAction, clampHops]
  omega

end ViewerNav
