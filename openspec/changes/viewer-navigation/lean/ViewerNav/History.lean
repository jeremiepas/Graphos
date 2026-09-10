/-
History.lean — "History-integrated back and forward" requirement.

Browser history as a zipper (back stack / current / forward stack), with the
push-vs-replace split from design Decision 2. Proves:
  * goBack_push            — Back returns to the previous position
  * goForward_goBack       — Forward replays exactly the step taken Back
  * steps_nonpos_back      — tuning actions never touch the back stack,
                             so facet toggles cannot pollute history
  * fresh_canGoBack        — controls mirror history at the ends
-/
import ViewerNav.State

namespace ViewerNav

structure History where
  back : List ViewerState
  current : ViewerState
  forward : List ViewerState
deriving Repr, DecidableEq

namespace History

def fresh (s : ViewerState) : History := ⟨[], s, []⟩

/-- Position-changing navigation: push an entry; browser semantics clear the
forward stack. -/
def push (h : History) (s : ViewerState) : History := ⟨h.current :: h.back, s, []⟩

/-- View-tuning update: replace the current entry in place. -/
def replace (h : History) (s : ViewerState) : History := ⟨h.back, s, h.forward⟩

def goBack : History → Option History
  | ⟨[], _, _⟩ => none
  | ⟨p :: bs, c, fs⟩ => some ⟨bs, p, c :: fs⟩

def goForward : History → Option History
  | ⟨_, _, []⟩ => none
  | ⟨bs, c, f :: fs⟩ => some ⟨c :: bs, f, fs⟩

def canGoBack (h : History) : Bool := !h.back.isEmpty
def canGoForward (h : History) : Bool := !h.forward.isEmpty

end History

/-- Scenario "Back returns to previous position": after a push, Back lands on
what was current before the push. -/
theorem goBack_push (h : History) (s : ViewerState) :
    (h.push s).goBack = some ⟨h.back, h.current, [s]⟩ := rfl

/-- Scenario "Forward replays a step back": going Back then Forward restores
the history exactly — same position, same stacks. -/
theorem goForward_goBack (h h' : History) (hb : h.goBack = some h') :
    h'.goForward = some h := by
  obtain ⟨back, current, forward⟩ := h
  cases back with
  | nil => simp [History.goBack] at hb
  | cons p bs =>
    simp [History.goBack] at hb
    subst hb
    rfl

/-- Scenario "On-screen controls mirror history": empty history disables Back. -/
theorem fresh_canGoBack (s : ViewerState) : (History.fresh s).canGoBack = false := rfl

/-- ... and the first drill-down enables it. -/
theorem push_canGoBack (h : History) (s : ViewerState) :
    (h.push s).canGoBack = true := rfl

-- ---------------------------------------------------------------------------
-- The dispatcher over history: push/replace split by action class
-- ---------------------------------------------------------------------------

/-- One dispatcher step: position changes push, tuning changes replace. -/
def step (h : History) (a : Action) : History :=
  let s := applyAction h.current a
  if a.isPositionChange then h.push s else h.replace s

def steps (h : History) (as : List Action) : History := as.foldl step h

/-- Replace never touches the back stack. -/
theorem replace_back (h : History) (s : ViewerState) :
    (h.replace s).back = h.back := rfl

/-- Scenario "Facet toggles do not pollute history" (generalized): any sequence
of non-position actions leaves the back stack — hence every Back target —
untouched. -/
theorem steps_nonpos_back : ∀ (as : List Action) (h : History),
    (∀ a ∈ as, a.isPositionChange = false) → (steps h as).back = h.back
  | [], _, _ => rfl
  | a :: as, h, hnp => by
    have ha : a.isPositionChange = false := hnp a (by simp)
    have ih : (steps (step h a) as).back = (step h a).back :=
      steps_nonpos_back as (step h a) (fun b hb => hnp b (by simp [hb]))
    calc (steps h (a :: as)).back
        = (steps (step h a) as).back := rfl
      _ = (step h a).back := ih
      _ = h.back := by simp [step, ha, History.replace]

/-- The position Back would land on. -/
def backPosition (h : History) : Option Position := h.back.head?.map (·.pos)

/-- Corollary in scenario terms: after any burst of facet/hop tweaks, Back goes
to the same position it would have gone to before them. -/
theorem steps_nonpos_backPosition (as : List Action) (h : History)
    (hnp : ∀ a ∈ as, a.isPositionChange = false) :
    backPosition (steps h as) = backPosition h := by
  unfold backPosition
  rw [steps_nonpos_back as h hnp]

end ViewerNav
