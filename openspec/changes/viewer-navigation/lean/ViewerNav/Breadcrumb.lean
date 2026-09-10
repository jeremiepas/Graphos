/-
Breadcrumb.lean — "Breadcrumb trail" + the Escape half of the keyboard map.

The trail is a pure derivation of Position (design Decision 3). Proves:
  * breadcrumb_length_le  — at most three segments
  * breadcrumb_head       — the trail always starts at Overview
  * breadcrumb_last       — the last segment is the current position
  * breadcrumb_escape     — clicking the parent segment ≡ one Escape:
                            the escaped trail is the trail minus its last segment
  * escape_escape         — Escape always reaches Overview in ≤ 2 presses
-/
import ViewerNav.State

namespace ViewerNav

/-- Hierarchical Escape (keyboard requirement): close the panel (node → its
community), then up a level (community → overview), then stay. -/
def escape : Position → Position
  | .node cid _ => .community cid
  | .community _ => .overview
  | .overview => .overview

def rank : Position → Nat
  | .overview => 0
  | .community _ => 1
  | .node _ _ => 2

/-- Escape strictly descends the hierarchy — it can never loop. -/
theorem escape_rank_lt (p : Position) (h : p ≠ .overview) :
    rank (escape p) < rank p := by
  cases p with
  | overview => exact absurd rfl h
  | community _ => simp [escape, rank]
  | node _ _ => simp [escape, rank]

/-- Scenario "Escape walks up the hierarchy" (generalized): from any position,
two Escapes reach the Overview. -/
theorem escape_escape (p : Position) : escape (escape p) = .overview := by
  cases p <;> rfl

-- ---------------------------------------------------------------------------
-- Breadcrumb
-- ---------------------------------------------------------------------------

/-- The trail: Overview ▸ community ▸ node, derived purely from the position. -/
def breadcrumb : Position → List Position
  | .overview => [.overview]
  | .community c => [.overview, .community c]
  | .node c n => [.overview, .community c, .node c n]

/-- Requirement: "up to three segments". -/
theorem breadcrumb_length_le (p : Position) : (breadcrumb p).length ≤ 3 := by
  cases p <;> simp [breadcrumb]

/-- The trail is never empty and always starts at Overview. -/
theorem breadcrumb_head (p : Position) : (breadcrumb p).head? = some .overview := by
  cases p <;> rfl

/-- The last segment is the current position (the non-link segment). -/
theorem breadcrumb_last (p : Position) : (breadcrumb p).getLast? = some p := by
  cases p <;> rfl

/-- Scenario "Clicking an ancestor navigates" (structural half): the parent
segment's position is exactly `escape p` — the trail of the escaped position is
the current trail minus its last segment. Breadcrumb clicks and Escape can
therefore never disagree about the hierarchy. -/
theorem breadcrumb_escape (p : Position) (h : p ≠ .overview) :
    breadcrumb (escape p) = (breadcrumb p).dropLast := by
  cases p with
  | overview => exact absurd rfl h
  | community c => rfl
  | node c n => rfl

end ViewerNav
