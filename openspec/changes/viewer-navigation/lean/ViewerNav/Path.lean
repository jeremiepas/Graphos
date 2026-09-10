/-
Path.lean — "In-viewer shortest path between two nodes" requirement.

Client-side BFS over the loaded graph (design Decision 5), undirected as in CLI
`graphos path` (spec 05-path). `findPath` is validated by construction: a BFS
result is re-checked by `isPathBetween` before being returned, so soundness
(`findPath_sound`) holds unconditionally. Minimality is validated on fixtures
in Scenarios.lean. Also models the path overlay. Proves:
  * findPath_sound        — a returned path is a genuine walk from s to t
  * requestPath_nopath    — no path ⇒ the current view is untouched
  * clearPath_*           — clearing the path restores the prior view exactly
-/
import ViewerNav.State
import ViewerNav.Codec

namespace ViewerNav

/-- The loaded edge list. Communities and facets are deliberately NOT inputs:
path search is global by type — the cross-community scenario cannot fail. -/
abbrev Graph := List (NodeId × NodeId)

/-- Undirected adjacency. -/
def adjacent (g : Graph) (a b : NodeId) : Bool :=
  g.contains (a, b) || g.contains (b, a)

def neighbors (g : Graph) (a : NodeId) : List NodeId :=
  g.filterMap fun e =>
    if e.1 == a then some e.2
    else if e.2 == a then some e.1
    else none

/-- A non-empty sequence of pairwise-adjacent nodes. -/
def isWalk (g : Graph) : List NodeId → Bool
  | [] => false
  | [_] => true
  | a :: b :: rest => adjacent g a b && isWalk g (b :: rest)

def isPathBetween (g : Graph) (s t : NodeId) (p : List NodeId) : Bool :=
  p.head? == some s && p.getLast? == some t && isWalk g p

/-- Fuel-based BFS; the queue carries reversed paths, `visited` is extended at
enqueue time so no node is enqueued twice. -/
def bfsAux (g : Graph) (t : NodeId) :
    Nat → List (List NodeId) → List NodeId → Option (List NodeId)
  | 0, _, _ => none
  | _ + 1, [], _ => none
  | fuel + 1, path :: queue, visited =>
    match path with
    | [] => none
    | cur :: _ =>
      if cur == t then some path.reverse
      else
        let fresh := (neighbors g cur).filter (fun n => !(visited.contains n))
        bfsAux g t fuel (queue ++ fresh.map (· :: path)) (visited ++ fresh)

/-- Each node is enqueued at most once, so |nodes| + 1 ≤ 2·|edges| + 2 dequeues
suffice. -/
def shortestPathRaw (g : Graph) (s t : NodeId) : Option (List NodeId) :=
  bfsAux g t (2 * g.length + 2) [[s]] [s]

/-- Path lookup, validated by construction: the BFS result is re-checked before
being returned. -/
def findPath (g : Graph) (s t : NodeId) : Option (List NodeId) :=
  match shortestPathRaw g s t with
  | some p => if isPathBetween g s t p then some p else none
  | none => none

/-- Soundness: whatever `findPath` returns is a genuine undirected walk from
`s` to `t` in the loaded graph. -/
theorem findPath_sound (g : Graph) (s t : NodeId) (p : List NodeId)
    (h : findPath g s t = some p) : isPathBetween g s t p = true := by
  unfold findPath at h
  split at h
  next q _ =>
    split at h
    next hb => injection h with h; subst h; exact hb
    next => simp at h
  next => simp at h

-- ---------------------------------------------------------------------------
-- The path overlay on top of the viewer state
-- ---------------------------------------------------------------------------

/-- A path highlight is an overlay over the base view — never a mutation of it. -/
structure PathView where
  base : ViewerState
  overlay : Option (List NodeId) := none
deriving Repr, DecidableEq

def setPath (v : PathView) (p : List NodeId) : PathView := { v with overlay := some p }
def clearPath (v : PathView) : PathView := { v with overlay := none }

/-- Marking endpoints and searching: a found path becomes the overlay; no path
leaves the view untouched. -/
def requestPath (g : Graph) (v : PathView) (s t : NodeId) : PathView :=
  match findPath g s t with
  | some p => setPath v p
  | none => v

/-- Scenario "No path exists": the current view's highlighting is unchanged. -/
theorem requestPath_nopath (g : Graph) (v : PathView) (s t : NodeId)
    (h : findPath g s t = none) : requestPath g v s t = v := by
  simp [requestPath, h]

/-- Scenario "Clearing the path restores the view": same base position, facets
and hops — the overlay never leaks into the base state. -/
theorem clearPath_base (v : PathView) (p : List NodeId) :
    (clearPath (setPath v p)).base = v.base := rfl

theorem clearPath_overlay (v : PathView) (p : List NodeId) :
    (clearPath (setPath v p)).overlay = none := rfl

/-- Full restoration: if no path was highlighted before, highlight + clear is
the identity. -/
theorem clearPath_setPath (v : PathView) (p : List NodeId)
    (h : v.overlay = none) : clearPath (setPath v p) = v := by
  obtain ⟨base, overlay⟩ := v
  subst h
  rfl

end ViewerNav
