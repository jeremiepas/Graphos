/-
Topo.lean — the crown theorem of the checker: a valid topological order
certifies acyclicity.

The gate never trusts the sort — it re-checks the emitted order with
`isTopoOrder` (linear time). This module proves that re-checker sufficient:
if every checked edge goes strictly forward in the order, then positions
strictly increase along any chain, so a closed chain would need
`pos n < pos n` — impossible. The sort implementation is certificate-checked
by construction (`checkAcyclic`), so its own correctness is never assumed.
-/
import SpecCheck.Basic

namespace SpecCheck

/-- Position of a node in the order (order.length if absent — totality keeps
the theorem free of membership side conditions). -/
def pos (order : List NodeId) (n : NodeId) : Nat :=
  match order with
  | [] => 0
  | x :: xs => if x == n then 0 else pos xs n + 1

/-- The last node of a nonempty chain `a :: rest`. -/
def lastFrom (a : NodeId) : List NodeId → NodeId
  | [] => a
  | x :: xs => lastFrom x xs

/-- Closed chain: length ≥ 2, chain, ends where it starts — the cycle witness
shape. -/
def isClosedChain (step : NodeId → NodeId → Bool) : List NodeId → Bool
  | a :: b :: rest => isChain step (a :: b :: rest) && (lastFrom b rest == a)
  | _ => false

/-- The re-checked half of the certificate: every edge of the selected
relations goes strictly forward in the order. -/
def respectsEdges (g : SpecGraph) (rels : List Rel) (order : List NodeId) : Bool :=
  g.edges.all fun e =>
    !rels.contains e.rel || decide (pos order e.src < pos order e.tgt)

/-- The full certificate check: covers all nodes and respects all edges. -/
def isTopoOrder (g : SpecGraph) (rels : List Rel) (order : List NodeId) : Bool :=
  (nodeIds g).all order.contains && respectsEdges g rels order


-- ---------------------------------------------------------------------------
-- Proof: respectsEdges ⇒ no closed chain
-- ---------------------------------------------------------------------------

/-- Bridge: a step of the chain is an edge, and respected edges advance the
position. -/
theorem step_pos_lt (g : SpecGraph) (rels : List Rel) (order : List NodeId)
    (hresp : respectsEdges g rels order = true)
    {a b : NodeId} (hstep : stepRel g rels a b = true) :
    pos order a < pos order b := by
  unfold stepRel at hstep
  unfold respectsEdges at hresp
  rcases List.any_eq_true.mp hstep with ⟨e, he_mem, he_pred⟩
  have hcond := List.all_eq_true.mp hresp e he_mem
  rcases Bool.and_eq_true_iff.mp he_pred with ⟨hst, hrel⟩
  rcases Bool.and_eq_true_iff.mp hst with ⟨hsrc, htgt⟩
  have hsrc' : e.src = a := by simpa using hsrc
  have htgt' : e.tgt = b := by simpa using htgt
  rw [hrel] at hcond
  simp at hcond
  rw [hsrc', htgt'] at hcond
  exact hcond

/-- Positions strictly increase along any nonempty continuation of a chain. -/
theorem chain_pos_lt (step : NodeId → NodeId → Bool) (order : List NodeId)
    (hmono : ∀ a b, step a b = true → pos order a < pos order b) :
    ∀ (rest : List NodeId) (b a : NodeId),
      isChain step (a :: b :: rest) = true →
      pos order a < pos order (lastFrom b rest) := by
  intro rest
  induction rest with
  | nil =>
    intro b a hchain
    rcases Bool.and_eq_true_iff.mp hchain with ⟨hab, _⟩
    exact hmono a b hab
  | cons c rest ih =>
    intro b a hchain
    rcases Bool.and_eq_true_iff.mp hchain with ⟨hab, htail⟩
    have h1 : pos order a < pos order b := hmono a b hab
    have h2 : pos order b < pos order (lastFrom c rest) := ih c b htail
    exact Nat.lt_trans h1 h2

/-- **The crown theorem**: if the order respects every edge, no closed chain
exists over those relations — a valid topological order certifies acyclicity. -/
theorem no_closed_chain_of_respects (g : SpecGraph) (rels : List Rel)
    (order : List NodeId) (hresp : respectsEdges g rels order = true)
    (c : List NodeId) : isClosedChain (stepRel g rels) c ≠ true := by
  intro hclosed
  match c with
  | [] => simp [isClosedChain] at hclosed
  | [_] => simp [isClosedChain] at hclosed
  | a :: b :: rest =>
    unfold isClosedChain at hclosed
    rcases Bool.and_eq_true_iff.mp hclosed with ⟨hchain, hlast⟩
    have hlast' : lastFrom b rest = a := by simpa using hlast
    have hlt : pos order a < pos order (lastFrom b rest) :=
      chain_pos_lt (stepRel g rels) order
        (fun x y h => step_pos_lt g rels order hresp h) rest b a hchain
    rw [hlast'] at hlt
    exact Nat.lt_irrefl _ hlt

/-- Corollary in certificate terms: an order passing the full `isTopoOrder`
re-check certifies acyclicity. -/
theorem topo_certifies_acyclic (g : SpecGraph) (rels : List Rel)
    (order : List NodeId) (htopo : isTopoOrder g rels order = true)
    (c : List NodeId) : isClosedChain (stepRel g rels) c ≠ true :=
  no_closed_chain_of_respects g rels order
    (Bool.and_eq_true_iff.mp htopo).right c


-- ---------------------------------------------------------------------------
-- The sort: certificate-checked by construction
-- ---------------------------------------------------------------------------

/-- A node with no incoming selected edge from the remaining set. -/
def pickSource (g : SpecGraph) (rels : List Rel) (remaining : List NodeId) :
    Option NodeId :=
  remaining.find? fun n => !(remaining.any fun m => stepRel g rels m n)

def topoSortAux (g : SpecGraph) (rels : List Rel) :
    Nat → List NodeId → List NodeId → Option (List NodeId)
  | 0, remaining, acc =>
      if remaining.isEmpty then some acc.reverse else none
  | fuel + 1, remaining, acc =>
      if remaining.isEmpty then some acc.reverse
      else
        match pickSource g rels remaining with
        | none => none
        | some n =>
            topoSortAux g rels fuel (remaining.filter (· != n)) (n :: acc)

def topoSort (g : SpecGraph) (rels : List Rel) : Option (List NodeId) :=
  topoSortAux g rels (g.nodes.length + 1) (nodeIds g) []

/-- The verdict the gate consumes: the sorted order, accepted only if the
re-checker validates it. Soundness is by construction — whatever the sort
does, an accepted order satisfies `isTopoOrder`, hence `topo_certifies_acyclic`
applies. -/
def checkAcyclic (g : SpecGraph) (rels : List Rel) : Option (List NodeId) :=
  match topoSort g rels with
  | some order => if isTopoOrder g rels order then some order else none
  | none => none

theorem checkAcyclic_sound (g : SpecGraph) (rels : List Rel)
    (order : List NodeId) (h : checkAcyclic g rels = some order) :
    isTopoOrder g rels order = true := by
  unfold checkAcyclic at h
  split at h
  next found _ =>
    split at h
    next hcheck => injection h with h'; subst h'; exact hcheck
    next => simp at h
  next => simp at h

/-- End-to-end: an accepted verdict rules out every closed chain. -/
theorem checkAcyclic_no_cycle (g : SpecGraph) (rels : List Rel)
    (order : List NodeId) (h : checkAcyclic g rels = some order)
    (c : List NodeId) : isClosedChain (stepRel g rels) c ≠ true :=
  topo_certifies_acyclic g rels order (checkAcyclic_sound g rels order h) c

end SpecCheck
