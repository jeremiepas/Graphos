/-
Paths.lean — coverage with path certificates.

"Requirement is implemented" is a positive claim carrying its witness: the
path from the requirement to a code node. `findPathTo` re-checks the BFS
output before returning it, so soundness holds by construction regardless of
the search implementation — the design's asymmetry (positive certifies,
negative warns) rests on exactly this.
-/
import SpecCheck.Basic
import SpecCheck.Topo

namespace SpecCheck

/-- A certificate that `p` is a chain from `s` to a node satisfying `isTgt`. -/
def isPathTo (step : NodeId → NodeId → Bool) (s : NodeId)
    (isTgt : NodeId → Bool) (p : List NodeId) : Bool :=
  match p with
  | [] => false
  | a :: rest =>
      (a == s) && isChain step (a :: rest) && isTgt (lastFrom a rest)

/-- Fuel-based BFS carrying reversed paths; visited grows at enqueue time. -/
def bfsAux (g : SpecGraph) (rels : List Rel) (isTgt : NodeId → Bool) :
    Nat → List (List NodeId) → List NodeId → Option (List NodeId)
  | 0, _, _ => none
  | _ + 1, [], _ => none
  | fuel + 1, path :: queue, visited =>
      match path with
      | [] => none
      | cur :: _ =>
          if isTgt cur then some path.reverse
          else
            let fresh := (succs g rels cur).filter (fun n => !visited.contains n)
            bfsAux g rels isTgt fuel
              (queue ++ fresh.map (· :: path)) (visited ++ fresh)

/-- Path search, validated by construction: the BFS result is re-checked by
`isPathTo` before being returned. -/
def findPathTo (g : SpecGraph) (rels : List Rel) (s : NodeId)
    (isTgt : NodeId → Bool) : Option (List NodeId) :=
  match bfsAux g rels isTgt (g.edges.length * 2 + 2) [[s]] [s] with
  | some p => if isPathTo (stepRel g rels) s isTgt p then some p else none
  | none => none

/-- Soundness: an implemented verdict's certificate is a genuine chain from
the requirement to a target node — by construction, independent of the BFS. -/
theorem findPathTo_certified (g : SpecGraph) (rels : List Rel) (s : NodeId)
    (isTgt : NodeId → Bool) (p : List NodeId)
    (h : findPathTo g rels s isTgt = some p) :
    isPathTo (stepRel g rels) s isTgt p = true := by
  unfold findPathTo at h
  split at h
  next q _ =>
    split at h
    next hcheck => injection h with h'; subst h'; exact hcheck
    next => simp at h
  next => simp at h

-- ---------------------------------------------------------------------------
-- Coverage checks over the certificate machinery
-- ---------------------------------------------------------------------------

/-- Relations coverage traverses (per spec-graph-checks). -/
def coverageRels : List Rel :=
  [Rel.satisfies, Rel.references, Rel.contains]

def isCodeNode (g : SpecGraph) (n : NodeId) : Bool :=
  g.nodes.any fun nd => nd.id == n && nd.kind == Kind.code

/-- Implemented = certificate exists; the certificate is the report entry. -/
def implementedWitness (g : SpecGraph) (r : NodeId) : Option (List NodeId) :=
  findPathTo g coverageRels r (isCodeNode g)

/-- Unimplemented requirements — the warning-severity finding (negative claim,
depends on extraction completeness; never gates by default). -/
def unimplemented (g : SpecGraph) : List NodeId :=
  (activeRequirements g).filter fun r => (implementedWitness g r).isNone

/-- An unimplemented finding and an implemented witness are mutually
exclusive by construction. -/
theorem unimplemented_no_witness (g : SpecGraph) (r : NodeId)
    (h : r ∈ unimplemented g) : implementedWitness g r = none := by
  unfold unimplemented at h
  have hp := (List.mem_filter.mp h).2
  cases hw : implementedWitness g r with
  | none => rfl
  | some p => rw [hw] at hp; simp at hp

end SpecCheck
