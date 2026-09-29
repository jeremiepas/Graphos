/-
Basic.lean — the spec graph: typed nodes (Requirement/Decision/Scenario/Code),
typed edges (the spec-artifact relation vocabulary), and the chain checker
shared by every certificate in the model.
-/

namespace SpecCheck

abbrev NodeId := String

inductive Kind where
  | requirement
  | decision
  | scenario
  | constraintNode
  | code
deriving Repr, DecidableEq

inductive Rel where
  | dependsOn
  | refines
  | conflictsWith
  | satisfies
  | supersedes
  | constrains
  | references
  | contains
deriving Repr, DecidableEq

structure SpecNode where
  id : NodeId
  kind : Kind
  active : Bool := true
deriving Repr, DecidableEq

structure SpecEdge where
  src : NodeId
  rel : Rel
  tgt : NodeId
deriving Repr, DecidableEq

structure SpecGraph where
  nodes : List SpecNode
  edges : List SpecEdge
deriving Repr

/-- Does an edge with one of the selected relations connect a to b? -/
def stepRel (g : SpecGraph) (rels : List Rel) (a b : NodeId) : Bool :=
  g.edges.any fun e => e.src == a && e.tgt == b && rels.contains e.rel

/-- Successors of a node over the selected relations. -/
def succs (g : SpecGraph) (rels : List Rel) (a : NodeId) : List NodeId :=
  g.edges.filterMap fun e =>
    if e.src == a && rels.contains e.rel then some e.tgt else none

def nodeIds (g : SpecGraph) : List NodeId :=
  g.nodes.map (·.id)

def activeRequirements (g : SpecGraph) : List NodeId :=
  g.nodes.filterMap fun n =>
    if n.kind == Kind.requirement && n.active then some n.id else none

def codeIds (g : SpecGraph) : List NodeId :=
  g.nodes.filterMap fun n => if n.kind == Kind.code then some n.id else none

-- ---------------------------------------------------------------------------
-- Chains: the walk checker every certificate reduces to
-- ---------------------------------------------------------------------------

/-- A non-empty sequence of nodes where each consecutive pair is connected by
`step`. This checker is the trusted core: cycle witnesses and path
certificates are both validated by it. -/
def isChain (step : NodeId → NodeId → Bool) : List NodeId → Bool
  | [] => false
  | [_] => true
  | a :: b :: rest => step a b && isChain step (b :: rest)

end SpecCheck
