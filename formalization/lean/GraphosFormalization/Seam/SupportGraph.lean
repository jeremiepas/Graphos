/-
  Graphos formalization — the **M → Graph → G_s seam** (Phase 2 → Phase 1).

  Requirements: `docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`
  §1.1 (the verified pipeline), §1.2 (notation), §1.3 (the one merge correction);
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §1.1 (`G_s`), §2 (INV-Q0–Q5); `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §2 (the colimit object `M`). Issue: AVI-1308.

  Pinned Haskell surface:

  | Pipeline stage | Surface |
  | --- | --- |
  | merged extraction `M` | `mergeExtractions` `Domain/Graph/Core.hs:129-136`; build fold `UseCase/Build.hs:11-13` |
  | `buildGraph gDirected M` | `Domain/Graph/Core.hs:96-122` |
  | `Graph` record | `Domain/Graph/Core.hs:47-57` (`gEdges :: Map (NodeId, NodeId) Edge`) |
  | `neighbors` | `Domain/Graph/Query.hs:37-41` |
  | `degree` | `Domain/Graph/Query.hs:44` |
  | `Q` on `G_s` | `Domain/Community.hs:92-107,151-167` |

  ## What this module pins, and why it is the seam

  Phase 2 (`Ext/*.lean`) ends at the colimit object `M : Extraction`. Phase 1
  (`Phase1/*.lean`) begins at the simple support graph `G_s` and defines `Q`
  over it. Between them sit two *Haskell* functions that neither requirement
  layer named formally:

  ```
  M ──buildGraph gDirected──▶ Graph ──neighbors──▶ G_s = (V, A) ──Q──▶ Float
  ```

  The seam is the composite. It is pinned here as a function of `M` alone:

  * `buildGraph` copies `extNodes M` into `gNodes` unchanged (`Core.hs:99`) and
    keeps only edges whose **both** endpoints are node keys (`Core.hs:100-103`,
    the dangling drop).
  * It keys `gEdges` by the **endpoint pair** (`Core.hs:104`) — not by `EdgeId`
    — so two distinct `EdgeId`s with the same `(source,target)` already collapse
    to one `gEdges` entry, and `Map.fromList` keeps the **last** one in
    `Map.elems` order.
  * `neighbors` (`Query.hs:37-41`) returns a `Set NodeId`: forward targets for
    `gDirected g = True`, forward ∪ backward targets otherwise. Adjacency is
    built with `Map.fromListWith Set.union` (`Core.hs:105-113`), so every
    remaining multi-edge (same endpoints, different `EdgeId`) collapses too.
    `gAdjBack` itself is built as `rev <> fwdAdj` for undirected graphs;
    `Data.Map`'s `<>` is left-biased, so the stored `gAdjBack` need not equal
    `fwd ∪ rev`, but `neighbors` unions both maps, and its membership is
    exactly `fwd ∨ rev`. The seam is therefore pinned at the `neighbors`
    level — the level `degree` and `computeCommunityStats` actually read.
  * `G_s` is what Phase 1's `SupportGraph` models: its edge set is the
    symmetric closure of the **valid endpoint pairs**. Everything else Phase 2
    carries — `EdgeId`, `edgeRelation`, `edgeWeight`, `edgeConfidence`, which
    view won a conflicting merge, and the map's iteration order — is
    unobservable to `Q` (`buildGraph_supportAdj_eq_true_iff` names none of it).

  ## Modelling choices

  1. **`BuiltGraph` keeps only what `neighbors`/`degree` read.** The Haskell
     `Graph` also carries `gEdges`, `gHash`, `gEmbeddings`, …; none is read by
     `Query.neighbors` or `Community.computeCommunityStats`, so they are not
     modelled. The endpoint-pair list `edgePairs` is kept as evidence for the
     drop/collapse lemmas, not as an input to the adjacency (which is already a
     `Set`).
  2. **The edge-key enumeration is explicit.** `Map.elems (extEdges M)` needs
     the map's key list; the extensional view model of `Ext.Basic` has no
     intrinsic support, so `buildGraph` takes an explicit `eks : List EK`
     standing for the `Map`'s key enumeration (`Ext/Complexity.lean` makes the
     same choice for `|keys Eᵢ|`). Every seam theorem that must hold for the
     shipped code is stated for an arbitrary `eks`.
  3. **Node *presence*, not node payload.** `Q` reads only `|V|`, degrees and
     adjacencies, so the seam carries `nodes : ExtView V N` and consults only
     its `isSome` projection. `N` stays arbitrary, so the seam is agnostic to
     `Node`'s fields.
  4. **The dangling drop happens once, after the colimit.** `buildGraph` is
     called on `M = foldr ⊕ emptyExtraction` (`Build.hs:11-13`), never per view.
     `dangling_resurrection` below proves this is *not* the same as merging
     per-view support graphs: an edge dangling inside its own view is dropped
     there, yet survives in `G_s(M)` once its target arrives from another view.
     `G_s(M)` is a function of `M`, not of the family's per-view `G_s`s.
  5. **`SupportGraph.irrefl` is a seam obligation, not a theorem.** Phase 1's
     `SupportGraph` is simple (`adj v v = false`), but `buildGraph` does **not**
     drop self-edges. The seam therefore carries `NoSelfLoops` as an explicit
     hypothesis (`buildGraph_supportAdj_irrefl`). Nothing in the extraction lane
     forbids a self-edge, so this is a genuine precondition the Phase 1 layer
     already assumes; a Haskell-side guard is a separate dev question, not a
     Lean gap.
  6. **The `Graph` record cited as `Types/Graph.hs:83` is `Core.hs:47`.** In the
     requirements that line reference lands in the `Extraction` accessor block
     (`extNodes`, `Types/Graph.hs:89`). The `LabeledGraph` record — which keys
     `gEdges` by `EdgeId` — is `Types/Graph.hs:106-110`; the pipeline's runtime
     record is `Domain/Graph/Core.hs:47-57`, and its `gEdges` is keyed by
     `(NodeId, NodeId)`. That endpoint key is what makes the multi-edge
     collapse immediate, so the seam is pinned against `Core.hs:47`.

  ## The seam theorem

  `buildGraph_supportAdj_merge_swap` is the payload of the seam: if two views
  agree on edge **endpoints** at every shared key — which the shipped `EdgeId`
  encoding guarantees, since `EdgeId` is `"<src>-><tgt>:<relation"`
  (`Convert.hs:165`) — then `G_s(A ⊕ B) = G_s(B ⊕ A)` **even though**
  `A ⊕ B ≠ B ⊕ A` (`Ext/Lax.lean`, `merge_ne_merge_swap`). Phase 2's
  first/old-wins precedence therefore cannot leak into Phase 1's audited `Q`.
  `ac4_seam_modularity_eq` instantiates that on the AC-4 conflicting-`EdgeId`
  fixture and closes the loop against `Phase1.Modularity.modularity`.
-/
import GraphosFormalization.Ext.Basic
import GraphosFormalization.Ext.Colimit
import GraphosFormalization.Ext.Lax
import GraphosFormalization.Phase1.Modularity

namespace GraphosFormalization

namespace Seam

/-! ## §1. `buildGraph`'s input and output objects -/

/-- One extraction edge as `buildGraph` sees it: the `EdgeId` components
`edgeSource`/`edgeTarget` (`Types/Graph.hs:73`) plus the payload the graph does
**not** read for adjacency (`edgeRelation`/`edgeWeight`/`edgeConfidence`). The
payload is one opaque `P` precisely because the seam proves it is ignored. -/
structure SeamEdge (V P : Type) where
  /-- `edgeSource`. -/
  src : V
  /-- `edgeTarget`. -/
  tgt : V
  /-- Everything else the edge carries (`edgeRelation`, `edgeWeight`,
  `edgeConfidence`, …): read by neither `buildGraph`'s adjacency nor
  `neighbors`/`degree`. -/
  payload : P
  deriving DecidableEq, Repr

/-- The part of the Haskell `Graph` (`Core.hs:47-57`) that `neighbors` and
`degree` (`Query.hs:37-44`) actually read, after `buildGraph`
(`Core.hs:96-122`):

* `nodes` — `gNodes`, copied verbatim from `extNodes M`;
* `adjFwd`/`adjBack` — the two adjacency maps as membership tests (a `Set`
  lookup: one `Bool` per ordered pair, so duplicate neighbours collapse);
* `directed` — `gDirected`;
* `edgePairs` — the endpoint pairs (`edgeSource, edgeTarget`) of the *valid*
  edges, in `Map.elems` order: `gEdges`'s key set with multiplicity, kept as
  evidence for the drop/collapse lemmas.

`gHash`, `gCompositions`, `gEmbeddings*` are deliberately absent: no Phase 1
consumer reads them. -/
structure BuiltGraph (V N P : Type) where
  /-- `gNodes` (`Core.hs:99`): `buildGraph` copies `extNodes M` unchanged. -/
  nodes : ExtView V N
  /-- Endpoint pairs of the valid edges, in `Map.elems (extEdges M)` order.
  This is `gEdges`'s key set (`Core.hs:104`), with multiplicity; the adjacency
  below is its `Set`-collapse. -/
  edgePairs : List (V × V)
  /-- `gAdjFwd` membership (`Core.hs:105-106`). -/
  adjFwd : V → V → Bool
  /-- `gAdjBack` membership (`Core.hs:107-113`). -/
  adjBack : V → V → Bool
  /-- `gDirected` (`Core.hs:52`). -/
  directed : Bool

namespace BuiltGraph

variable {V N P : Type}

/-- `neighbors` (`Query.hs:37-41`) as a membership test: forward targets when
the graph is directed, forward ∪ backward otherwise. -/
def neighbors (g : BuiltGraph V N P) (u v : V) : Bool :=
  if g.directed then g.adjFwd u v else g.adjFwd u v || g.adjBack u v

/-- The undirected **support adjacency** `A` of `G_s = (V, A)`: the symmetric
closure of the valid incidence relation. For an undirected build
(`gDirected = False`, the Leiden lane) `buildGraph_supportAdj_eq_neighbors`
shows this is exactly `neighbors`, and `buildGraph_supportAdj_symm` shows it is
symmetric, so it is a legitimate `SupportGraph.adj`. -/
def supportAdj (g : BuiltGraph V N P) (u v : V) : Bool :=
  g.adjFwd u v || g.adjBack u v

/-- For a directed build, `neighbors` is forward-only (`Query.hs:38-39`). -/
theorem neighbors_of_directed (g : BuiltGraph V N P) (u v : V) (h : g.directed = true) :
    g.neighbors u v = g.adjFwd u v := by
  simp [neighbors, h]

/-- For an undirected build, `neighbors` is the support adjacency
(`Query.hs:40-41`): `fwd ∪ (rev ∪ fwd) = fwd ∪ rev`. -/
theorem neighbors_of_undirected (g : BuiltGraph V N P) (u v : V)
    (h : g.directed = false) : g.neighbors u v = g.supportAdj u v := by
  simp [neighbors, supportAdj, h]

end BuiltGraph

/-- `Bool` equality from truth equivalence (core Lean has no `Bool.eq_iff_iff`
in the form the seam needs). -/
theorem bool_eq_of_iff {a b : Bool} (h : (a = true) ↔ (b = true)) : a = b := by
  cases a <;> cases b <;> simp_all

/-! ## §2. `buildGraph` — the dangling drop and the adjacency -/

/-- `buildGraph`'s validity test (`Core.hs:100-103`): an edge survives iff
**both** endpoints are node keys (`Map.member (edgeSource e) nodes &&
Map.member (edgeTarget e) nodes`). -/
def validEdge {V EK N P : Type} (M : Extraction V EK N (SeamEdge V P))
    (e : SeamEdge V P) : Bool :=
  (M.extNodes e.src).isSome && (M.extNodes e.tgt).isSome

/-- The valid edge list, i.e. Haskell's `validEdges` (`Core.hs:101-103`):
`Map.elems (extEdges M)` filtered by the dangling drop. `eks` stands for the
`Map`'s key enumeration. -/
def validEdges {V EK N P : Type} (M : Extraction V EK N (SeamEdge V P))
    (eks : List EK) : List (SeamEdge V P) :=
  (eks.filterMap M.extEdges).filter (validEdge M)

/-- Membership in `validEdges`, in the `Map`'s own terms: some enumerated key
carries the edge, and the edge passes the dangling drop. -/
theorem mem_validEdges_iff {V EK N P : Type} (M : Extraction V EK N (SeamEdge V P))
    (eks : List EK) (e : SeamEdge V P) :
    e ∈ validEdges M eks ↔
      (∃ k ∈ eks, M.extEdges k = some e) ∧ validEdge M e = true := by
  simp [validEdges, List.mem_filterMap]

/-- The dangling drop as an equation on the validity test. -/
theorem validEdge_eq_true_iff {V EK N P : Type} (M : Extraction V EK N (SeamEdge V P))
    (e : SeamEdge V P) :
    validEdge M e = true ↔
      (M.extNodes e.src).isSome = true ∧ (M.extNodes e.tgt).isSome = true := by
  simp [validEdge]

/-- The drop itself: an edge with a missing endpoint is never in `validEdges`,
whatever the key enumeration. -/
theorem not_mem_validEdges_of_dangling {V EK N P : Type}
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) (e : SeamEdge V P)
    (h : (M.extNodes e.src).isSome = false ∨ (M.extNodes e.tgt).isSome = false) :
    e ∉ validEdges M eks := by
  intro he
  obtain ⟨-, hv⟩ := (mem_validEdges_iff M eks e).mp he
  rw [validEdge_eq_true_iff] at hv
  rcases h with h | h
  · rw [h] at hv; exact Bool.false_ne_true hv.1
  · rw [h] at hv; exact Bool.false_ne_true hv.2

section Build

variable {V EK N P : Type} [DecidableEq V]

/-- `buildGraph` (`Core.hs:96-122`) at the seam's observables.

Faithful reading:

* `nodes := M.extNodes` — `Core.hs:99`, no filtering;
* `edgePairs := map (fun e => (src e, tgt e)) validEdges` — `gEdges` is keyed by
  the endpoint pair (`Core.hs:104`);
* `adjFwd u v := ∃ e ∈ validEdges, src e = u ∧ tgt e = v`
  (`Map.fromListWith Set.union`, `Core.hs:105-106`);
* `adjBack u v := ∃ e ∈ validEdges, tgt e = u ∧ src e = v` (`Core.hs:107-113`;
  the extra `<> fwdAdj` of the undirected branch is absorbed by `neighbors`,
  see `neighbors_of_undirected`). -/
def buildGraph (d : Bool) (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) :
    BuiltGraph V N P :=
  let es := validEdges M eks
  { nodes := M.extNodes
  , edgePairs := es.map (fun e => (e.src, e.tgt))
  , adjFwd := fun u v => es.any (fun e => decide (e.src = u ∧ e.tgt = v))
  , adjBack := fun u v => es.any (fun e => decide (e.tgt = u ∧ e.src = v))
  , directed := d }

@[simp] theorem buildGraph_nodes (d : Bool) (M : Extraction V EK N (SeamEdge V P))
    (eks : List EK) : (buildGraph d M eks).nodes = M.extNodes := rfl

@[simp] theorem buildGraph_directed (d : Bool) (M : Extraction V EK N (SeamEdge V P))
    (eks : List EK) : (buildGraph d M eks).directed = d := rfl

/-- `adjFwd` membership is "some valid edge has these endpoints". -/
theorem buildGraph_adjFwd_eq_true_iff (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) (u v : V) :
    (buildGraph d M eks).adjFwd u v = true ↔
      ∃ e ∈ validEdges M eks, e.src = u ∧ e.tgt = v := by
  simp [buildGraph, List.any_eq_true, decide_eq_true_eq]

/-- `adjBack` membership is the reverse incidence. -/
theorem buildGraph_adjBack_eq_true_iff (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) (u v : V) :
    (buildGraph d M eks).adjBack u v = true ↔
      ∃ e ∈ validEdges M eks, e.tgt = u ∧ e.src = v := by
  simp [buildGraph, List.any_eq_true, decide_eq_true_eq]

/-- **The support relation of `G_s`.** `supportAdj u v` holds iff some valid
edge joins `u` and `v` in either orientation. This is the unique place Phase 2
data enters Phase 1, and the statement mentions **no** `EdgeId`, relation,
weight, confidence, merge winner or map order. -/
theorem buildGraph_supportAdj_eq_true_iff (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) (u v : V) :
    (buildGraph d M eks).supportAdj u v = true ↔
      ∃ e ∈ validEdges M eks, (e.src = u ∧ e.tgt = v) ∨ (e.src = v ∧ e.tgt = u) := by
  rw [BuiltGraph.supportAdj, Bool.or_eq_true, buildGraph_adjFwd_eq_true_iff,
    buildGraph_adjBack_eq_true_iff]
  constructor
  · rintro (⟨e, he, h1, h2⟩ | ⟨e, he, h1, h2⟩)
    · exact ⟨e, he, Or.inl ⟨h1, h2⟩⟩
    · exact ⟨e, he, Or.inr ⟨h2, h1⟩⟩
  · rintro ⟨e, he, h | h⟩
    · exact Or.inl ⟨e, he, h.1, h.2⟩
    · exact Or.inr ⟨e, he, h.2, h.1⟩

/-- The two `any`-folds differ only in the order of the conjunction. -/
theorem any_endpoints_comm (es : List (SeamEdge V P)) (u v : V) :
    es.any (fun e => decide (e.src = u ∧ e.tgt = v)) =
      es.any (fun e => decide (e.tgt = v ∧ e.src = u)) := by
  induction es with
  | nil => rfl
  | cons e es ih =>
      simp only [List.any_cons]
      rw [ih]
      have hswap : decide (e.src = u ∧ e.tgt = v) = decide (e.tgt = v ∧ e.src = u) := by
        by_cases h1 : e.src = u <;> by_cases h2 : e.tgt = v <;> simp [h1, h2]
      rw [hswap]

/-- **`G_s` is undirected.** The support adjacency is symmetric for every
`buildGraph` output, so it can be handed to `SupportGraph.symm` — and `Q`'s
undirected range bound (AVI-618 §3) applies to the pipeline's `G_s`. -/
theorem buildGraph_supportAdj_symm (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) (u v : V) :
    (buildGraph d M eks).supportAdj u v = (buildGraph d M eks).supportAdj v u := by
  simp only [BuiltGraph.supportAdj, buildGraph]
  rw [any_endpoints_comm (validEdges M eks) u v,
    ← any_endpoints_comm (validEdges M eks) v u, Bool.or_comm]

/-- **`G_s` is simple up to self-edges.** `buildGraph` does not filter
`src = tgt`, so simplicity is an explicit precondition `NoSelfLoops`: no valid
edge is a self-edge. Under it, the support adjacency is irreflexive, as
`SupportGraph` requires. -/
def NoSelfLoops (M : Extraction V EK N (SeamEdge V P)) (eks : List EK) : Prop :=
  ∀ e ∈ validEdges M eks, e.src ≠ e.tgt

theorem buildGraph_supportAdj_irrefl (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK)
    (h : NoSelfLoops M eks) (v : V) :
    (buildGraph d M eks).supportAdj v v = false := by
  cases hb : (buildGraph d M eks).supportAdj v v with
  | false => rfl
  | true =>
      obtain ⟨e, he, hcase⟩ :=
        (buildGraph_supportAdj_eq_true_iff d M eks v v).mp hb
      rcases hcase with ⟨h1, h2⟩ | ⟨h1, h2⟩
      · exact absurd (h1.trans h2.symm) (h e he)
      · exact absurd (h1.trans h2.symm) (h e he)

/-! ### Packaging `G_s` as Phase 1's object -/

/-- `G_s = (V, A)` as Phase 1's `SupportGraph` (`AVI-618` §1.1): an exhaustive
duplicate-free vertex enumeration, the symmetric support adjacency, and
irreflexivity. `symm`/`irrefl` are hypotheses here because a general
`BuiltGraph` need not have been built by `buildGraph`; the `buildGraph`
constructor below discharges them. -/
def BuiltGraph.toSupportGraph (g : BuiltGraph V N P) (vertices : List V)
    (hnodup : vertices.Nodup) (hcomplete : ∀ v, v ∈ vertices)
    (hsymm : ∀ u v, g.supportAdj u v = g.supportAdj v u)
    (hirrefl : ∀ v, g.supportAdj v v = false) : SupportGraph V :=
  { vertices := vertices
  , nodup := hnodup
  , complete := hcomplete
  , adj := g.supportAdj
  , symm := hsymm
  , irrefl := hirrefl }

omit [DecidableEq V] in
@[simp] theorem BuiltGraph.toSupportGraph_adj (g : BuiltGraph V N P)
    (vertices : List V) (hnodup : vertices.Nodup) (hcomplete : ∀ v, v ∈ vertices)
    (hsymm : ∀ u v, g.supportAdj u v = g.supportAdj v u)
    (hirrefl : ∀ v, g.supportAdj v v = false) :
    (g.toSupportGraph vertices hnodup hcomplete hsymm hirrefl).adj = g.supportAdj := rfl

/-- **The seam, concretely**: `G_s` of the graph that `buildGraph` builds from
`M`, packaged as the Phase 1 object that `modularity` consumes. The `eks`,
`vertices`, `nodup` and `complete` arguments are the two enumerations the
Haskell maps supply (`Map.elems`); `NoSelfLoops` is the simplicity precondition
of choice 5 in the header. -/
def buildGraph_toSupportGraph (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK)
    (vertices : List V) (hnodup : vertices.Nodup) (hcomplete : ∀ v, v ∈ vertices)
    (hloop : NoSelfLoops M eks) : SupportGraph V :=
  (buildGraph d M eks).toSupportGraph vertices hnodup hcomplete
    (buildGraph_supportAdj_symm d M eks)
    (buildGraph_supportAdj_irrefl d M eks hloop)

@[simp] theorem buildGraph_toSupportGraph_adj (d : Bool)
    (M : Extraction V EK N (SeamEdge V P)) (eks : List EK)
    (vertices : List V) (hnodup : vertices.Nodup) (hcomplete : ∀ v, v ∈ vertices)
    (hloop : NoSelfLoops M eks) :
    (buildGraph_toSupportGraph d M eks vertices hnodup hcomplete hloop).adj =
      (buildGraph d M eks).supportAdj := rfl

end Build

/-! ## §3. The seam under the colimit: `G_s(M)` vs `G_s(Eᵢ)` -/

namespace Merge

variable {V EK N P : Type}

/-- Node presence is monotone in the left operand of `⊕`: an embedding cannot
remove a node key (`Map.union` keeps the first operand's keys). -/
theorem isSome_extNodes_merge_left {A B : Extraction V EK N (SeamEdge V P)} {x : V}
    (h : (A.extNodes x).isSome = true) :
    ((mergeExtractions A B).extNodes x).isSome = true := by
  cases hA : A.extNodes x with
  | none => simp [hA] at h
  | some v => simp [mergeExtractions, ExtView.union, hA]

/-- ... and in the right operand. -/
theorem isSome_extNodes_merge_right {A B : Extraction V EK N (SeamEdge V P)} {x : V}
    (h : (B.extNodes x).isSome = true) :
    ((mergeExtractions A B).extNodes x).isSome = true := by
  cases hA : A.extNodes x with
  | some v => simp [mergeExtractions, ExtView.union, hA]
  | none =>
      cases hB : B.extNodes x with
      | none => simp [hB] at h
      | some w => simp [mergeExtractions, ExtView.union, hA, hB]

/-- **Node presence is merge-order independent.** `A ⊕ B` and `B ⊕ A` may
carry different `Node` payloads at a shared key, but they have exactly the same
node *keys*, because `Map.union` is a union of domains. Validity — and hence
`G_s` — only sees presence. -/
theorem isSome_extNodes_merge_swap (A B : Extraction V EK N (SeamEdge V P)) (x : V) :
    ((mergeExtractions A B).extNodes x).isSome =
      ((mergeExtractions B A).extNodes x).isSome := by
  cases hA : A.extNodes x with
  | some v =>
      cases hB : B.extNodes x with
      | some w => simp [mergeExtractions, ExtView.union, hA, hB]
      | none => simp [mergeExtractions, ExtView.union, hA, hB]
  | none =>
      cases hB : B.extNodes x with
      | some w => simp [mergeExtractions, ExtView.union, hA, hB]
      | none => simp [mergeExtractions, ExtView.union, hA, hB]

/-- The dangling-drop test is invariant under swapping the merge operands. -/
theorem validEdge_merge_swap (A B : Extraction V EK N (SeamEdge V P))
    (e : SeamEdge V P) :
    validEdge (mergeExtractions A B) e = validEdge (mergeExtractions B A) e := by
  simp only [validEdge, isSome_extNodes_merge_swap]

/-- Validity is monotone in the left operand: merging cannot invalidate an
edge that was valid in the first view, since it cannot remove node keys. -/
theorem validEdge_merge_left {A B : Extraction V EK N (SeamEdge V P)} {e : SeamEdge V P}
    (h : validEdge A e = true) : validEdge (mergeExtractions A B) e = true := by
  rw [validEdge_eq_true_iff] at h ⊢
  exact ⟨isSome_extNodes_merge_left h.1, isSome_extNodes_merge_left h.2⟩

/-! ### Endpoint correspondence across merge orders -/

/-- Lookup-level endpoint correspondence: at any key, the winner in `B ⊕ A` is
endpoint-equal to the winner in `A ⊕ B`. The payload may differ (that is the
`EdgeId` conflict), the endpoints may not (`EdgeId` encodes `(source, target)`). -/
theorem extEdges_merge_swap_endpoints {A B : Extraction V EK N (SeamEdge V P)}
    (hE : ∀ (k : EK) (eA eB : SeamEdge V P),
      A.extEdges k = some eA → B.extEdges k = some eB →
        eA.src = eB.src ∧ eA.tgt = eB.tgt)
    (k : EK) (e : SeamEdge V P)
    (hk : (mergeExtractions A B).extEdges k = some e) :
    ∃ e', (mergeExtractions B A).extEdges k = some e' ∧
      e'.src = e.src ∧ e'.tgt = e.tgt := by
  cases hA : A.extEdges k with
  | none =>
      have hB : B.extEdges k = some e := by
        simpa [mergeExtractions, ExtView.union, hA] using hk
      refine ⟨e, ?_, rfl, rfl⟩
      simp [mergeExtractions, ExtView.union, hA, hB]
  | some eA =>
      have hAe : eA = e := by
        have h : (some eA : Option (SeamEdge V P)) = some e := by
          simpa [mergeExtractions, ExtView.union, hA] using hk
        exact Option.some.inj h
      cases hB : B.extEdges k with
      | none =>
          refine ⟨eA, ?_, by rw [hAe], by rw [hAe]⟩
          simp [mergeExtractions, ExtView.union, hA, hB]
      | some eB =>
          obtain ⟨hs, ht⟩ := hE k eA eB hA hB
          refine ⟨eB, ?_, ?_, ?_⟩
          · simp [mergeExtractions, ExtView.union, hB]
          · calc eB.src = eA.src := hs.symm
              _ = e.src := by rw [hAe]
          · calc eB.tgt = eA.tgt := ht.symm
              _ = e.tgt := by rw [hAe]

/-- **Every valid edge of `A ⊕ B` has an endpoint-equal valid edge in
`B ⊕ A`.** This is the load-bearing lemma of the seam: the two merge orders can
only differ in data `G_s` does not read. -/
theorem exists_validEdge_swap_of_mem {A B : Extraction V EK N (SeamEdge V P)}
    {eks : List EK} {e : SeamEdge V P}
    (hE : ∀ (k : EK) (eA eB : SeamEdge V P),
      A.extEdges k = some eA → B.extEdges k = some eB →
        eA.src = eB.src ∧ eA.tgt = eB.tgt)
    (he : e ∈ validEdges (mergeExtractions A B) eks) :
    ∃ e' ∈ validEdges (mergeExtractions B A) eks, e'.src = e.src ∧ e'.tgt = e.tgt := by
  obtain ⟨⟨k, hk, hkEq⟩, hvalid⟩ := (mem_validEdges_iff _ _ _).mp he
  obtain ⟨e', hkEq', hs, ht⟩ := extEdges_merge_swap_endpoints hE k e hkEq
  have hve' : validEdge (mergeExtractions A B) e' = true := by
    rw [validEdge_eq_true_iff] at hvalid ⊢
    exact ⟨by rw [hs]; exact hvalid.1, by rw [ht]; exact hvalid.2⟩
  have hv' : validEdge (mergeExtractions B A) e' = true := by
    rw [validEdge_merge_swap (A := B) (B := A)]
    exact hve'
  exact ⟨e', (mem_validEdges_iff _ _ _).mpr ⟨⟨k, hk, hkEq'⟩, hv'⟩, hs, ht⟩

section BuildLevel

variable [DecidableEq V]

/-- The support-adjacency transfer of `exists_validEdge_swap_of_mem`. -/
theorem supportAdj_merge_swap_of_endpoints (d : Bool)
    {A B : Extraction V EK N (SeamEdge V P)} {eks : List EK} {u v : V}
    (hE : ∀ (k : EK) (eA eB : SeamEdge V P),
      A.extEdges k = some eA → B.extEdges k = some eB →
        eA.src = eB.src ∧ eA.tgt = eB.tgt)
    (h : (buildGraph d (mergeExtractions A B) eks).supportAdj u v = true) :
    (buildGraph d (mergeExtractions B A) eks).supportAdj u v = true := by
  obtain ⟨e, he, hcase⟩ :=
    (buildGraph_supportAdj_eq_true_iff d (mergeExtractions A B) eks u v).mp h
  obtain ⟨e', he', hs, ht⟩ := exists_validEdge_swap_of_mem hE he
  refine (buildGraph_supportAdj_eq_true_iff d (mergeExtractions B A) eks u v).mpr
    ⟨e', he', ?_⟩
  rcases hcase with ⟨h1, h2⟩ | ⟨h1, h2⟩
  · exact Or.inl ⟨by rw [hs, h1], by rw [ht, h2]⟩
  · exact Or.inr ⟨by rw [hs, h1], by rw [ht, h2]⟩

/-- **The seam theorem: `G_s` cannot see the merge order.**

If the two views agree on edge endpoints at every shared key — guaranteed by the
shipped `EdgeId` encoding, which embeds `(source,target,relation)` in the key
(`Convert.hs:165`) — then the two merge orders induce the **same support
adjacency**, even though the merged extractions themselves differ
(`Ext/Lax.lean:merge_ne_merge_swap`). Phase 2's first/old-wins precedence is
therefore invisible to Phase 1's `Q`. -/
theorem buildGraph_supportAdj_merge_swap (d : Bool)
    (A B : Extraction V EK N (SeamEdge V P)) (eks : List EK)
    (hE : ∀ (k : EK) (eA eB : SeamEdge V P),
      A.extEdges k = some eA → B.extEdges k = some eB →
        eA.src = eB.src ∧ eA.tgt = eB.tgt)
    (u v : V) :
    (buildGraph d (mergeExtractions A B) eks).supportAdj u v =
      (buildGraph d (mergeExtractions B A) eks).supportAdj u v := by
  apply bool_eq_of_iff
  constructor
  · exact supportAdj_merge_swap_of_endpoints d hE
  · exact supportAdj_merge_swap_of_endpoints d
      (A := B) (B := A)
      (fun k eB eA hB hA => by
        obtain ⟨hs, ht⟩ := hE k eA eB hA hB
        exact ⟨hs.symm, ht.symm⟩)

/-- **Per-view containment**: `G_s(A) ⊆ G_s(A ⊕ B)` on the same key
enumeration. A view's support edges survive the colimit — the colimit adds
node keys and edges, never removes them. -/
theorem buildGraph_supportAdj_merge_left (d : Bool)
    (A B : Extraction V EK N (SeamEdge V P)) (eks : List EK) (u v : V)
    (h : (buildGraph d A eks).supportAdj u v = true) :
    (buildGraph d (mergeExtractions A B) eks).supportAdj u v = true := by
  obtain ⟨e, he, hcase⟩ :=
    (buildGraph_supportAdj_eq_true_iff d A eks u v).mp h
  obtain ⟨⟨k, hk, hkEq⟩, hvalid⟩ := (mem_validEdges_iff _ _ _).mp he
  have hkEq' : (mergeExtractions A B).extEdges k = some e := by
    simp only [mergeExtractions_extEdges]
    rw [ExtView.union_apply_some hkEq]
  have hv' : validEdge (mergeExtractions A B) e = true := validEdge_merge_left hvalid
  exact (buildGraph_supportAdj_eq_true_iff d (mergeExtractions A B) eks u v).mpr
    ⟨e, (mem_validEdges_iff _ _ _).mpr ⟨⟨k, hk, hkEq'⟩, hv'⟩, hcase⟩

/-- **The seam is the build-path fold.** `M` in `buildGraph d M eks` is literally
`foldr ⊕ emptyExtraction` of the view family (`UseCase/Build.hs:11-13`), so the
support graph pinned above is the one the shipped pipeline constructs. -/
theorem buildGraph_foldMerge_eq_buildFold (d : Bool)
    (l : List (Extraction V EK N (SeamEdge V P))) (eks : List EK) :
    buildGraph d (foldMerge l) eks =
      buildGraph d (l.foldr mergeExtractions emptyExtraction) eks := by
  rw [foldMerge_eq_foldr]

end BuildLevel

end Merge

/-! ## §4. Phase 1 sees only `G_s`

`Q` (`Phase1/Modularity.lean`) reads `SupportGraph.vertices` and
`SupportGraph.adj` — through `deg`, `m`, `sigmaIn`, `sigmaTot`. `nodup`,
`complete`, `symm` and `irrefl` are proof fields, so two support graphs with the
same vertices and the same adjacency have the same `Q` for every partition. -/

section Modularity

variable {V : Type}

/-- `deg` is a function of `(V, A)`. -/
theorem deg_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (v : V) : g.deg v = g'.deg v := by
  simp only [SupportGraph.deg]
  rw [hv, ha]

/-- `2m` is a function of `(V, A)`: the degree sum is a `vertices.map deg`. -/
theorem totalDegree_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) : g.totalDegree = g'.totalDegree := by
  simp only [SupportGraph.totalDegree]
  rw [hv]
  rw [List.map_congr_left (fun v _ => deg_congr hv ha v)]

theorem m_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) : g.m = g'.m := by
  simp only [SupportGraph.m]
  rw [totalDegree_congr hv ha]

theorem twoM_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) : g.twoM = g'.twoM := by
  simp only [SupportGraph.twoM]
  rw [m_congr hv ha]

theorem sigmaIn_congr {g g' : SupportGraph V} (_hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (b : List V) : sigmaIn g b = sigmaIn g' b := by
  simp only [sigmaIn]
  rw [ha]

theorem sigmaTot_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (b : List V) : sigmaTot g b = sigmaTot g' b := by
  simp only [sigmaTot]
  rw [List.map_congr_left (fun v _ => deg_congr hv ha v)]

theorem communityTerm_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (b : List V) :
    communityTerm g b = communityTerm g' b := by
  simp only [communityTerm, twoM_congr hv ha, sigmaIn_congr hv ha b,
    sigmaTot_congr hv ha b]

/-- The block fold is a pointwise-equal fold, block by block. -/
theorem fold_blocks_congr {g g' : SupportGraph V} (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (bs : List (List V)) (q : Float) :
    bs.foldl (fun q b => q + communityTerm g b) q =
      bs.foldl (fun q b => q + communityTerm g' b) q := by
  induction bs generalizing q with
  | nil => rfl
  | cons b bs ih =>
      simp only [List.foldl_cons]
      rw [communityTerm_congr hv ha b, ih]

/-- `Q` is a function of `(V, A)` alone. -/
theorem modularity_congr_adj (g g' : SupportGraph V) (hv : g.vertices = g'.vertices)
    (ha : g.adj = g'.adj) (π : Partition V) : modularity g π = modularity g' π := by
  simp only [modularity]
  rw [m_congr hv ha]
  by_cases hm : g'.m = 0
  · simp [hm]
  · simp only [hm]
    exact fold_blocks_congr hv ha π.blocks 0.0

end Modularity

/-! ## §5. Fixtures: the AC-4 conflict and the dangling resurrection -/

namespace Fixture

/-- Both vertices are node keys; the node payload is irrelevant to the seam. -/
def allNodes : ExtView Bool Unit := fun _ => some ()

/-- The single `EdgeId` of the fixture. Two edges sharing it conflict. -/
def key : Bool := true

/-- AC-4's `eA`: `(false, true)`, weight 1. -/
def edgeA : SeamEdge Bool Nat := ⟨false, true, 1⟩

/-- AC-4's `eB`: **the same endpoints**, weight 2 (`Ext/Lax.lean:edgeB`). -/
def edgeB : SeamEdge Bool Nat := ⟨false, true, 2⟩

/-- The one-edge extraction `{key ↦ e}` over both nodes. -/
def oneEdge (e : SeamEdge Bool Nat) : Extraction Bool Bool Unit (SeamEdge Bool Nat) :=
  ⟨allNodes, fun k => if k = key then some e else none⟩

/-- The `Map`'s key enumeration. -/
def eks : List Bool := [key]

theorem edgeA_ne_edgeB : edgeA ≠ edgeB := by decide

theorem merge_AB_at_key :
    (mergeExtractions (oneEdge edgeA) (oneEdge edgeB)).extEdges key = some edgeA := by
  simp [mergeExtractions, ExtView.union, oneEdge]

theorem merge_BA_at_key :
    (mergeExtractions (oneEdge edgeB) (oneEdge edgeA)).extEdges key = some edgeB := by
  simp [mergeExtractions, ExtView.union, oneEdge]

/-- The AC-4 conflict is real on this fixture: the two merge orders differ. -/
theorem merges_differ :
    mergeExtractions (oneEdge edgeA) (oneEdge edgeB) ≠
      mergeExtractions (oneEdge edgeB) (oneEdge edgeA) := by
  intro h
  have hk := congrArg (fun M => M.extEdges key) h
  simp only [] at hk
  rw [merge_AB_at_key, merge_BA_at_key] at hk
  exact edgeA_ne_edgeB (Option.some.inj hk)

/-- The shipped `EdgeId` encodes `(source, target, relation)` in the key, so two
views that share an `EdgeId` agree on endpoints at that key. -/
theorem endpoint_agreement : ∀ (k : Bool) (eA eB : SeamEdge Bool Nat),
    (oneEdge edgeA).extEdges k = some eA → (oneEdge edgeB).extEdges k = some eB →
      eA.src = eB.src ∧ eA.tgt = eB.tgt := by
  intro k eA' eB' h1 h2
  by_cases hk : k = key
  · subst hk
    simp [oneEdge] at h1 h2
    subst h1
    subst h2
    decide
  · simp [oneEdge, hk] at h1

theorem supportAdj_eq : ∀ u v,
    (buildGraph false (mergeExtractions (oneEdge edgeA) (oneEdge edgeB)) eks).supportAdj u v =
      (buildGraph false (mergeExtractions (oneEdge edgeB) (oneEdge edgeA)) eks).supportAdj u v :=
  Merge.buildGraph_supportAdj_merge_swap false _ _ eks endpoint_agreement

def vertices : List Bool := [false, true]

theorem vertices_nodup : vertices.Nodup := by decide

theorem vertices_complete : ∀ v : Bool, v ∈ vertices := by
  intro v
  cases v <;> decide

theorem validEdges_AB :
    validEdges (mergeExtractions (oneEdge edgeA) (oneEdge edgeB)) eks = [edgeA] := by
  decide

theorem noSelfLoops_AB : NoSelfLoops (mergeExtractions (oneEdge edgeA) (oneEdge edgeB)) eks := by
  intro e he
  rw [validEdges_AB] at he
  simp only [List.mem_singleton] at he
  subst he
  decide

theorem validEdges_BA :
    validEdges (mergeExtractions (oneEdge edgeB) (oneEdge edgeA)) eks = [edgeB] := by
  decide

theorem noSelfLoops_BA : NoSelfLoops (mergeExtractions (oneEdge edgeB) (oneEdge edgeA)) eks := by
  intro e he
  rw [validEdges_BA] at he
  simp only [List.mem_singleton] at he
  subst he
  decide

/-- `G_s(A ⊕ B)` as Phase 1's object. -/
def gsAB : SupportGraph Bool :=
  buildGraph_toSupportGraph false (mergeExtractions (oneEdge edgeA) (oneEdge edgeB)) eks
    vertices vertices_nodup vertices_complete noSelfLoops_AB

/-- `G_s(B ⊕ A)` as Phase 1's object. -/
def gsBA : SupportGraph Bool :=
  buildGraph_toSupportGraph false (mergeExtractions (oneEdge edgeB) (oneEdge edgeA)) eks
    vertices vertices_nodup vertices_complete noSelfLoops_BA

theorem gsAB_adj_eq : gsAB.adj = gsBA.adj := by
  funext u v
  simp only [gsAB, gsBA, buildGraph_toSupportGraph_adj]
  exact supportAdj_eq u v

/-- **The seam, closed on the AC-4 fixture.** The two merge orders are different
extractions (`merges_differ`), yet `Q` is identical on **every** partition of
`V`: the first/old-wins precedence that `mergedExtraction_laxColimit_oldWins`
proves observable at the `Extraction` level is unobservable at the audit level. -/
theorem ac4_seam_modularity_eq (π : Partition Bool) : modularity gsAB π = modularity gsBA π :=
  modularity_congr_adj gsAB gsBA rfl gsAB_adj_eq π

end Fixture

/-! **The dangling drop is applied once, after the colimit.** `buildGraph` runs on
the merged extraction (`Build.hs:11-13`), so an edge dangling inside its own view
is *not* dropped from `G_s(M)` if its endpoint node arrives from another view.
Hence `G_s(M) ≠ ⋃ᵢ G_s(Eᵢ)` — `G_s` is a function of `M`, not of the family's
per-view graphs. -/
namespace DanglingResurrection

/-- Only `false` is a node key: `danglingEdge`'s target is missing. -/
def nodesA : ExtView Bool Unit := fun b => if b = false then some () else none

/-- The other view contributes the missing node `true`, and no edges. -/
def nodesBoth : ExtView Bool Unit := fun _ => some ()

/-- `false → true`: dangling in `extA`. -/
def danglingEdge : SeamEdge Bool Nat := ⟨false, true, 0⟩

def extA : Extraction Bool Bool Unit (SeamEdge Bool Nat) :=
  ⟨nodesA, fun k => if k = true then some danglingEdge else none⟩

def extB : Extraction Bool Bool Unit (SeamEdge Bool Nat) :=
  ⟨nodesBoth, fun _ => none⟩

def eks : List Bool := [true]

/-- Inside its own view the edge is dangling. -/
theorem dangling_in_A : validEdge extA danglingEdge = false := by decide

/-- … so the view's own `G_s` has no support edge… -/
theorem supportAdj_A : (buildGraph false extA eks).supportAdj false true = false := by decide

/-- … but after the merge the target node is present… -/
theorem valid_in_merge : validEdge (mergeExtractions extA extB) danglingEdge = true := by decide

/-- … so the merged graph's `G_s` has it. -/
theorem supportAdj_merge :
    (buildGraph false (mergeExtractions extA extB) eks).supportAdj false true = true := by
  decide

/-- The edge is absent from **both** per-view support graphs, and present in the
merged one: the dangling drop does not commute with the merge. -/
theorem not_union_of_views :
    (buildGraph false extA eks).supportAdj false true = false ∧
      (buildGraph false extB eks).supportAdj false true = false ∧
      (buildGraph false (mergeExtractions extA extB) eks).supportAdj false true = true := by
  refine ⟨by decide, by decide, by decide⟩

end DanglingResurrection

end Seam

end GraphosFormalization
