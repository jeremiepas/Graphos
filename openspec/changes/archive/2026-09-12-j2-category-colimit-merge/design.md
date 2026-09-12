# J2 category half — design (colimit merge, coequalizer conflicts, confluence)

This is the category-theoretic half of [AVI-528](/AVI/issues/AVI-528) J2. It agrees
with the graph/community half ([AVI-555](/AVI/issues/AVI-555)) on the shared
notation, the iso notion, and the conflict semantics, then carries the categorical
universal properties for M1/M2/M4/M5 and supplies the canonical form backing M3.

## 0. Shared interface (jointly owned with graph half, set on AVI-528)

- **Shared object.** `(G, γ)` with `G = merge(V)` and `γ = cluster(G)`.
- **Iso notion.** A morphism-target isomorphism is a bijection preserving
  `NodeId` labels, edge weights `A_uv`, and community labels `c(v)`. Two outputs
  "agree up to iso" iff such a structure-preserving bijection exists. (This is the
  *only* iso notion used; all isomorphisms below are in this sense.)
- **Conflict semantics.** Last-write-wins on shared `NodeId`: the later-applied
  view's payload wins. This is a **coequalizer**, explicitly *not* a coproduct.

## 1. Objects and morphisms first (before any universal property)

### 1.1 The category GraphCat

- **Objects.** `Graph` (`Domain/Graph/Core.hs`) carrying
  `gNodes :: Map NodeId Node`, `gEdges :: Map (NodeId,NodeId) Edge`,
  `gAdjFwd/gAdjBack`, `gDirected :: Bool`, plus the community assignment
  `γ = CommunityMap = Map CommunityId [NodeId]` attached by `cluster`. We write an
  object `X = (G, γ_G)` and omit `γ` when community structure is not under
  discussion.
- **Morphisms.** A morphism `h : X -> Y` is a **graph homomorphism preserving the
  iso-notion data**: a partial function `dom(h) ⊆ Nodes(X)` into `Nodes(Y)` that
  (i) is `NodeId`-consistent wherever defined, (ii) carries every edge of `X`
  (with its weight `A_uv`) to the corresponding edge of `Y`, and (iii) respects
  directedness and community labels. The identity is the total map; composition is
  partial-function composition restricted to a common domain. This is the standard
  category of graphs and structure-preserving homomorphisms
  (see Mac Lane, *Categories of Graphs* / Lack–Šťava, *Coherent functors for the
  theory of Graph algebras*, 2006).

> Diagram honesty. Every morphism above corresponds to an actual Haskell map:
> a homomorphism `h : X -> Y` is realized by `Map.lookup`-style inclusion of
> `gNodes X` into `gNodes Y`; composition is `(Prelude..)`. No morphism is
> declared without this realization.

### 1.2 The view diagram `D : I -> GraphCat`

- **Index category `I`.** For a batch of extraction views `{vₖ}` produced by
  `UseCase.Extract.*`, `I` is the **discrete** category whose objects are the
  views and whose only morphisms are identities — each view is an independent local
  extraction. (Incremental/watch-mode joins add spans/pushout edges; that is out of
  scope here and flagged 4.4.)
- **Diagram.** `D(k) = Gₖ` is the graph built from view `k`
  (`buildGraph` in `Domain/Graph/Core.hs`), with community assignment left empty
  until after merge (`gCompositions = Nothing`, fresh labels imposed by re-cluster).

## 2. `merge = colim D` and its universal property

### 2.1 Coproduct formulation

For **consistent** views (views that agree on every shared `NodeId` payload and
share one `gDirected` flag), the index category `I` is discrete, so

> `colim D = ⊔ₖ Gₖ` — the **coproduct** in `GraphCat`.

The universal cocone is the family of inclusions `λₖ : Gₖ -> ⊔ₖ Gₖ`; the
**universal property** states:

> For any object `G` and any cocone `fₖ : Gₖ -> G`, there exists a **unique**
> mediating morphism `⟪μ⟫ : ⊔ₖ Gₖ -> G` with `⟪μ⟫ ∘ λₖ = fₖ` for all `k`.
> Uniqueness is *up to the iso notion of §0* (unique iso).

### 2.2 Graphos mapping (this universal property IS `mergeGraphs`)

`mergeGraphs :: Graph -> Graph -> Graph` (`Domain/Graph/Core.hs:66`) realizes the
coproduct fold:

```
mergedNodes    = gNodes old <> gNodes new        -- Map.union: new wins on key clash
mergedEdges    = filter dangling (gEdges old <> gEdges new)
mergedFwd/Bwd  = Map.unionWith Set.union (…old)(…new)
```

- `Map.union` (`<>`) is associative with identity `empty`, so folding over any
  linear order of the family produces a cocone; the coproduct object is the union.
- The inclusions `λₖ` are the canonical `Map.insert`-style embeddings of each view's
  maps into the union — exactly the homomorphisms of §1.2.
- **Diagram honesty.** For consistent views `mergeGraphs` is *commutative and
  associative* on the wire: agreeing payloads make `A <> B = B <> A`, adjacency is
  symmetric, and `computeGraphHash` sorts keys, so the produced object is the same
  representative regardless of fold order. The unique mediating morphism `⟪μ⟫` is
  the map sending each view's nodes/edges to themselves in the union.
- **Oracle honesty.** `computeGraphHash` hashes only node ids and edge endpoint
  tuples `(a,b)` — it omits `edgeWeight`, `edgeRelation`, and community labels — so
  it is a *structural* canonical key, not a full iso oracle: identical hash ⇒
  identical node+endpoint skeleton, but the iso notion of §0 also requires equal edge
  weights and community labels. The faithful determinism oracle is therefore the triple
  `(gNodes, gEdges, CommunityMap)`, with `computeGraphHash` a necessary (structural)
  check, not sufficient on its own.

## 3. Confluence for consistent views (M1 + M2) via the universal property

### 3.1 Claim

For any permutation `σ` of consistent views,
`merge(V) ≅ merge(V_σ)` and `cluster(merge(V)) ≅ cluster(merge(V_σ))`
(up to the §0 iso). [M1, M2]

### 3.2 Proof sketch (cites the universal property + uniqueness)

1. `σ` is an automorphism of the discrete index category, hence a diagram
   isomorphism `D ≅ D∘σ`. By **functoriality of colimit**, it induces an isomorphism
   of coproducts `merge(V) ≅ merge(V_σ)`. (Mac Lane, *CTW*, I.3 Prop 1: colimits are
   unique up to unique iso; I.6 symmetric functions give the canonical coproduct
   isomorphism under index permutation.)
2. For consistent views the induced iso is **equality** of the canonical
   representative (§2.2), so the two outputs are identical graphs.
3. `detectCommunities` (`Domain/Community.hs`) builds `leidenCore` from
   `Map.keys (gNodes g)` in ascending key order and relabels via
   `leidenStateToCommunityMap` in the same deterministic order. Hence `cluster` is a
   **deterministic function of the canonical graph**, so it preserves the equality
   of §3.2 step 2, giving `cluster(merge(V)) ≅ cluster(merge(V_σ))`. [M2]

> The confluence claim is *exactly* the universal property: coproducts are unique
> up to unique iso, and a view permutation is a diagram automorphism. No stronger
> claim is made — confluence holds **only for consistent views**.

## 4. Conflict = coequalizer, not coproduct (M4)

### 4.1 Why it must be a coequalizer

At the `Map` layer there is exactly one slot per `NodeId`, so `Map.union` always
collapses on `NodeId` regardless of whether views agree; it never retains two distinct
payloads for the same id. The coproduct-vs-coequalizer contrast is therefore a
property of the **index category**, not of the container: over the discrete index of
consistent views, that single collapsing union *reads as* the coproduct
(`Gₐ ⊔ G_b`) because agreeing payloads make the collapse invisible. When two views
disagree on the payload of a shared `NodeId` `n`, the same `Map.union` op is no longer
a bare coproduct — it *is* the LWW quotient identifying the two candidate payloads,
which is precisely a coequalizer. Resolution emits one node per id
(`q ∘ u = q ∘ v`), never both; calling it a coequalizer records that the collapsing op
now performs identification under conflict.

### 4.2 Construction

Let `u, v : P -> Gₐ ⊔ G_b` be the two inclusions of a single candidate-payload
object `P` picking the two conflicting payloads of `n`. The **coequalizer**

> `q : Gₐ ⊔ G_b -> Q` with `q ∘ u = q ∘ v`

identifies the pair to one node. In Graphos this is exactly
`gNodes old <> gNodes new` with **right-wins** (`new`/later = last write): the
coequalizer quotient keeps the later payload and drops the earlier. The dangling
edge filter and adjacency unions then propagate through `q`.

- **Divergence quantified.** Let `Δ = Δ_nodes ∪ Δ_edges` where
  `Δ_nodes = { n : payloadₐ(n) ≠ payload_b(n) }` and
  `Δ_edges = { (a,b) : edgeWeightₐ(a,b) ≠ edgeWeight_b(a,b) }` (edges are keyed by
  `(NodeId,NodeId)` and merged new-wins, so a differing weight on a shared tuple also
  flips under fold order). The two orderings differ **only** on `Δ`; divergence is
  bounded by `|Δ_nodes| + |Δ_edges|` (not node count alone), and measured further by
  the induced `countMoves` (community labels moved, `Domain/Community.hs:countMoves`).
  Deterministic given order: the fold is a fixed linear order, so the coequalizer
  result is a fixed function of that order.
- **Not a coproduct.** A coproduct would keep both payloads (no identification); the
  LWW quotient explicitly collapses them, so the construction is the coequalizer,
  and the acceptance test must assert the collapse (one node per conflicting id).

### 4.3 Graphos mapping

`mergeGraphs` right-wins on key clash is the LWW coequalizer; `Δ` is exactly the set
of `NodeId`s present in both inputs with differing `Node` payloads. The requirement
SHALL quantify `|Δ|` and assert determinism-given-order, not order-independence.

## 5. `merge × cluster` is non-commutative under conflicts (CTO note 3, case 3)

### 5.1 Claim

The colimit algebra of `merge` is **not** the same commutative/idempotent algebra as
`cluster`; `merge` and `cluster` do **not** commute in general, and under conflicts
their composition is order-dependent. This must be *measured*, not assumed absent.

### 5.2 Why (functoriality fails)

1. **`cluster` is not a functor preserving coproducts.** Leiden computes a *global*
   partition from adjacency (`leidenCore` reads the full `gAdjFwd`); the partition of
   a union is not the union of partitions, and labels are freshly remapped
   (`leidenStateToCommunityMap`), so `cluster(Gₐ ⊔ G_b) ≠ cluster(Gₐ) ⊔ cluster(G_b)`.
2. **Under conflicts, `merge` itself is order-dependent.** The LWW coequalizer picks
    the later payload, so `merge(A,B)` and `merge(B,A)` differ on
    `Δ = Δ_nodes ∪ Δ_edges`; feeding either into `cluster` can shift the argmax
    tie-break and visit-order, changing the partition. Non-commutativity is quantified
    by `countMoves( … )` across the two compositions plus `|Δ_nodes| + |Δ_edges|`.
3. **Different algebras.** `merge` is a colimit/coequalizer (universal,
   order-independent up to iso *only* for consistent views); `cluster` is a
   deterministic-but-non-functorial partition function. Collapsing the two into one
   commutative-idempotent algebra would erase the conflict boundary — which is
   exactly what CTO note 3 forbids.

## 6. Idempoteness `A +-+ A ~= A` asserted separately (M5)

`mergeGraphs A A = A` holds **by `Map.union` idempotency on identical maps**:
`gNodes A <> gNodes A = gNodes A`, edges filtered identically, adjacency unioned to
itself, `gDirected` unchanged, `computeGraphHash` stable. This is asserted **as an
independent property**, derived solely from Map idempotency — *not* from §3
confluence nor §5 non-commutativity. (Note the layering: graph-layer idempoteness
always holds; the community relabel after merge is deterministic, so the pair
`((A),γ)` is idempotent up to the §0 iso.)

## 7. Acceptance coverage (CTO note 3 — all three cases)

| Case | Categorical object | Acceptance assertion |
|---|---|---|
| 1. Consistent coproduct | `colim D = ⊔ Gₖ`, universal cone `λₖ`, unique `⟪μ⟫` | two view orders → isomorphic `(graph, communityMap)`; mediating morphism unique up to §0 iso [M1,M2] |
| 2. LWW coequalizer | `q : ⊔ -> Q`, `q∘u = q∘v`; divergence `|Δ_nodes| + |Δ_edges|` | conflict resolves to one node per id; deterministic given order; divergence bounded by `|Δ_nodes| + |Δ_edges|`/`countMoves` [M4] |
| 3. `merge × cluster` | colimit ≠ cluster algebra; `cluster` non-functorial | non-commutativity measured (not assumed); results differ under reordered conflict [M2,M4] |

## 8. Prior art

- Colimits unique up to unique iso; symmetric coproducts under index permutation —
  Mac Lane, *Categories Work*, I.3 & I.6.
- Category of graphs and structure-preserving homomorphisms; coherent functors —
  Lack & Šťava, *Coherent functors for the theory of Graph algebras* (2006);
  Freyd & Schauer, *Bounds* (1990).
- Coequalizer = quotient by the congruence generated by a parallel pair — standard
  (Mac Lane, *CTW* I.6); last-write-wins as a graph coequalizer quotient is the
  special case of a pushout along an identity pair.

## 9. Scope / out of scope

- **In scope:** merge as coproduct/colimit and LWW coequalizer; confluence for
  consistent views; `merge × cluster` non-commutativity; idempoteness.
- **Out of scope (flagged, not asserted here):** spans/pushouts for incremental and
  watch-mode view joining (`UseCase.Extract.*` incremental path), which extend `I`
  beyond discrete and require pushout universal properties — reserved for a follow-up
  once the graph half fixes the incremental surface.
