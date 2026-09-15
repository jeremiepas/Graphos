# Monoidal Structure over Composition — Mathematical Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-525](/AVI/issues/AVI-525) (child of [AVI-512](/AVI/issues/AVI-512), child of [AVI-508](/AVI/issues/AVI-508))
**Status:** requirements document — grounded in the domain model + openspec specs
**Scope:** math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion.

---

## 0. Scope and relationship to the sibling math docs

This doc formalizes **composition** in the Graphos context graph — precisely how edges combine into paths, how those paths compose, and how independent contexts compose side-by-side. It is the *sequential + parallel composition* complement to:

- **AVI-511** ("Merge / Consolidation as a Colimit") models *gluing overlapping views* into one graph (colimit / pushout over `𝔻`). That is **union** of contexts.
- **AVI-524** ("Lax Colimit / Functoriality") models *inconsistent merge* and *pipeline composition of functors*.
- **AVI-518** ("Shortest-Path Reachability & SCC") models *reachability* (`⤳`) as the reflexive-transitive closure of the edge relation, and *SCC* as its mutual-reachability quotient.

This doc supplies the **category/monoid that AVI-518 takes as given**: the edge relation is not merely a set of pairs but carries a canonical **composition** (edges concatenate into walks). Concretely:

| Novel contribution | Concrete Graphos surface | Prior-doc gap it closes |
|---|---|---|
| (1) Path category `Π(G)` over the edge relation (composition = walk concatenation) | `shortestPath` returns ordered node-list (`Query.hs:80`) | AVI-518 took `⤳` as given; did not name its carrier |
| (2) Composition laws (strict assoc + unit); monoid of closed walks | neighbor-expansion depth (`UseCase/Query.hs:380`) | "k-hop" was informal |
| (3) Reachability / expansion as structural functors out of `Π(G)` | `shortestPath`, `neighborhoodExpansion` | AVI-518 Thm 2.4 stated, not functorIALIZED |
| (4) SCC condensation as coarsest equivalence quotient of `Π(G)` (dual to merge colimit) | `stronglyConnectedComponents` (AVI-518 GAP-1) | AVI-518 §11 named it; no universal-property statement |
| (5) Parallel composition = symmetric monoidal structure on `LapGraph` (disjoint union) | `mergeExtractions` disjoint merge (`Core.hs:129`); `emptyExtraction` (`Types/Graph.hs:54`) | AVI-511 §1.2(i) coproduct, no unit/symmetry framing |

> **Constrains (required by the issue).** Every requirement below names one of these surfaces. Behavior-constraining requirements (marked 🔧) require a loop with [Graphos Dev](/AVI/agents/graphos-dev) before finalizing the Haskell semantics.

---

## 1. Notation (shared glossary + composition objects)

Notation is drawn from the shared glossary in `docs/math-requirements/consolidated-requirements.md` (§0) and [AVI-511](/AVI/issues/AVI-511) (§0). This doc adds composition-specific objects. Line numbers verified against checked-in sources.

- `NodeId = Text` — `Graphos.Domain.Types.Node.NodeId` (`Node.hs:46`).
- `Edge = Edge { edgeId, edgeSource :: NodeId, edgeTarget :: NodeId, edgeRelation :: Relation, edgeWeight :: Double, edgeConfidence :: Confidence Double, edgeExtra :: Maybe Value }` (`Edge.hs:88`). `edgeSource / edgeTarget / edgeRelation` are the incidence + coloring accessors. `Relation ∈ {Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred}` (`Edge.hs:32`).
- `Graph = Graph { gNodes :: Map NodeId Node, gEdges :: Map (NodeId,NodeId) Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId), gDirected :: Bool, … }` (`Graph/Core.hs:47`).
- `LabeledGraph = LabeledGraph { gNodes, gEdges, gAdjFwd, gAdjBack }` (`Types/Graph.hs:83`).
- `Extraction = Extraction { extractionNodes :: Map NodeId Node, extractionEdges :: Map EdgeId Edge }`, accessors `extNodes / extEdges` (`Types/Graph.hs:47,66-69`). `emptyExtraction` (`Types/Graph.hs:54`) is the initial view.
- **Quiver of the graph.** `𝓖 = (V, A)` with `V = Map.keys (gNodes g)` and `A = { (edgeSource e, edgeTarget e) : e ∈ gEdges g }` (the ordered-pair projection of `gEdges`; multi-edges collapse to their endpoint pair). `𝓖` is the **directed multigraph / quiver** underlying `g`; this is the argument of the free-category functor below.
- **Walk.** A **walk** `p` from `v` to `w` is a list `[e_1, …, e_k]` of edges with `edgeSource e_1 = v`, `edgeTarget e_k = w`, and `edgeTarget e_i = edgeSource e_{i+1}` for all `i`. Its **length** is `|p| = k`. The **empty walk** `[v]` (length 0, no edges) is the identity at `v`. A **path** is a walk with distinct internal nodes; a **cycle** is a closed walk (`v = w`) of length ≥ 1.
- **Composition of walks.** For composable walks `p : v→u`, `q : u→w`, `p · q := p ++ q` (list concatenation), `: v→w`. Identity at `v` is the empty walk `ε_v := [v]`.
- **Reachability** `u ⤳ v` = reflexive-transitive closure of `A` (AVI-518 §1.1); mutual reachability `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)`.
- **Hop distance.** `d(u,v) = min { |p| : p is a walk u→v over A }`; `∞` if `¬(u ⤳ v)`.

> **Constrains:** `Graph` (`Graph/Core.hs:47`), `Edge`/incidence (`Edge.hs:88-92`), `Extraction`/`emptyExtraction` (`Types/Graph.hs:47,54`), `shortestPath` (`Query.hs:80`), `neighborhoodExpansion` (`UseCase/Query.hs:380`).

---

## 2. The path category `Π(G)` — objects, morphisms, composition

### 2.1 Definition of `Π(G)`

**Definition (path / walk category).** For a context graph `G` with quiver `𝓖 = (V, A)`, define the **path category** `Π(G)`:
- **objects** = the nodes `V`;
- **morphisms** `v → w` = the set of walks from `v` to `w` over `A` (lists of composable edges; the empty walk `ε_v` is the unique length-0 morphism at `v`);
- **composition** `· : Π(G)(v,u) × Π(G)(u,w) → Π(G)(v,w)` = walk concatenation `p · q = p ++ q`;
- **identity** `id_v = ε_v = [v]` (the empty walk).

**Proposition (`Π(G)` is a category; in fact a strict one).** Identity laws: `ε_v ++ p = p` and `p ++ ε_w = p` (list `++` with an empty list on either side is the identity, on the nose). Associativity: `(p ++ q) ++ r = p ++ (q ++ r)` for composable `p,q,r` — list concatenation is **strictly associative** (no bracketing ambiguity), so composition is associative **on the nose**, not merely up to canonical iso. Hence `Π(G)` satisfies the category axioms strictly. ∎

> **Honesty note (strictness).** The free-category construction is generally stated with composition defined up to a canonical associativity isomorphism (morphisms = equivalence classes of bracketed composites). Graphos realizes walks as concrete **lists**, and Haskell's `Data.List.(++)` is strictly associative with strict unit, so the concrete `Π(G)` is a **strict category**: the composition and unit laws hold on the nose. The abstract statement (assoc up to canonical iso) is weaker and always holds; the concrete realization is stronger.

> **Constrains:** `gEdges` (the edge list feeding walk construction), `edgeSource/edgeTarget` (`Edge.hs:88-92`), `shortestPath` returning `[NodeId]` (a walk, `Query.hs:80`). The composition requirement is a *structure* requirement: any path routine must compose walks by concatenation with the empty list as identity.

### 2.2 Universal property — `Π` is the free-category functor (adjunction)

**Definition (forgetful + free functors).** The **forgetful functor** `U : Cat → Gra` sends a category to its underlying quiver (objects → vertices, morphisms → edges, forgetting composition/identities). The **free-category functor** `Π : Gra → Cat` sends a quiver to its path category and a quiver-map to the induced functor.

**Claim (free–forgetful adjunction `Π ⊣ U`).** `Π` is left adjoint to `U`: there is a natural isomorphism
```
Hom_Cat( Π(𝓖),  C )   ≅   Hom_Gra( 𝓖,  U(C) )
```
natural in every quiver `𝓖 : Gra` and every category `C : Cat`. The unit `η_𝓖 : 𝓖 → U(Π(𝓖))` is the canonical quiver inclusion of edges into the path category; the **universal mapping-out property** is:

> **R-COMP-UNIQ (free category).** For every quiver map `f : 𝓖 → U(C)` into the underlying quiver of a category `C`, there exists a **unique** functor `f̂ : Π(𝓖) → C` with `U(f̂) ∘ η_𝓖 = f`. The functor `f̂` is **uniquely determined** — it must extend `f` by concatenation, `f̂(e_1 … e_k) = f(e_1) ∘ … ∘ f(e_k)` — so no choice remains; equivalently `Π(𝓖)` is the free category on `𝓖`, unique up to a **unique isomorphism** commuting with the universal arrows.

> **Constrains:** This is the standard free-forgetful adjunction (the left-adjoint free-category functor to the forgetful `U : Cat → Gra`; Mac Lane, *Category Theory for the Working Mathematician*, Ch. IV.1; Riehl, *Categorical Methods in Algebraic Topology*, Ch. 4). The requirement pins that any Graphos operation mapping edges into a compositional structure factors uniquely through `Π(𝓖)`.

---

## 3. Composition laws — monoid on composable arrows

### 3.1 Sequential composition laws (diagram honesty)

Composition in `Π(G)` is a **partial binary operation** (defined exactly on composable pairs `(p,q)` with `edgeTarget p = edgeSource q`) satisfying, for all composable triples:
- **Associativity (on the nose):** `(p · q) · r = p · (q · r)`;
- **Unit:** `ε_{src} · p = p · ε_{tgt} = p`.

Every claimed commutative diagram below corresponds to an actual equality of composed walks. E.g. for composable `p : v→u`, `q : u→w`, `r : w→x`:
```
   v ──p──▶ u ──q──▶ w ──r──▶ x
   │                              │
   │  (p·q)·r  =  p·(q·r)         │  = p++q++r
   ▼                            ▼
   w ──(p·q)─▶ x      v ──(q·r)─▶ x     (both equal the single walk [p,q,r])
```
The two legs are **the same list**; the square commutes on the nose. No comparison 2-cell is needed (unlike AVI-524 §3.2 lax merge): sequential walk-composition is strict.

> **Constrains:** `shortestPathWithCached` builds a single `[NodeId]` list (`Query.hs:84`); `neighborhoodExpansion` walks edges (`UseCase/Query.hs:391`). Composition must be associative with the empty node-list as unit — tested by asserting `(p·q)·r == p·(q·r)` on random composable triples.

### 3.2 Monoid of closed walks at a node

For a fixed node `v`, the **closed walks** `Π(G)(v,v)` (walks starting and ending at `v`) form a **monoid** under `·`, with unit `ε_v`:
```
WalkMonoid(v) := ( Π(G)(v,v),  ·,  ε_v )
```
This is the **fundamental walk monoid** of `G` at `v` (analogous to the fundamental groupoid/monoid of a space, but without invertibility — walks need not reverse). Requirement: any routine reasoning about "cycles through `v`" or "return walks" respects this monoid (associativity + identity), e.g. `edgeBetweenness`-style return counts compose monoidally.

> **Constrains:** Closed walks appear implicitly in SCC self-loops and in `gAdjFwd[v] ∋ v` reflexive self-edges. No dedicated closed-walk routine exists yet; if one is added it must be the monoid `WalkMonoid(v)`.

---

## 4. Reachability & expansion as structural functors out of `Π(G)`

### 4.1 Reachability as the preorder reflection of `Π(G)`

**Definition (reachability as thin-category coimage).** The reflexive-transitive closure `⤳` (AVI-518 §1.1) is the **image** of `Π(G)` under the reflective subcategory `Preord ↪ Cat`: there is a unique functor
```
ρ_G : Π(G) → (V, ⤳)
```
that is the identity on objects and coequalizes all parallel morphisms — `p ⤳ q ⟹ ρ_G(p) = ρ_G(q)` for composable `p, q` (i.e. `ρ_G` collapses every hom-set `Π(G)(v,w)` to a single arrow when one exists, and to nothing otherwise). `(V, ⤳)` is a **thin category** (≤1 morphism per pair) by construction.

**Universal property (preorder reflection).** The reflector `R : Cat → Preord` is left adjoint to `incl : Preord ↪ Cat`; the counit `R∘incl ⇒ id_{Preord}` is an iso, so `ρ_G : Π(G) ⇒ (V,⤳)` is the **universal** functor from `Π(G)` into a preorder. For every preorder `P` (thin category) and every functor `F : Π(G) → P`, there is a **unique** preorder-functor `F̂ : (V,⤳) → P` with `F̂ ∘ ρ_G = F`. Because a thin category admits at most one morphism per pair, `F` already coequalizes parallel morphisms, so `F̂` is *forced* (no choice remains); equivalently `(V,⤳)` is unique up to a **unique isomorphism** commuting with the quotient arrow. ∎

> **Constrains:** `shortestPath g s t == Just p` ⟺ `s ⤳ t` (AVI-518 Thm 2.4); `shortestPathWithCached` (`Query.hs:84`) is the decision procedure for `ρ_G`. Composition is strict (on the nose, §3.1), so `ρ_G` is an honest functor — not a lax comparison.

### 4.2 Functoriality in `G` (reachability is endofunctorial on quivers)

**Claim.** The assignment `G ↦ (V(G), ⤳_G)` extends to a functor
```
Rea : Gra → Preord
```
On **objects**: a quiver `𝓖` maps to its reachability preorder (reflexive-transitive closure of its arcs). On a **quiver homomorphism** `f : 𝓖 → 𝓗` (preserving incidence: `src(f(e)) = f(src(e))`, `tgt(f(e)) = f(tgt(e))`): `Rea(f) = f` on vertices, which is a preorder morphism because `f` maps every `𝓖`-walk to an `𝓗`-walk, hence `u ⤳_𝓖 v ⟹ f(u) ⤳_𝓗 f(v)`. This is the composite `Gra →Π Cat →R Preord`; both legs are functors (§2.2 + §4.1), so `Rea` is a functor. The naturality square for `f : 𝓖 → 𝓗` commutes **on the nose**:
```
   Π(𝓖) ──Π(f)──▶ Π(𝓗)
    │              │
   ρ_𝓖            ρ_𝓗
    ▼              ▼
  (V𝓖,⤳) ──f────▶ (V𝓗,⤳)    (f(u)⤳f(v) whenever u⤳v, since f maps walks to walks)
```

> **Constrains:** reachability is invariant under any permutation of `gNodes`/`gEdges` keys (AVI-518 INV-9); `Rea` depends only on `(V,E)`.

### 4.3 Neighborhood expansion as truncated reachability (naturality of depth-k balls)

**Definition.** For source `s` and depth `k`, the **depth-k forward ball** is `B_k(s) := { v : d(s,v) ≤ k } ⊆ (V, ⤳)`, where `d` is hop distance (§1). As `k → ∞`, `B_∞(s) = { v : s ⤳ v }` recovers the full forward cone of `ρ_G`.

**Claim (expansion is a natural family).** `neighborhoodExpansion` realizes `B_k(s)` over the index adjacency; for a quiver homomorphism `f : 𝓖 → 𝓗`, `f` can only shrink or preserve balls, never enlarge them:
```
f( B_k^𝓖(s) )  ⊆  B_k^𝓗( f(s) )
```
(the image of a ≤k walk is a ≤k walk, so membership can be *lost* but never *gained*). The inclusion square
```
   B_k^𝓖(s) ──f──▶  B_k^𝓗(f(s))
     │                   │
     ⊆                   ⊆
     ▼                   ▼
   (V𝓖,⤳)  ───f──────▶  (V𝓗,⤳)
```
commutes in the sense of **subset-inclusion naturality** (a lax/natural-transformation-of-filtration statement, not an iso). Expansion is a **monotone filtration** indexed by `k ∈ ℕ` — a direction, not an isomorphism.

> **Constrains:** `neighborhoodExpansion startId depth g idx` (`UseCase/Query.hs:380`) returns nodes within hop-depth `depth` over `giAdj idx`; `bfsFrom idx startId depth` (`Graph/Index.hs`). The requirement: expansion equals `B_depth(s)` **exactly** — no node outside the depth-k forward ball may appear, and none may be missing. Composition is strict; the laxity here is the *truncation* (depth cut), not the walk composition.

---

## 5. SCC condensation as the coarsest quotient of `Π(G)`

### 5.1 Condensation as the poset (T0) reflection of reachability

**Definition.** Mutual reachability `↔` (AVI-518 §1.2) is the **symmetrization** of `⤳`: `u ↔ v ⟺ (u ⤳ v) ∧ (v ⤳ u)`. By AVI-518 Thm 2.1 it is an equivalence relation, hence a **congruence** on `(V,⤳)` and on `Π(G)` (any two parallel morphisms between `↔`-equivalent objects are identified). The **SCC condensation** is the quotient:
```
cond(G) := (V, ⤳) / ↔   (= V/↔ with the induced partial order)
```
whose vertices are the SCCs and whose arcs are the inter-SCC arcs of `E` (AVI-518 §1.3).

**Universal property (coarsest symmetric quotient / T0 reflection).** The poset reflection is left adjoint to `Posord ↪ Preord`: `(V,⤳) → cond(G)` is the **universal** preorder map that factors through a partial order. Formally, for every partial order `Q` and every monotone map `h : (V,⤳) → Q`, there is a **unique** monotone map `ĥ : cond(G) → Q` with `ĥ ∘ q = h` (where `q : (V,⤳) → cond(G)` is the quotient). A partial order is antisymmetric, so `h` must already identify every `↔`-class; `ĥ` is therefore *forced*, and is unique ⇒ `cond(G)` is the **coarsest** quotient making reachability antisymmetric, unique up to a **unique isomorphism** commuting with the quotient arrow. ∎

> **Honesty (quotient = coequalizer, dual to merge).** `cond(G)` is the **coequalizer** in `Graph`/`LapGraph` of the pair collapsing each `↔`-class; equivalently a **quotient epimorphism** `q : G ↛ cond(G)` surjective on objects and collapsing arrows. It is a *limit-like* (co-quotient) construction — **dual** to the merge colimit ([AVI-511](/AVI/issues/AVI-511), which *glues* views into one graph); AVI-518 §11 flagged this duality. The arrow is reversed: merge coproduces views into a single graph, condensation folds one graph along a congruence. The objects/edges both constructions act on are identical (`V`, `E`) — so any functorial claim about merge (§2–4 of this doc, §3 of AVI-524) must also respect that `cond` is defined purely on `(V,E)`.

> **Constrains:** `stronglyConnectedComponents :: Graph -> Map Int [NodeId]` (+ `WithCached`) is the new routine that computes the `↔`-classes (AVI-518 GAP-1); condensation is built from them (`AVI-518 §3.1`). The **partition** (the sets) is canonical and unique (AVI-518 Thm 2.2); only the integer community-labeling is order-dependent (§10). `cond(G)` is acyclic (AVI-518 Thm 2.3) — consistent with it being a partial order / thin category.

### 5.2 Diagram honesty

The quotient `q : Π(G) → Π(cond(G))` collapses each SCC to an object and is the identity on inter-SCC walks; the square
```
   Π(G)  ──q──▶  Π(cond(G))
    │              │
   (V,⤳) ───q────▶ (V/↔, ≤)
```
commutes **on the nose**: `q` identifies exactly the parallel morphisms that `ρ` then folds (`p ↦ q` iff `src(p) ↔ tgt(p)`). No 2-cell is needed — this is a **strict quotient functor** (an epimorphism in `Cat`), not a lax comparison.

> **Constrains:** `edgeBetweenness` (AVI-518, `Analysis.hs:152`) counts walks through nodes; its return-walk structure respects `WalkMonoid(v)` (§3.2) and is constant on SCCs (all members of an SCC share the same in/out cone), so condensation is the canonical co-domain for betweenness-style aggregates.

---

## 6. Parallel composition = symmetric monoidal structure on `LapGraph`

### 6.1 The monoidal product (parallel, side-by-side)

**Definition (parallel composition).** On the category `Extract` of extractions with structure-preserving embeddings, define:
- **object** = `Extraction`;
- **parallel composition** `A ⊗ B := A ⊔ B`, the **disjoint-key union**: `extNodes(A⊗B) = extNodes A ⊎ extNodes B`, `extEdges(A⊗B) = extEdges A ⊎ extEdges B` — i.e. `mergeExtractions A B` **restricted to disjoint key sets** (no shared keys ⇒ `Map.union` is genuine disjoint union);
- **unit** = `emptyExtraction` (`Types/Graph.hs:54`), the view with empty node set **and** empty edge set.

**Proposition (symmetric monoidal).** `(Extract, ⊗, emptyExtraction)` is a **symmetric monoidal category**:
- *Unit:* `mergeExtractions A emptyExtraction == A` and `mergeExtractions emptyExtraction A == A` (`Map.union` with `Map.empty` is the identity, on the nose) ⇒ `emptyExtraction` is a two-sided unit. It is also the **initial object** (the empty embedding maps uniquely into every extraction), so `(Extract, ⊗)` is a **cocartesian monoidal category**: the tensor is the coproduct and the unit is the initial (nullary-coproduct) object. (Note: `emptyExtraction` is initial but *not* terminal — there is no map from a non-empty extraction to it — so this is cocartesian, not a biproduct/zero setting.)
- *Associativity:* `(A ⊗ B) ⊗ C == A ⊗ (B ⊗ C)` — `Map.union` is associative on disjoint keys, on the nose.
- *Symmetry:* the swap `σ_{A,B} : A⊗B → B⊗A` is the canonical iso (disjoint sets swap); it satisfies `σ ∘ σ = id` and the hexagon identities trivially (all coherence maps are identities on disjoint data).

> **Universal property (coproduct).** In the disjoint-key regime `A ⊗ B` **is** the binary **coproduct** of `A` and `B` in `Extract`: it carries inclusions `ι_A : A ↪ A⊗B`, `ι_B : B ↪ A⊗B`, and for any pair of embeddings `f : A → C`, `g : B → C` there is a **unique** mediating embedding `[f,g] : A⊗B → C` with `[f,g]∘ι_A = f`, `[f,g]∘ι_B = g`. Unique up to unique iso. The monoidal unit `emptyExtraction` is the nullary coproduct (initial object). Hence `⊗` is the **coproduct monoidal structure**.

> **Constrains:** `mergeExtractions` (`Core.hs:129-136`, `Map.union` on nodes+edges), `emptyExtraction` (`Types/Graph.hs:54`). This **closes the AVI-511 §1.2(i) gap**: AVI-511 noted "coproduct" for disjoint-file merge but did not name the unit (`emptyExtraction`) nor the symmetry; here `⊗` is pinned as the coproduct monoidal structure with its universal property.

### 6.2 Where the monoidal reading stops (overlap → lax merge, not a violation)

**Honesty.** `mergeExtractions` is `Map.union`, which **deduplicates by key** (second-wins, AVI-524 §3.2). So on *overlapping* keys it is **not** the coproduct/disjoint union — it is the **lax colimit** of AVI-524 (§3.2). The symmetric-monoidal claim of §6.1 therefore holds **precisely on disjoint-key inputs** (independent contexts composed side-by-side = the "parallel composition" the issue targets); on overlapping keys the governing construction is the lax merge, and the two are related by the inclusion `A⊗B ↪ mergeExtractions A B` (disjoint ⊆ amalgamated). No universal-property violation: the coproduct pins disjoint inputs only.

> **Constrains:** `mergeExtractions` second-wins (`Core.hs:131-132`); inconsistent merge = lax colimit with `α` (AVI-524 R-RELAX). 🔧 Loop with [Graphos Dev](/AVI/agents/graphos-dev) to confirm that "parallel composition of independent contexts" is intended to mean **disjoint-key** merge (the regime where `⊗` is a true coproduct), distinct from the conflict-resolving lax merge.

---

## 7. Acceptance Criteria (all required by the issue)

### AC-1 — Sequential composition laws (on-the-nose assoc + unit)
For random **composable** walk triples `p,v→u`, `q,u→w`, `r,w→x` over `gEdges`: assert `(p·q)·r == p·(q·r)` (list `++` associative on the nose) and `ε_{src}·p == p == p·ε_{tgt}` (empty walk/unit). Also assert `WalkMonoid(v)` (§3.2): closed walks at `v` compose associatively with unit `ε_v`. Surface: `shortestPathWithCached` builds one `[NodeId]` list (`Query.hs:84`); `neighborhoodExpansion` walks edges (`UseCase/Query.hs:391`).

### AC-2 — Free-category universal property (`Π ⊣ U`)
Any edge-mapping `f : 𝓖 → U(C)` into the underlying quiver of a category `C` extends to a **unique** functor `f̂ : Π(𝓖) → C` (`f̂(e_1…e_k) = f(e_1)∘…∘f(e_k)`). Check: the canonical extension is the *only* compatible extension (no alternative), i.e. `Hom_Cat(Π(𝓖), C) ≅ Hom_Gra(𝓖, U(C))` on small fixtures. Structural/axiom check over `Data.List.(++)`. Surface: any path/routine mapping edges into a compositional structure factors uniquely through `Π(𝓖)` (§2.2).

### AC-3 — Reachability = preorder reflection (`ρ_G`)
For every `s,t` in a fixture: `shortestPath g s t == Just _` ⟺ `s ⤳ t` (independently computed as the reflexive-transitive closure of `E`). For every quiver homomorphism `f` between fixtures, `u ⤳_𝓖 v ⟹ f(u) ⤳_𝓗 f(v)` (`Rea` functorial, §4.2). For expansion: `neighborhoodExpansion s k g idx` returns exactly `B_k(s)` — no more, no less (§4.3).

### AC-4 — SCC condensation = coarsest symmetric quotient
On the AVI-518 §8 golden fixture: the partition equals `{[a,b,c]],[d,e],[f]}`, condensation arcs `{a,b,c}→{d,e}→{f}` and is **acyclic** (AVI-518 Thm 2.3); and for any monotone map from `(V,⤳)` into a partial order, it factors **uniquely** through `cond(G)` (§5.1). Property-test over random directed graphs: partition uniqueness + acyclic condensation + unique factorization.

### AC-5 — Symmetric monoidal parallel composition
For **disjoint-key** extractions `A,B`: `mergeExtractions A B == A ⊔ B` (disjoint union), associative `((A⊗B)⊗C == A⊗(B⊗C))`, unit `emptyExtraction`, swap iso `σ`. For **overlapping** keys: assert it is the **lax merge** (second-wins, AVI-524 §3.2), *not* the coproduct — i.e. `mergeExtractions A B ≠ disjointUnion(A,B)` when keys overlap, documenting the boundary (§6.2). 🔧 Confirm with Dev that disjoint-key is the intended "parallel composition" regime.

### AC-6 — Determinism (see §10)
Composition/reachability/condensation answers are invariant under any permutation of `gNodes`/`gEdges` keys (AVI-518 INV-9); the merge result under `foldl' mergeExtractions` is order-canonical (§10).

---

## 8. Requirements Traceability Table

| Req | Statement | Surface | Acceptance | Locked? |
|---|---|---|---|---|
| R-COMP | `Π ⊣ U` free-forgetful adjunction; edges extend uniquely into composition (§2.2) | quiver `𝓖`, `shortestPath` (`Query.hs:80`) | AC-2 (axiom check) | structural |
| R-SEQ | Sequential composition is strict assoc + unit on the nose (§3.1) | `gEdges`, `[NodeId]` walk (`Query.hs:84`) | AC-1 | structural |
| R-MONOID | Closed walks at `v` form `WalkMonoid(v)` (§3.2) | SCC self-loops, `gAdjFwd[v]∋v` | AC-1 | proposed |
| R-REACH | `⤳` = preorder reflection of `Π(G)`; `Rea : Gra → Preord` (§4) | `shortestPath` (`Query.hs:80`) | AC-3 | structural |
| R-EXPAND | Expansion = truncated reachability `B_k(s)`; natural filtration (§4.3) | `neighborhoodExpansion` (`UseCase/Query.hs:380`) | AC-3 | structural |
| R-SCC | `cond(G)` = coarsest symmetric quotient of `(V,⤳)`; dual to merge colimit (§5) | `stronglyConnectedComponents` (AVI-518 GAP-1) | AC-4 | structural |
| R-PARALLEL | `(Extract,⊗,emptyExtraction)` symmetric monoidal (= coproduct); holds on disjoint keys (§6) | `mergeExtractions` (`Core.hs:129`), `emptyExtraction` (`Types/Graph.hs:54`) | AC-5 | 🔧 loop w/ Dev |
| R-LAXBOUND | Overlap → lax merge (§6.2), not a coproduct violation | `mergeExtractions` second-wins (`Core.hs:131`) | AC-5 | 🔧 loop w/ Dev |

---

## 9. Prior Art

- **Free category / free-forgetful adjunction `Π ⊣ U`.** Mac Lane, *Category Theory for the Working Mathematician*, Ch. IV.1; Riehl, *Categorical Methods in Algebraic Topology*, Ch. 4 (this doc §2.2).
- **Preorder reflection `R ⊣ incl : Preord ↪ Cat`** and the **poset/T0 (Kolmogorov) reflection `Posord ↪ Preord`**; both are reflective, giving the coequalizer/universal-mapping statements (§4.1, §5.1) — standard (Borceux & Brumberg, *Category Theory*, vol. I; Street, *Categorical Structure*).
- **SCC condensation as a quotient DAG.** Tarjan (1972); Kosarajan (1978) — the combinatorial construction; categorically it is the T0 reflection of reachability (§5.1).
- **Coproduct = disjoint union; monoidal structure from coproduct.** The "cartesian/zero monoidal" shape where the unit is the initial object — standard (Mac Lane, Ch. V.3; Lack, *Fundamentals of Category Theory*). Graphos realizes the coproduct via `Map.union` on disjoint keys (`Core.hs:129`).
- **Lax colimit / lax natural transformation** for the overlap case (AVI-524 §3.2): Street *Fibrations and Yoneda's theorem in an n-category*; Street & Walters *Fibrations = Yoneda* — "commutes only up to a chosen non-invertible 2-cell `α`."
- **`Data.Map.union` second-wins.** `containers` library semantics, not invented here; the disjoint-key coproduct reading (§6.1) is the Graphos choice layered on top.

---

## 10. Determinism Lens

- **Composition / reachability / condensation answers** depend only on `(V,E)` (AVI-518 INV-8, INV-9), hence are invariant under any permutation of `gNodes`/`gEdges` keys — fully deterministic and representation-independent (§4.2, §5.1).
- **Sequential composition** is strict (on the nose, §3.1): `(p·q)·r == p·(q·r)` holds regardless of `Map` iteration/insertion order — no tie-break needed.
- **Parallel composition (`⊗`).** On disjoint keys `mergeExtractions` is associative + commutative up to canonical iso (sheaf local-to-global consistency, AVI-524 §10); the n-way colimit is a single `foldl' mergeExtractions` (linear, `O(Σ|Vᵢ|)`, AVI-524 AC-3). Under conflict, `Map.union` second-wins makes it order-dependent (§6.2) — a documented policy choice, tested (AC-5).
- **SCC labeling.** The *partition* is canonical/unique (AVI-518 Thm 2.2); only the integer community-labeling is order-dependent — constrain seeds to **ascending `NodeId`** (AVI-518 §9), consistent with `toCachedFGL` sequential indexing (`Analysis.hs:64`).
- **Expansion.** `B_k(s)` depends only on `d(s,·)` over `E`; invariant under key permutation (§4.3).

> **Constrains:** `toCachedFGL` (`Analysis.hs:64`) sequential indexing; `shortestPathWithCached` (`Query.hs:84`); `neighborhoodExpansion` (`UseCase/Query.hs:380`); `mergeExtractions` (`Core.hs:129`); new `stronglyConnectedComponents` seed loop (AVI-518 §9).

---

## 11. Next Actions / Open Items

1. **Loop the 🔧 requirements with [Graphos Dev](/AVI/agents/graphos-dev):** §6.1/AC-5 (that "parallel composition of independent contexts" means *disjoint-key* merge, distinct from the conflict-resolving lax merge of §6.2) and §3.2 (`WalkMonoid(v)` semantics for return-walk counts). Until confirmation, R-MONOID/R-PARALLEL/R-LAXBOUND are *proposed* semantics.
2. **Property tests (AC-1, AC-3, AC-5):** random composable triples ⇒ strict assoc+unit; `shortestPath Just ⟺ ⤳`; disjoint extractions ⇒ coproduct (`⊗`), overlapping ⇒ lax merge (second-wins).
3. **Golden-file test (AC-4):** reproduce the AVI-518 §8 partition/condensation and the unique-factorization-through-`cond(G)` property.
4. **SCC routine (AVI-518 GAP-1):** `stronglyConnectedComponents :: Graph -> Map Int [NodeId]` (+ `WithCached`) + `shortestPathReachable`; implement with ascending-`NodeId` seeds (§10).
5. **Consolidate notation** into `docs/math-requirements/consolidated-requirements.md` (§0) so AVI-518/511/524/525 share one glossary; add the composition objects (`Π(G)`, `WalkMonoid(v)`, `⊗`, `Rea`, `cond`) to the shared entries.
6. **Escalate** any category-level judgment above confidence to the Head of R&D (per execution contract): e.g. whether modeling `⤳` as the preorder reflection is sufficient vs. tracking *shortest-walk* metric structure (hop distance) in a weighted setting — currently `Rea` captures reachability only, not hop-minimality (AVI-518 AF-1).

---

## 12. Joint-Surface Note (with Graph Theory Expert)

Every construction in this doc makes a claim about the **nodes** `V` and **edges** `E` of `G`: `Π(G)` (walks), `Rea` (reachability), `cond(G)` (SCC quotient), `(Extract,⊗)` (disjoint union). The **combinatorial validation** of these claims — that `shortestPath`/`neighborhoodExpansion`/`stronglyConnectedComponents` actually compute the stated closures/quotients at `O(N+E)`, that the partition is unique, that the condensation is acyclic — is the Graph Theory Expert's acceptance bar ([AVI-518](/AVI/issues/AVI-518) §3–§5). CT pins *what* the objects and universal properties are (`Π ⊣ U`, preorder/poset reflection, coproduct); GT validates *that* the algorithms realize them. The two operate on identical `(V,E)` ([AVI-511](/AVI/issues/AVI-511) merge colimit and this doc's SCC quotient are dual on the same objects), so any discrepancy is resolved jointly, not by re-deriving CT.
