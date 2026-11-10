/-!
# IntentGraph — squelette Lean 4

Formalisation minimale de :
  * un graphe de code typé CodeGraph (export GraphOS) ;
  * une logique de specs Spec à quatre patterns (existence, absence,
    couverture, cohérence de structures parallèles) ;
  * une sémantique sat : CodeGraph → Spec → Prop **décidable**, donc le
    vérificateur check est decide et sa correction est un théorème d'une
    ligne (check_sound) ;
  * l'analyse d'impact d'un changement : nœuds touchés touched, fermeture
    impact, frontière frontier, obligations et complete ;
  * l'admissibilité d'un changement vis-à-vis d'un ensemble de specs.

Lean 4 core uniquement, pas de Mathlib. Compile avec Lean 4.29+/4.30
(lake build vert ; seul ajustement depuis le premier jet : le binder
prefix renommé pre, mot-clé Lean). Les exemples jouets, y compris la
détection de l'oubli schéma/contrat, sont acceptés par le noyau (by decide).
Les obligations O1–O4 sont démontrées (aucun sorry) — voir la section 6.
-/

namespace IntentGraph

/-! ## 1. Le graphe -/

/-- Types de nœuds. À étendre selon l'extracteur (config, i18n, IaC…). -/
inductive NodeKind where
  | handler | validator | test | schemaField | contractField | doc | module | other
  deriving DecidableEq, Repr

/-- Étiquettes d'arêtes. -/
inductive Label where
  | calls        -- a appelle b
  | implements   -- a implémente l'interface b
  | tests        -- le test a exerce b
  | references   -- a référence b par chaîne (route, clé de config, event…)
  | maps         -- a correspond à b dans une structure parallèle (schéma ↔ contrat)
  | dependsOn    -- dépendance de module / import
  deriving DecidableEq, Repr

/-- Un nœud. sig est la signature observable (nom + types) et body un hash
    du contenu : c'est ce qui permet la règle « corps changé, signature stable ». -/
structure Node where
  id   : Nat
  kind : NodeKind
  name : String
  sig  : String := ""
  body : String := ""
  deriving DecidableEq, Repr

structure Edge where
  src : Nat
  lbl : Label
  dst : Nat
  deriving DecidableEq, Repr

structure CodeGraph where
  nodes : List Node
  edges : List Edge
  deriving Repr

namespace CodeGraph

def ids (G : CodeGraph) : List Nat := G.nodes.map (·.id)

def node? (G : CodeGraph) (v : Nat) : Option Node := G.nodes.find? (·.id = v)

/-- Successeurs directs de v par l'étiquette ℓ. -/
def succ (G : CodeGraph) (ℓ : Label) (v : Nat) : List Nat :=
  G.edges.filterMap fun e => if e.src = v ∧ e.lbl = ℓ then some e.dst else none

/-- Une étape de saturation : S ∪ succ(S). -/
def step (G : CodeGraph) (ℓ : Label) (S : List Nat) : List Nat :=
  (S ++ S.flatMap (G.succ ℓ)).eraseDups

/-- Fermeture réflexive-transitive de S par ℓ.
    Fuel = |V| : tout chemin simple a au plus |V| arêtes, donc |V| étapes
    suffisent à saturer. C'est cette borne que reach_complete doit justifier. -/
def reachSet (G : CodeGraph) (ℓ : Label) (S : List Nat) : List Nat :=
  (List.range G.nodes.length).foldl (fun acc _ => G.step ℓ acc) S

/-- u atteint v par un chemin étiqueté ℓ (calcul). -/
def reach (G : CodeGraph) (ℓ : Label) (u v : Nat) : Bool :=
  (G.reachSet ℓ [u]).contains v

/-- u atteint v (définition inductive de référence, ce que reach doit
    capturer). -/
inductive Path (G : CodeGraph) (ℓ : Label) : Nat → Nat → Prop
  | refl (u : Nat) : Path G ℓ u u
  | step {u v w : Nat} : Edge.mk u ℓ v ∈ G.edges → Path G ℓ v w → Path G ℓ u w

end CodeGraph

/-! ## 2. Les specs : quatre patterns -/

inductive Spec where
  /-- Existence : il existe un nœud de type k nommé name qui atteint, via
      des arêtes ℓ, un nœud de type k'.
      Ex. : le handler POST /orders appelle (transitivement) un validateur. -/
  | reaches (k : NodeKind) (name : String) (ℓ : Label) (k' : NodeKind)
  /-- Absence : aucun nœud de type k dont le nom commence par pre, hors
      liste blanche. Ex. : plus aucune référence à l'ancien SDK.
      (Binder nommé pre : prefix est un mot-clé Lean.) -/
  | absent (k : NodeKind) (pre : String) (whitelist : List String)
  /-- Couverture : tout nœud de type k est atteint via ℓ par un nœud de
      type k'. Ex. : tout handler est exercé par un test. -/
  | covered (k : NodeKind) (ℓ : Label) (k' : NodeKind)
  /-- Cohérence de structures parallèles : correspondance totale dans les deux
      sens entre les nœuds de type a et ceux de type b via ℓ.
      Ex. : tout champ de schéma a un champ de contrat et réciproquement.
      (L'unicité — bijection stricte — s'ajoute comme raffinement.) -/
  | parallel (a b : NodeKind) (ℓ : Label)
  | and (φ ψ : Spec)
  | not (φ : Spec)
  deriving Repr

open CodeGraph

/-- Sémantique. Tous les quantificateurs sont bornés par G.nodes, donc
    la proposition est décidable et le vérificateur est decide. -/
def sat (G : CodeGraph) : Spec → Prop
  | .reaches k name ℓ k' =>
      ∃ u ∈ G.nodes, u.kind = k ∧ u.name = name ∧
        ∃ w ∈ G.nodes, w.kind = k' ∧ G.reach ℓ u.id w.id = true
  | .absent k pre wl =>
      ∀ u ∈ G.nodes, u.kind = k → u.name.startsWith pre = true → u.name ∈ wl
  | .covered k ℓ k' =>
      ∀ u ∈ G.nodes, u.kind = k →
        ∃ w ∈ G.nodes, w.kind = k' ∧ G.reach ℓ w.id u.id = true
  | .parallel a b ℓ =>
      (∀ u ∈ G.nodes, u.kind = a → ∃ w ∈ G.nodes, w.kind = b ∧ Edge.mk u.id ℓ w.id ∈ G.edges) ∧
      (∀ w ∈ G.nodes, w.kind = b → ∃ u ∈ G.nodes, u.kind = a ∧ Edge.mk u.id ℓ w.id ∈ G.edges)
  | .and φ ψ => sat G φ ∧ sat G ψ
  | .not φ   => ¬ sat G φ

instance decSat (G : CodeGraph) : (φ : Spec) → Decidable (sat G φ)
  | .reaches .. => by unfold sat; exact inferInstance
  | .absent ..  => by unfold sat; exact inferInstance
  | .covered .. => by unfold sat; exact inferInstance
  | .parallel .. => by unfold sat; exact inferInstance
  | .and φ ψ => by unfold sat; exact @instDecidableAnd _ _ (decSat G φ) (decSat G ψ)
  | .not φ   => by unfold sat; exact @instDecidableNot _ (decSat G φ)

/-- Le vérificateur certifié : c'est littéralement la procédure de décision. -/
def check (G : CodeGraph) (φ : Spec) : Bool := decide (sat G φ)

/-- Correction : si check dit oui, la spec est vraie dans le modèle. -/
theorem check_sound {G : CodeGraph} {φ : Spec} : check G φ = true → sat G φ :=
  of_decide_eq_true

/-- Complétude : si la spec est vraie, check dit oui. -/
theorem check_complete {G : CodeGraph} {φ : Spec} : sat G φ → check G φ = true :=
  decide_eq_true

/-! ## 3. Changement, impact, frontière, obligations -/

/-- Un changement = graphe avant / graphe après. La correspondance μ est
    l'identité sur Node.id (identifiant stable fourni par GraphOS). -/
structure Change where
  before : CodeGraph
  after  : CodeGraph

namespace Change

def outEdges (G : CodeGraph) (v : Nat) : List Edge := G.edges.filter (·.src = v)

def sameEdgeSet (E₁ E₂ : List Edge) : Bool :=
  E₁.all E₂.contains && E₂.all E₁.contains

/-- Nœuds touchés T : supprimés, ajoutés, modifiés (attributs ou arêtes sortantes). -/
def touched (c : Change) : List Nat :=
  let removed := c.before.ids.filter fun v => (c.after.node? v).isNone
  let added   := c.after.ids.filter fun v => (c.before.node? v).isNone
  let changed := c.before.ids.filter fun v =>
    match c.before.node? v, c.after.node? v with
    | some n₁, some n₂ =>
        decide (n₁ ≠ n₂) || !sameEdgeSet (outEdges c.before v) (outEdges c.after v)
    | _, _ => false
  (removed ++ added ++ changed).eraseDups

end Change

/-- Étiquettes qui propagent un changement de la cible vers la source :
    si a --ℓ--> b et b change, alors a est impacté. -/
def depLabels : List Label := [.calls, .implements, .references, .maps, .dependsOn]

namespace CodeGraph

/-- Prédécesseurs de v par une arête de dépendance : D⁻¹(v). -/
def impactedBy (G : CodeGraph) (v : Nat) : List Nat :=
  G.edges.filterMap fun e =>
    if e.dst = v ∧ e.lbl ∈ depLabels then some e.src else none

def impactStep (G : CodeGraph) (S : List Nat) : List Nat :=
  (S ++ S.flatMap G.impactedBy).eraseDups

/-- I = D*(T). -/
def impact (G : CodeGraph) (T : List Nat) : List Nat :=
  (List.range G.nodes.length).foldl (fun acc _ => G.impactStep acc) T

/-- F = I \ T : atteint par propagation, non modifié. Les suspects. -/
def frontier (G : CodeGraph) (T : List Nat) : List Nat :=
  (G.impact T).filter fun v => !T.contains v

end CodeGraph

/-- Une règle de non-affectation : r c v = true certifie que le nœud v de
    la frontière n'est pas affecté par le changement c. Chaque règle doit
    être accompagnée d'un argument de sûreté (voir obligations). -/
abbrev Rule := Change → Nat → Bool

/-- Règle exemple : tous les nœuds touchés dont v dépend directement ont
    gardé leur signature ; seul leur corps a changé. Un appelant n'est pas
    affecté par un changement de corps à signature stable. -/
def stableSignature : Rule := fun c v =>
  let T := c.touched
  let deps := c.before.edges.filterMap fun e =>
    if e.src = v ∧ e.lbl ∈ depLabels ∧ T.contains e.dst then some e.dst else none
  deps.all fun d =>
    match c.before.node? d, c.after.node? d with
    | some n₁, some n₂ => decide (n₁.sig = n₂.sig ∧ n₁.kind = n₂.kind ∧ n₁.name = n₂.name)
    | _, _ => false

/-- Moyens de décharge disponibles pour un changement donné. -/
structure Discharge where
  acks  : List Nat    -- acquittements humains explicites
  rules : List Rule   -- règles de non-affectation admises

/-- ob(v) : l'obligation du nœud frontière v est déchargée. -/
def discharged (c : Change) (d : Discharge) (v : Nat) : Bool :=
  d.acks.contains v || d.rules.any fun r => r c v

/-- Obligations restantes : ce que la review doit regarder. -/
def openObligations (c : Change) (d : Discharge) : List Nat :=
  (c.before.frontier c.touched).filter fun v => !discharged c d v

/-- Complete(G, G') ⟺ ∀ v ∈ F, ob(v). -/
def complete (c : Change) (d : Discharge) : Bool :=
  (c.before.frontier c.touched).all (discharged c d)

/-! ## 4. Admissibilité d'un changement -/

/-- Le changement est complet, la nouvelle spec est vraie après, toutes les
    specs déjà validées le restent. -/
def admissible (c : Change) (d : Discharge) (Φ : List Spec) (φnew : Spec) : Bool :=
  complete c d && check c.after φnew && Φ.all (check c.after)

/-- Non-trivialité sur un jeu fini de graphes-sondes : la spec en accepte au
    moins un et en rejette au moins un. Une spec qui accepte tout ou rejette
    tout ne mesure rien. (Version bornée de « ni valide ni insatisfiable ».) -/
def nontrivial (probes : List CodeGraph) (φ : Spec) : Bool :=
  probes.any (check · φ) && probes.any (fun G => !check G φ)

/-! ## 5. Instance jouet -/

section Toy

/-- Avant : un handler, un validateur, un test, un champ de schéma et son
    champ de contrat. -/
def G₀ : CodeGraph :=
  { nodes :=
      [ { id := 1, kind := .handler,       name := "POST /orders", sig := "(Req) -> Res" }
      , { id := 2, kind := .validator,     name := "validateOrder", sig := "(Order) -> Result", body := "h1" }
      , { id := 3, kind := .test,          name := "orders.spec" }
      , { id := 4, kind := .schemaField,   name := "orders.total" }
      , { id := 5, kind := .contractField, name := "Order.total" } ]
    edges :=
      [ ⟨1, .calls, 2⟩, ⟨3, .tests, 1⟩, ⟨4, .maps, 5⟩ ] }

/-- Après, changement A : on modifie le corps du validateur (signature stable)
    ET on ajoute un champ de schéma orders.currency sans champ de contrat.
    C'est l'oubli typique. -/
def G₁ : CodeGraph :=
  { nodes :=
      [ { id := 1, kind := .handler,       name := "POST /orders", sig := "(Req) -> Res" }
      , { id := 2, kind := .validator,     name := "validateOrder", sig := "(Order) -> Result", body := "h2" }
      , { id := 3, kind := .test,          name := "orders.spec" }
      , { id := 4, kind := .schemaField,   name := "orders.total" }
      , { id := 5, kind := .contractField, name := "Order.total" }
      , { id := 6, kind := .schemaField,   name := "orders.currency" } ]
    edges :=
      [ ⟨1, .calls, 2⟩, ⟨3, .tests, 1⟩, ⟨4, .maps, 5⟩ ] }

/-- Après, changement B : même chose, oubli corrigé. -/
def G₂ : CodeGraph :=
  { G₁ with
    nodes := G₁.nodes ++ [ { id := 7, kind := .contractField, name := "Order.currency" } ]
    edges := G₁.edges ++ [ ⟨6, .maps, 7⟩ ] }

def specHandlerValidates : Spec := .reaches .handler "POST /orders" .calls .validator
def specHandlersTested   : Spec := .covered .handler .tests .test
def specSchemaContract   : Spec := .parallel .schemaField .contractField .maps
def specNoLegacy         : Spec := .absent .module "legacy/" []

def Φ : List Spec := [specHandlerValidates, specHandlersTested, specSchemaContract, specNoLegacy]

def cA : Change := ⟨G₀, G₁⟩
def cB : Change := ⟨G₀, G₂⟩
def d  : Discharge := ⟨[], [stableSignature]⟩

-- Les specs individuelles, prouvées par le noyau (pas par #eval).
example : sat G₀ specHandlerValidates := by decide
example : sat G₀ specSchemaContract   := by decide
example : ¬ sat G₁ specSchemaContract := by decide   -- l'oubli est détecté
example : sat G₂ specSchemaContract   := by decide

-- Analyse d'impact du changement A.
#eval cA.touched                       -- [6, 2]      : champ ajouté, validateur modifié
#eval G₀.frontier cA.touched           -- [1]         : le handler appelle le validateur
#eval openObligations cA d             -- []          : déchargé par stableSignature
#eval complete cA d                    -- true
#eval admissible cA d Φ specHandlerValidates
                                       -- false       : specSchemaContract cassée après
#eval Φ.filter fun φ => !check G₁ φ    -- [parallel schemaField contractField maps]

-- Changement B : tout passe.
#eval admissible cB d Φ specHandlerValidates   -- true
example : admissible cB d Φ specHandlerValidates = true := by decide

-- Non-trivialité de specSchemaContract sur les sondes disponibles.
#eval nontrivial [G₀, G₁, G₂] specSchemaContract   -- true

end Toy

/-! ## 5bis. Boîte à outils : clôture saturée sur une carte de successeurs -/

section ClosureToolkit
open CodeGraph

/-- Une étape de saturation pour la carte de successeurs `f` :
    `S ↦ dedup(S ∪ ⋃ f[S])`. C'est le moule commun de `step` et `impactStep`. -/
private def stepF (f : Nat → List Nat) (S : List Nat) : List Nat :=
  (S ++ S.flatMap f).eraseDups

/-- `k` étapes de saturation depuis la graine `S` (le pli du bas vers le haut :
    `iterF (k+1) S = stepF (iterF f k S)`). -/
private def iterF (f : Nat → List Nat) : Nat → List Nat → List Nat
  | 0, S => S
  | k + 1, S => stepF f (iterF f k S)

/-- Les deux compositions coïncident. -/
theorem iterF_step (f : Nat → List Nat) (k : Nat) (S : List Nat) :
    iterF f k (stepF f S) = iterF f (k + 1) S := by
  induction k with
  | zero => rfl
  | succ n ih =>
    show stepF f (iterF f n (stepF f S)) = stepF f (iterF f (n + 1) S)
    rw [ih]

/-- Le pli sur `List.range k` calcule exactement `iterF k` : les indices du
    pli sont ignorés, seule la fonction d'accumulateur compte. C'est le pont
    `reachSet`/`impact` (plis sur `range |V|`) → `iterF`. -/
theorem foldl_range_iterF {f : Nat → List Nat} {g : List Nat → Nat → List Nat}
    (hg : ∀ S n, g S n = stepF f S) (k : Nat) (S : List Nat) :
    (List.range k).foldl g S = iterF f k S := by
  induction k generalizing S with
  | zero => rfl
  | succ n ih =>
    rw [List.range_succ, List.foldl_append, List.foldl_cons, List.foldl_nil, hg, ih S]
    rfl

theorem mem_stepF_of_mem {f : Nat → List Nat} {S : List Nat} {v : Nat} (h : v ∈ S) :
    v ∈ stepF f S := by
  exact List.mem_eraseDups.mpr (List.mem_append.mpr (Or.inl h))

theorem mem_stepF_of_succ {f : Nat → List Nat} {S : List Nat} {u v : Nat}
    (hu : u ∈ S) (h : v ∈ f u) : v ∈ stepF f S := by
  refine List.mem_eraseDups.mpr ?_
  refine List.mem_append.mpr (Or.inr ?_)
  exact List.mem_flatMap.mpr ⟨u, hu, h⟩

theorem mem_of_mem_stepF {f : Nat → List Nat} {S : List Nat} {v : Nat}
    (h : v ∈ stepF f S) : v ∈ S ∨ ∃ u ∈ S, v ∈ f u := by
  have h1 : v ∈ S ++ S.flatMap f := List.mem_eraseDups.mp h
  rcases List.mem_append.mp h1 with h1' | h1'
  · exact Or.inl h1'
  · rcases List.mem_flatMap.mp h1' with ⟨u, hu, hv⟩
    exact Or.inr ⟨u, hu, hv⟩

theorem stepF_mono {f : Nat → List Nat} {S₁ S₂ : List Nat}
    (h : ∀ x ∈ S₁, x ∈ S₂) {v : Nat} (hv : v ∈ stepF f S₁) : v ∈ stepF f S₂ := by
  rcases mem_of_mem_stepF hv with h1 | ⟨u, hu, hx⟩
  · exact mem_stepF_of_mem (h _ h1)
  · exact mem_stepF_of_succ (h _ hu) hx

theorem iterF_mono {f : Nat → List Nat} (S₀ : List Nat) :
    ∀ (i j : Nat) (v : Nat), v ∈ iterF f j S₀ → v ∈ iterF f (j + i) S₀ := by
  intro i
  induction i with
  | zero => intro j v h; exact h
  | succ d ih =>
    intro j v h
    show v ∈ iterF f ((j + d) + 1) S₀
    exact mem_stepF_of_mem (ih j v h)

theorem iterF_seed_mono {f : Nat → List Nat} :
    ∀ (k : Nat) (S₁ S₂ : List Nat), (∀ x ∈ S₁, x ∈ S₂) →
      ∀ v ∈ iterF f k S₁, v ∈ iterF f k S₂ := by
  intro k
  induction k with
  | zero => intro _ _ h _ hv; exact h _ hv
  | succ d ih =>
    intro S₁ S₂ h v hv
    rcases mem_of_mem_stepF hv with h1 | ⟨u, hu, hx⟩
    · exact mem_stepF_of_mem (ih S₁ S₂ h v h1)
    · exact mem_stepF_of_succ (ih S₁ S₂ h u hu) hx

/-- Décalage : un nœud atteint au pas `j` engendre, comme graine, un sous-ensemble
    de l'itéré `j + k`. -/
theorem iterF_shift {f : Nat → List Nat} {S : List Nat} {x j : Nat} (h : x ∈ iterF f j S) :
    ∀ (k x' : Nat), x' ∈ iterF f k [x] → x' ∈ iterF f (j + k) S := by
  intro k
  induction k with
  | zero =>
    intro x' hx'
    rcases List.mem_singleton.mp hx' with rfl
    rw [Nat.add_zero]
    exact h
  | succ d ih =>
    intro x' hx'
    show x' ∈ iterF f ((j + d) + 1) S
    rcases mem_of_mem_stepF hx' with h1 | ⟨u, hu, hxu⟩
    · exact mem_stepF_of_mem (ih x' h1)
    · exact mem_stepF_of_succ (ih u hu) hxu

/-- Sous bonne formation de la carte des successeurs et de la graine, tous les
    itérés restent dans l'univers `ids`. -/
theorem iterF_stays {f : Nat → List Nat} {ids : List Nat}
    (hsucc : ∀ u, ∀ x ∈ f u, x ∈ ids) (S : List Nat) (hS : ∀ x ∈ S, x ∈ ids) :
    ∀ (k v : Nat), v ∈ iterF f k S → v ∈ ids := by
  intro k
  induction k with
  | zero => intro _ hv; exact hS _ hv
  | succ d ih =>
    intro v hv
    rcases mem_of_mem_stepF hv with h1 | ⟨u, _, hx⟩
    · exact ih v h1
    · exact hsucc u _ hx

/-- Variante à graine libre : les membres d'un itéré viennent de la graine
    (utile quand la graine n'est pas supposée dans l'univers) ou de l'univers. -/
theorem iterF_stays2 {f : Nat → List Nat} {ids : List Nat}
    (hsucc : ∀ u, ∀ x ∈ f u, x ∈ ids) (S₀ : List Nat) :
    ∀ (k v : Nat), v ∈ iterF f k S₀ → v ∈ S₀ ∨ v ∈ ids := by
  intro k
  induction k with
  | zero => intro v hv; exact Or.inl hv
  | succ d ih =>
    intro v hv
    rcases mem_of_mem_stepF hv with h1 | ⟨u, _, hx⟩
    · exact ih v h1
    · exact Or.inr (hsucc u _ hx)

/-- `cnt L U` compte les éléments de `U` qui appartiennent à `L` :
    la mesure discrète qui pilote l'argument de saturation. -/
private def cnt (L U : List Nat) : Nat :=
  match U with
  | [] => 0
  | x :: xs => if x ∈ L then cnt L xs + 1 else cnt L xs

theorem cnt_le_length (L U : List Nat) : cnt L U ≤ U.length := by
  induction U with
  | nil => exact Nat.zero_le _
  | cons x xs ih =>
    simp only [cnt, List.length_cons]
    split <;> omega

theorem cnt_mono {L₁ L₂ : List Nat} (h : ∀ x ∈ L₁, x ∈ L₂) :
    ∀ U, cnt L₁ U ≤ cnt L₂ U := by
  intro U
  induction U with
  | nil => exact Nat.zero_le _
  | cons x xs ih =>
    simp only [cnt]
    rcases Classical.em (x ∈ L₁) with h1 | h1
    · rcases Classical.em (x ∈ L₂) with h2 | h2
      · rw [if_pos h1, if_pos h2]; exact Nat.succ_le_succ ih
      · exact absurd (h x h1) h2
    · rcases Classical.em (x ∈ L₂) with h2 | h2
      · rw [if_neg h1, if_pos h2]; omega
      · rw [if_neg h1, if_neg h2]; exact ih

theorem one_le_cnt {L U : List Nat} {x : Nat} (hx : x ∈ U) (hL : x ∈ L) : 1 ≤ cnt L U := by
  induction U with
  | nil => exact absurd hx List.not_mem_nil
  | cons y ys ih =>
    rcases List.mem_cons.mp hx with rfl | hx'
    · simp only [cnt]; rw [if_pos hL]; omega
    · simp only [cnt]; split
      · omega
      · exact ih hx'

/-- Ajouter `v ∉ L` à la graine fait croître la mesure d'au moins un,
    dès que `v` est dans l'univers. -/
theorem cnt_cons_mem {L : List Nat} {v : Nat} (hv : v ∉ L) :
    ∀ (U : List Nat), v ∈ U → cnt L U + 1 ≤ cnt (v :: L) U := by
  intro U hU
  induction U with
  | nil => exact absurd hU List.not_mem_nil
  | cons x xs ih =>
    have hmono : cnt L xs ≤ cnt (v :: L) xs :=
      cnt_mono (fun y hy => List.mem_cons_of_mem v hy) xs
    by_cases hEq : x = v
    · subst hEq
      have hxL : x ∉ L := hv
      have hxmem : x ∈ x :: L := List.mem_cons_self
      simp only [cnt]
      rw [if_neg hxL, if_pos hxmem]
      omega
    · have hiff : x ∈ v :: L ↔ x ∈ L := by simp [List.mem_cons, hEq]
      have hv' : v ∈ xs := by
        rcases List.mem_cons.mp hU with h | h
        · exact absurd h.symm hEq
        · exact h
      simp only [cnt]
      rcases Classical.em (x ∈ L) with h1 | h1
      · rw [if_pos h1, if_pos (hiff.mpr h1)]
        have h1' : cnt L xs + 1 ≤ cnt (v :: L) xs := ih hv'
        omega
      · rw [if_neg h1, if_neg (fun hmem => h1 (hiff.mp hmem))]
        have h1' : cnt L xs + 1 ≤ cnt (v :: L) xs := ih hv'
        omega

theorem cnt_growth {f : Nat → List Nat} {ids : List Nat} {L : List Nat} {v : Nat}
    (hvids : v ∈ ids) (hstep : v ∈ stepF f L) (hnot : v ∉ L) :
    cnt L ids + 1 ≤ cnt (stepF f L) ids := by
  have h1 : cnt (v :: L) ids ≤ cnt (stepF f L) ids :=
    cnt_mono (by
      intro y hy
      rcases List.mem_cons.mp hy with rfl | hy'
      · exact hstep
      · exact mem_stepF_of_mem hy') ids
  have h2 : cnt L ids + 1 ≤ cnt (v :: L) ids := cnt_cons_mem hnot ids hvids
  omega

/-- Du non-pour-tous à l'existence d'un contre-exemple (classique, cœur Lean). -/
theorem exists_not_of_not_forall {α : Sort u} {p : α → Prop} (h : ¬ ∀ a, p a) : ∃ a, ¬ p a := by
  rcases Classical.em (∃ a, ¬ p a) with hex | hnex
  · exact hex
  · have hall : ∀ a, p a := fun a => Classical.byContradiction (fun hna => hnex ⟨a, hna⟩)
    exact absurd hall h

/-- **Pigeonnier par croissance discrète.** Tant que l'itéré n'est pas saturé,
    une étape ajoute au moins un nouvel élément de l'univers, donc fait
    croître `cnt` d'au moins un ; comme `cnt ≤ |univers|`, l'itéré est saturé
    à un rang `j ≤ |univers|`. -/
theorem closure_point_exists {f : Nat → List Nat} {ids : List Nat}
    (hsucc : ∀ u, ∀ x ∈ f u, x ∈ ids) (S₀ : List Nat) (hS₀ : ∀ x ∈ S₀, x ∈ ids) :
    ∃ j, j ≤ ids.length ∧ ∀ x ∈ stepF f (iterF f j S₀), x ∈ iterF f j S₀ := by
  have hkey : ∀ m, cnt (iterF f m S₀) ids ≥ cnt S₀ ids + m ∨
      ∃ j, j ≤ m ∧ (∀ x ∈ stepF f (iterF f j S₀), x ∈ iterF f j S₀) := by
    intro m
    induction m with
    | zero => exact Or.inl (Nat.le_of_eq rfl)
    | succ m ih =>
      rcases ih with h1 | ⟨j, hj, hc⟩
      · rcases Classical.em (∀ x ∈ stepF f (iterF f m S₀), x ∈ iterF f m S₀) with hc0 | hc0
        · exact Or.inr ⟨m, Nat.le_succ _, hc0⟩
        · obtain ⟨v, hv1, hv0⟩ : ∃ v, v ∈ stepF f (iterF f m S₀) ∧ v ∉ iterF f m S₀ := by
            have h1' : ¬ (∀ v, v ∈ stepF f (iterF f m S₀) → v ∈ iterF f m S₀) := hc0
            obtain ⟨v, hvn⟩ := exists_not_of_not_forall h1'
            rcases Classical.em (v ∈ stepF f (iterF f m S₀)) with hm | hm
            · exact ⟨v, hm, fun hmem => hvn (fun _ => hmem)⟩
            · exfalso
              exact hvn (fun hmem' => absurd hmem' hm)
          have hvids : v ∈ ids := by
            rcases mem_of_mem_stepF hv1 with h | ⟨u, _, hx⟩
            · exact absurd h hv0
            · exact hsucc u _ hx
          have hg : cnt (iterF f m S₀) ids + 1 ≤ cnt (stepF f (iterF f m S₀)) ids :=
            cnt_growth hvids hv1 hv0
          refine Or.inl ?_
          show cnt (stepF f (iterF f m S₀)) ids ≥ cnt S₀ ids + (m + 1)
          omega
      · exact Or.inr ⟨j, Nat.le_trans hj (Nat.le_succ _), hc⟩
  rcases hkey ids.length with h1 | ⟨j, hj, hc⟩
  · rcases Classical.em (∃ x, x ∈ S₀) with ⟨x, hx⟩ | hno
    · exfalso
      have hpos : 1 ≤ cnt S₀ ids := one_le_cnt (hS₀ x hx) hx
      have hle := cnt_le_length (iterF f ids.length S₀) ids
      omega
    · exact ⟨0, Nat.zero_le _, by
        intro x hx
        rcases mem_of_mem_stepF hx with h | ⟨u, hu, _⟩
        · exact absurd ⟨x, h⟩ hno
        · exact absurd ⟨u, hu⟩ hno⟩
  · exact ⟨j, hj, hc⟩

/-- À partir d'un rang saturé, l'itéré ne change plus (vers le bas). -/
theorem iterF_down {f : Nat → List Nat} (S₀ : List Nat) {j : Nat}
    (hc : ∀ x ∈ stepF f (iterF f j S₀), x ∈ iterF f j S₀) :
    ∀ (M : Nat), j ≤ M → ∀ x ∈ iterF f M S₀, x ∈ iterF f j S₀ := by
  have hdown : ∀ (i x : Nat), x ∈ iterF f (j + i) S₀ → x ∈ iterF f j S₀ := by
    intro i
    induction i with
    | zero => intro x h; rwa [Nat.add_zero] at h
    | succ d ih =>
      intro x h
      rcases mem_of_mem_stepF h with h1 | ⟨u, hu, hx⟩
      · exact ih x h1
      · exact hc _ (mem_stepF_of_succ (ih u hu) hx)
  intro M hM x hx
  have hsplit : M = j + (M - j) := (Nat.add_sub_cancel' hM).symm
  rw [hsplit] at hx
  exact hdown (M - j) x hx

/-- **Saturation à `|univers|`.** Un pas de plus que `|univers|` n'apporte rien :
    c'est la justification ultime du combustible `|V|` de `reachSet` et `impact`. -/
theorem iterF_sat {f : Nat → List Nat} {ids : List Nat}
    (hsucc : ∀ u, ∀ x ∈ f u, x ∈ ids) {S₀ : List Nat} (hS₀ : ∀ x ∈ S₀, x ∈ ids) :
    ∀ x ∈ iterF f (ids.length + 1) S₀, x ∈ iterF f ids.length S₀ := by
  obtain ⟨j, hj, hc⟩ := closure_point_exists hsucc S₀ hS₀
  intro x hx
  have h1 : x ∈ iterF f j S₀ := iterF_down S₀ hc (ids.length + 1) (by omega) x hx
  have h2 : x ∈ iterF f (j + (ids.length - j)) S₀ :=
    iterF_mono S₀ (ids.length - j) j x h1
  rwa [Nat.add_sub_cancel' hj] at h2

end ClosureToolkit

/-! ## 5ter. Graphe concret : arêtes, successeurs, prédécesseurs, itérés -/

section ConcreteGraph
open CodeGraph

theorem mem_succ_iff_edge {G : CodeGraph} {ℓ : Label} {u v : Nat} :
    v ∈ G.succ ℓ u ↔ ∃ e ∈ G.edges, e.src = u ∧ e.lbl = ℓ ∧ e.dst = v := by
  constructor
  · intro h
    rcases List.mem_filterMap.mp h with ⟨e, he, hf⟩
    by_cases hcond : e.src = u ∧ e.lbl = ℓ
    · rw [if_pos hcond] at hf
      injection hf with hdst
      exact ⟨e, he, hcond.1, hcond.2, hdst⟩
    · rw [if_neg hcond] at hf
      exact absurd hf (by simp)
  · rintro ⟨e, he, hs, hl, hd⟩
    refine List.mem_filterMap.mpr ⟨e, he, ?_⟩
    rw [if_pos ⟨hs, hl⟩]
    simp [hd]

theorem edge_succ_mem {G : CodeGraph} {ℓ : Label} {u v : Nat} (h : Edge.mk u ℓ v ∈ G.edges) :
    v ∈ G.succ ℓ u := by
  refine List.mem_filterMap.mpr ⟨Edge.mk u ℓ v, h, ?_⟩
  rw [if_pos ⟨rfl, rfl⟩]

theorem succ_mem_ids {G : CodeGraph} {ℓ : Label}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids) :
    ∀ u x : Nat, x ∈ G.succ ℓ u → x ∈ G.ids := by
  intro u x hx
  rcases mem_succ_iff_edge.mp hx with ⟨e, he, _, _, hdst⟩
  have h2 := (hwf e he).2
  rwa [hdst] at h2

/-- Concaténation d'un arc au bout d'un chemin (Path ne croît que par
    préfixation, il faut la fermeture par suffixation). -/
theorem path_app {G : CodeGraph} {ℓ : Label} {u v w : Nat}
    (p : Path G ℓ u v) (h : Edge.mk v ℓ w ∈ G.edges) : Path G ℓ u w := by
  revert h
  induction p with
  | refl u => intro h; exact Path.step h (Path.refl w)
  | @step a b c h' p' ih => intro h; exact Path.step h' (ih h)

/-- Itéré vers chemin (O1) : ce que le calcul trouve correspond à un vrai
    chemin inductif, sans hypothèse de formation. -/
theorem iterF_mem_path {G : CodeGraph} {ℓ : Label} :
    ∀ (k : Nat) (S : List Nat) (v : Nat), v ∈ iterF (G.succ ℓ) k S →
      ∃ s, s ∈ S ∧ Path G ℓ s v := by
  intro k
  induction k with
  | zero => intro S v h; exact ⟨v, h, Path.refl v⟩
  | succ d ih =>
    intro S v h
    rcases mem_of_mem_stepF h with h1 | ⟨u, hu, hx⟩
    · obtain ⟨s, hs, hp⟩ := ih S v h1; exact ⟨s, hs, hp⟩
    · obtain ⟨s, hs, hp⟩ := ih S u hu
      have he : Edge.mk u ℓ v ∈ G.edges := by
        obtain ⟨e, hem, hsrc, hlbl, hdst⟩ := mem_succ_iff_edge.mp hx
        have heq : Edge.mk e.src e.lbl e.dst = Edge.mk u ℓ v := by
          rw [hsrc, hlbl, hdst]
        have heq₀ : e = Edge.mk u ℓ v := by rw [← heq]
        rw [heq₀] at hem
        exact hem
      exact ⟨s, hs, path_app hp he⟩

/-- La liste des ids a autant d'éléments que la liste des nœuds. -/
theorem ids_length (G : CodeGraph) : G.ids.length = G.nodes.length := by
  simp [CodeGraph.ids]

/-- Un id déclaré désigne un nœud (premier nœud portant cet id). -/
theorem ids_node? {G : CodeGraph} {v : Nat} (h : v ∈ G.ids) :
    ∃ n : Node, G.node? v = some n := by
  rw [CodeGraph.ids, List.mem_map] at h
  obtain ⟨n, hn, hid⟩ := h
  have hne : G.node? v ≠ none := by
    intro hnone
    rw [CodeGraph.node?, List.find?_eq_none] at hnone
    exact hnone n hn (by simp [hid])
  rw [Option.ne_none_iff_exists] at hne
  obtain ⟨m, hm⟩ := hne
  exact ⟨m, hm.symm⟩

theorem reachSet_eq_iterF (G : CodeGraph) (ℓ : Label) (S : List Nat) :
    G.reachSet ℓ S = iterF (G.succ ℓ) G.nodes.length S := by
  unfold reachSet
  exact foldl_range_iterF (fun _ _ => rfl) _ _

theorem mem_impactedBy_iff {G : CodeGraph} {u v : Nat} :
    u ∈ G.impactedBy v ↔ ∃ e ∈ G.edges, e.src = u ∧ e.lbl ∈ depLabels ∧ e.dst = v := by
  constructor
  · intro h
    rcases List.mem_filterMap.mp h with ⟨e, he, hf⟩
    by_cases hcond : e.dst = v ∧ e.lbl ∈ depLabels
    · rw [if_pos hcond] at hf
      injection hf with hsrc
      exact ⟨e, he, hsrc, hcond.2, hcond.1⟩
    · rw [if_neg hcond] at hf
      exact absurd hf (by simp)
  · rintro ⟨e, he, hs, hl, hd⟩
    refine List.mem_filterMap.mpr ⟨e, he, ?_⟩
    rw [if_pos ⟨hd, hl⟩]
    simp [hs]

theorem impactedBy_mem_ids {G : CodeGraph}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids) :
    ∀ u x : Nat, x ∈ G.impactedBy u → x ∈ G.ids := by
  intro u x hx
  rcases mem_impactedBy_iff.mp hx with ⟨e, he, hsrc, _, _⟩
  have h1 := (hwf e he).1
  rwa [hsrc] at h1

theorem impact_eq_iterF (G : CodeGraph) (T : List Nat) :
    G.impact T = iterF G.impactedBy G.nodes.length T := by
  unfold impact
  exact foldl_range_iterF (fun _ _ => rfl) _ _

/-- Chemin à combustible explicite : l'échelle calculable qui pilote le
    combustible de l'itéré (Path est une Prop, on ne peut pas y récursir
    pour produire un Nat). -/
inductive FPath (f : Nat → List Nat) : Nat → Nat → Nat → Prop
  | refl (s : Nat) : FPath f 0 s s
  | step {k s x v : Nat} (h : x ∈ f s) : FPath f k x v → FPath f (k + 1) s v

theorem path_imp_FPath {G : CodeGraph} {ℓ : Label} {u v : Nat} (p : Path G ℓ u v) :
    ∃ k, FPath (G.succ ℓ) k u v := by
  induction p with
  | refl u => exact ⟨0, .refl u⟩
  | @step u x v h p ih =>
    rcases ih with ⟨k, hk⟩
    exact ⟨k + 1, .step (edge_succ_mem h) hk⟩

theorem FPath_mem_iterF {f : Nat → List Nat} :
    ∀ {k s v : Nat}, FPath f k s v → v ∈ iterF f (k + 1) [s] := by
  intro k s v hp
  induction hp with
  | refl s =>
    show s ∈ iterF f 1 [s]
    exact mem_stepF_of_mem (List.mem_singleton.mpr rfl)
  | @step k s x v h p ih =>
    have hx1 : x ∈ iterF f 1 [s] :=
      mem_stepF_of_succ (List.mem_singleton.mpr rfl) h
    have hshift := iterF_shift hx1 (k + 1) v ih
    have hfe : ((k : Nat) + 1) + 1 = 1 + (k + 1) := by omega
    show v ∈ iterF f ((k + 1) + 1) [s]
    rwa [← hfe] at hshift

/-- Chemin vers itéré : un chemin aboutit, à combustible explicite `k + 1`,
    dans l'itéré d'origine. C'est la direction « complète » de reach. -/
theorem path_mem_iterF {G : CodeGraph} {ℓ : Label} {u v : Nat} (p : Path G ℓ u v) :
    ∃ k : Nat, v ∈ iterF (G.succ ℓ) (k + 1) [u] := by
  rcases path_imp_FPath p with ⟨k, hk⟩
  exact ⟨k, FPath_mem_iterF hk⟩

end ConcreteGraph

/-! ## 6. Obligations de preuve

Ce sont les théorèmes « une fois pour toutes » qui rendent le checker
digne de confiance indépendamment de GraphOS. Ils ne dépendent pas des
instances. -/

section Obligations
open CodeGraph

/-- (O1) reach est correct : tout `true` correspond à un vrai chemin.
    Aucune hypothèse de formation du graphe n'est nécessaire. -/
theorem reach_sound {G : CodeGraph} {ℓ : Label} {u v : Nat} :
    G.reach ℓ u v = true → Path G ℓ u v := by
  intro h
  simp only [CodeGraph.reach] at h
  rw [List.contains_iff_mem] at h
  rw [reachSet_eq_iterF] at h
  obtain ⟨s, hs, hp⟩ := iterF_mem_path _ _ _ h
  rcases List.mem_singleton.mp hs with rfl
  exact hp

/-- (O2) reach est complet quand les extrémités des arêtes sont des nœuds
    déclarés (hypothèse de bonne formation du graphe) : la borne de fuel |V|
    suffit. C'est ce théorème qui *fonde* le contrat de l'exporter (Decision
    4 : l'exporter n'émet que des arcs dont les bouts sont déclarés, le
    loader le vérifie). -/
theorem reach_complete {G : CodeGraph} {ℓ : Label} {u v : Nat}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids)
    (hu : u ∈ G.ids) :
    Path G ℓ u v → G.reach ℓ u v = true := by
  intro hp
  obtain ⟨k, hk⟩ := path_mem_iterF hp
  obtain ⟨j, hj, hc⟩ := closure_point_exists (succ_mem_ids hwf) [u]
    (by intro x hx; rcases List.mem_singleton.mp hx with rfl; exact hu)
  have hlen : G.ids.length = G.nodes.length := ids_length G
  have hfin : v ∈ iterF (G.succ ℓ) G.nodes.length [u] := by
    rcases Nat.le_total (k + 1) G.ids.length with hle | hge
    · have h2 : v ∈ iterF (G.succ ℓ) ((k + 1) + (G.ids.length - (k + 1))) [u] :=
        iterF_mono [u] (G.ids.length - (k + 1)) (k + 1) v hk
      rw [Nat.add_sub_cancel' hle] at h2
      rwa [hlen] at h2
    · have h2 : v ∈ iterF (G.succ ℓ) j [u] := iterF_down [u] hc _ (by omega) v hk
      have h3 : v ∈ iterF (G.succ ℓ) (j + (G.ids.length - j)) [u] :=
        iterF_mono [u] (G.ids.length - j) j v h2
      rw [Nat.add_sub_cancel' hj] at h3
      rwa [hlen] at h3
  simp only [CodeGraph.reach]
  rw [reachSet_eq_iterF]
  exact List.contains_iff_mem.mpr hfin

/-- (O3) impact est clos par D⁻¹ (même argument de saturation que O2) :
    ajouter un prédécesseur de plus ne change plus l'ensemble calculé. -/
theorem impact_closed {G : CodeGraph} {T : List Nat}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids)
    (hT : ∀ t ∈ T, t ∈ G.ids) :
    ∀ v ∈ G.impact T, ∀ u ∈ G.impactedBy v, u ∈ G.impact T := by
  intro v hv u hu
  rw [impact_eq_iterF] at hv ⊢
  have hu1 : u ∈ iterF G.impactedBy (G.nodes.length + 1) T :=
    mem_stepF_of_succ hv hu
  obtain ⟨j, hj, hc⟩ := closure_point_exists (impactedBy_mem_ids hwf) T hT
  have hj' : j ≤ G.nodes.length := by rwa [ids_length G] at hj
  have h2 : u ∈ iterF G.impactedBy j T := iterF_down T hc _ (by omega) u hu1
  have h3 : u ∈ iterF G.impactedBy (j + (G.nodes.length - j)) T :=
    iterF_mono T (G.nodes.length - j) j u h2
  rw [Nat.add_sub_cancel' hj'] at h3
  exact h3

/-! ### (O4) Sûreté de `stableSignature`

Le « comportement observable » d'un nœud `v` : les paires
`(signature du successeur, étiquette de dépendance)` atteintes depuis `v`
par un arc de dépendance (calls, implements, references, maps, dependsOn —
les étiquettes qui propagent l'impact, cf. Decision 3 ; les arcs `tests`
engendrent de la *staleness*, pas de la rupture, et sont volontairement
hors de la propagation).

Deux ajustements par rapport à la toute première formulation, chacun
imposé par un contre-exemple noyau-vérifiable :

* la conclusion est une égalité d'*ensembles* (appartenance point à
  point), pas une égalité de listes : `sameEdgeSet` compare déjà les arcs
  ensemblistement, l'*ordre* des arcs dans les listes avant/après n'est
  pas une donnée du modèle et n'est contrôlée par aucune hypothèse ;
* l'hypothèse de bonne formation (celle de O2, Decision 4) est nécessaire
  pour les deux graphes : sans elle, un arc pendouillant vers un nœud non
  déclaré peut faire apparaître ou disparaître une paire observable alors
  même que `v` est frontière et que la règle l'accepte. -/

def observable (G : CodeGraph) (v : Nat) : List (String × Label) :=
  G.edges.filterMap fun e =>
    if e.src = v ∧ e.lbl ∈ depLabels then
      (G.node? e.dst).map fun n => (n.sig, e.lbl)
    else none

/-- Caractérisation de l'appartenance à l'observable (déplie le filtre). -/
theorem mem_observable_iff {G : CodeGraph} {v : Nat} (p : String × Label) :
    p ∈ observable G v ↔
      ∃ e ∈ G.edges, e.src = v ∧ e.lbl ∈ depLabels ∧
        ∃ n : Node, G.node? e.dst = some n ∧ (n.sig, e.lbl) = p := by
  simp only [observable, List.mem_filterMap]
  constructor
  · rintro ⟨e, he, hf⟩
    by_cases hcond : e.src = v ∧ e.lbl ∈ depLabels
    · rw [if_pos hcond] at hf
      cases hn : G.node? e.dst with
      | none => rw [hn] at hf; simp at hf
      | some n => simp [hn] at hf; exact ⟨e, he, hcond.1, hcond.2, n, hn, hf⟩
    · rw [if_neg hcond] at hf
      exact absurd hf (by simp)
  · rintro ⟨e, he, hs, hl, n, hn, hp⟩
    refine ⟨e, he, ?_⟩
    rw [if_pos ⟨hs, hl⟩]
    simp [hn, hp]

/-- Si `w` n'est pas touché, il n'est ni supprimé, ni ajouté, ni modifié :
    décomposition de `touched` ensemblistement. -/
theorem touched_notMem_append {c : Change} {w : Nat} (h : w ∉ c.touched) :
    w ∉ c.before.ids.filter (fun x => (c.after.node? x).isNone) ∧
      w ∉ c.after.ids.filter (fun x => (c.before.node? x).isNone) ∧
      w ∉ c.before.ids.filter fun x =>
          match c.before.node? x, c.after.node? x with
          | some n₁, some n₂ =>
              decide (n₁ ≠ n₂) ||
                !Change.sameEdgeSet (Change.outEdges c.before x) (Change.outEdges c.after x)
          | _, _ => false := by
  simp only [Change.touched] at h
  refine ⟨?_, ?_, ?_⟩
  · intro hm1
    exact h (List.mem_eraseDups.mpr
      (List.mem_append.mpr (Or.inl (List.mem_append.mpr (Or.inl hm1)))))
  · intro hm2
    exact h (List.mem_eraseDups.mpr
      (List.mem_append.mpr (Or.inl (List.mem_append.mpr (Or.inr hm2)))))
  · intro hm3
    exact h (List.mem_eraseDups.mpr
      (List.mem_append.mpr (Or.inr hm3)))

/-- Lecture utile de « non supprimé » : si `w` est déclaré avant et pas
    touché, il est déclaré après. -/
theorem after_node_of_not_touched {c : Change} {w : Nat}
    (hwids : w ∈ c.before.ids) (h : w ∉ c.touched) :
    ∃ n' : Node, c.after.node? w = some n' := by
  obtain ⟨hnr, _, _⟩ := touched_notMem_append h
  cases hX : ((c.after.node? w).isNone) with
  | false =>
    have hne : c.after.node? w ≠ none := by
      intro hnone
      rw [hnone] at hX
      simp at hX
    rw [Option.ne_none_iff_exists] at hne
    obtain ⟨m, hm⟩ := hne
    exact ⟨m, hm.symm⟩
  | true => exact absurd (List.mem_filter.mpr ⟨hwids, hX⟩) hnr

/-- La signature d'un successeur de dépendance de `v` ne change pas :
    soit le successeur est touché et `hr` règle son cas, soit il ne l'est
    pas et le prédicat « modifié » est faux, donc le nœud est identique. -/
theorem dep_sig_stable {c : Change} {v : Nat}
    (hwf : ∀ e ∈ c.before.edges, e.src ∈ c.before.ids ∧ e.dst ∈ c.before.ids)
    (hr : stableSignature c v = true) :
    ∀ (e : Edge) (n n' : Node), e ∈ c.before.edges → e.src = v → e.lbl ∈ depLabels →
      c.before.node? e.dst = some n → c.after.node? e.dst = some n' → n.sig = n'.sig := by
  intro e n n' he1 hsrc hdep hb ha
  have hid1 : e.dst ∈ c.before.ids := (hwf e he1).2
  rcases Classical.em (e.dst ∈ c.touched) with hdt | hdt
  · simp only [stableSignature] at hr
    have hdeps : e.dst ∈
        c.before.edges.filterMap
          (fun x =>
            if x.src = v ∧ x.lbl ∈ depLabels ∧ c.touched.contains x.dst then
              some x.dst
            else none) := by
      refine List.mem_filterMap.mpr ⟨e, he1, ?_⟩
      rw [if_pos ⟨hsrc, hdep, List.contains_iff_mem.mpr hdt⟩]
    have hd := List.all_eq_true.mp hr e.dst hdeps
    simp only [hb, ha] at hd
    exact (decide_eq_true_eq.mp hd).1
  · obtain ⟨_, _, hnc⟩ := touched_notMem_append hdt
    have hpred : (match c.before.node? e.dst, c.after.node? e.dst with
        | some a, some b =>
            decide (a ≠ b) ||
              !Change.sameEdgeSet (Change.outEdges c.before e.dst) (Change.outEdges c.after e.dst)
        | _, _ => false) = false := by
      cases hX : (match c.before.node? e.dst, c.after.node? e.dst with
        | some a, some b =>
            decide (a ≠ b) ||
              !Change.sameEdgeSet (Change.outEdges c.before e.dst) (Change.outEdges c.after e.dst)
        | _, _ => false) with
      | false => rfl
      | true => exact absurd (List.mem_filter.mpr ⟨hid1, hX⟩) hnc
    simp only [hb, ha] at hpred
    rw [Bool.or_eq_false_iff] at hpred
    have hnn : n = n' := Classical.byContradiction (decide_eq_false_iff_not.mp hpred.1)
    rw [hnn]

/-- (O4) La règle `stableSignature` préserve l'observable d'un nœud frontière
    qu'elle accepte, sous la bonne formation des deux graphes. -/
theorem stableSignature_safe {c : Change} {v : Nat}
    (hv : v ∈ c.before.frontier c.touched)
    (hwf : ∀ e ∈ c.before.edges, e.src ∈ c.before.ids ∧ e.dst ∈ c.before.ids)
    (hwf' : ∀ e ∈ c.after.edges, e.src ∈ c.after.ids ∧ e.dst ∈ c.after.ids)
    (hr : stableSignature c v = true) :
    ∀ p, p ∈ observable c.before v ↔ p ∈ observable c.after v := by
  rcases List.mem_filter.mp hv with ⟨hvimp, hvc⟩
  have hvT : v ∉ c.touched := by
    intro hm
    rw [List.contains_iff_mem.mpr hm, Bool.not_true] at hvc
    exact Bool.false_ne_true hvc
  have hvimp' : v ∈ iterF c.before.impactedBy c.before.nodes.length c.touched := by
    rwa [impact_eq_iterF] at hvimp
  have hvids : v ∈ c.before.ids := by
    rcases iterF_stays2 (impactedBy_mem_ids hwf) c.touched c.before.nodes.length v hvimp' with
      h | h
    · exact absurd h hvT
    · exact h
  obtain ⟨n₁, hn₁⟩ : ∃ n₁ : Node, c.before.node? v = some n₁ := ids_node? hvids
  obtain ⟨n₂, hn₂⟩ : ∃ n₂ : Node, c.after.node? v = some n₂ :=
    after_node_of_not_touched hvids hvT
  -- les arcs sortants de v ne changent pas (v ∉ touched ⇒ prédicat « modifié » faux)
  obtain ⟨hvnr, _, hnc⟩ := touched_notMem_append hvT
  have hpredF : (match c.before.node? v, c.after.node? v with
      | some a, some b =>
          decide (a ≠ b) ||
            !Change.sameEdgeSet (Change.outEdges c.before v) (Change.outEdges c.after v)
      | _, _ => false) = false := by
    cases hX : (match c.before.node? v, c.after.node? v with
      | some a, some b =>
          decide (a ≠ b) ||
            !Change.sameEdgeSet (Change.outEdges c.before v) (Change.outEdges c.after v)
      | _, _ => false) with
    | false => rfl
    | true => exact absurd (List.mem_filter.mpr ⟨hvids, hX⟩) hnc
  simp only [hn₁, hn₂] at hpredF
  obtain ⟨_, hpnsame⟩ := Bool.or_eq_false_iff.mp hpredF
  have hsameT : Change.sameEdgeSet (Change.outEdges c.before v) (Change.outEdges c.after v) = true := by
    cases hs : (Change.sameEdgeSet (Change.outEdges c.before v) (Change.outEdges c.after v)) with
    | true => rfl
    | false => rw [hs] at hpnsame; simp at hpnsame
  obtain ⟨hall1, hall2⟩ := Bool.and_eq_true_iff.mp hsameT
  have htf : ∀ e ∈ Change.outEdges c.before v, e ∈ Change.outEdges c.after v := fun e he =>
    List.contains_iff_mem.mp (List.all_eq_true.mp hall1 e he)
  have htf' : ∀ e ∈ Change.outEdges c.after v, e ∈ Change.outEdges c.before v := fun e he =>
    List.contains_iff_mem.mp (List.all_eq_true.mp hall2 e he)
  -- membres du filtre outEdges = arcs dont la source est v
  have hmemOut : ∀ (H : CodeGraph) (e : Edge),
      e ∈ Change.outEdges H v ↔ e ∈ H.edges ∧ e.src = v := by
    intro H e
    constructor
    · intro hm
      obtain ⟨hmE, hp⟩ := List.mem_filter.mp hm
      exact ⟨hmE, decide_eq_true_eq.mp hp⟩
    · rintro ⟨hmE, hsrc⟩
      exact List.mem_filter.mpr ⟨hmE, by simp [hsrc]⟩
  -- sens direct : un arc de dépendance de v dans `before` passe dans `after`,
  -- son extrémité reste déclarée et de même signature
  have hfw : ∀ q, q ∈ observable c.before v → q ∈ observable c.after v := by
    intro q hq
    rw [mem_observable_iff] at hq ⊢
    obtain ⟨f, hf1, hsf, hlf, n, hnf, hpp⟩ := hq
    have hf2 : f ∈ c.after.edges := by
      have hout : f ∈ Change.outEdges c.before v := (hmemOut c.before f).mpr ⟨hf1, hsf⟩
      have hout2 : f ∈ Change.outEdges c.after v := htf f hout
      exact (hmemOut c.after f).mp hout2 |>.1
    obtain ⟨n', hn'⟩ : ∃ n' : Node, c.after.node? f.dst = some n' :=
      ids_node? ((hwf' f hf2).2)
    have hsig : n.sig = n'.sig :=
      dep_sig_stable (c := c) (v := v) hwf hr f n n' hf1 hsf hlf hnf hn'
    exact ⟨f, hf2, hsf, hlf, n', hn', by rw [← hpp, hsig]⟩
  -- sens retour : symétrique
  have hbw : ∀ q, q ∈ observable c.after v → q ∈ observable c.before v := by
    intro q hq
    rw [mem_observable_iff] at hq ⊢
    obtain ⟨f, hf2, hsf, hlf, n', hnf', hpp⟩ := hq
    have hf1 : f ∈ c.before.edges := by
      have hout : f ∈ Change.outEdges c.after v := (hmemOut c.after f).mpr ⟨hf2, hsf⟩
      have hout2 : f ∈ Change.outEdges c.before v := htf' f hout
      exact (hmemOut c.before f).mp hout2 |>.1
    obtain ⟨n, hn⟩ : ∃ n : Node, c.before.node? f.dst = some n :=
      ids_node? ((hwf f hf1).2)
    have hsig : n.sig = n'.sig :=
      dep_sig_stable (c := c) (v := v) hwf hr f n n' hf1 hsf hlf hn hnf'
    exact ⟨f, hf1, hsf, hlf, n, hn, by rw [← hpp, hsig]⟩
  intro p
  constructor
  · exact hfw p
  · exact hbw p
end Obligations

end IntentGraph
