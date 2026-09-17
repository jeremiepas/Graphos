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

Lean 4 core uniquement, pas de Mathlib. Compile avec Lean 4.30.0
(lake build vert ; seul ajustement depuis le premier jet : le binder
prefix renommé pre, mot-clé Lean). Les exemples jouets, y compris la
détection de l'oubli schéma/contrat, sont acceptés par le noyau (by decide).
Les sorry marquent les obligations de preuve restantes O1–O4, listées en
fin de fichier.
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

/-! ## 6. Obligations de preuve restantes

Ce sont les théorèmes « une fois pour toutes » qui rendent le checker
digne de confiance indépendamment de GraphOS. Ils ne dépendent pas des
instances. -/

section Obligations
open CodeGraph

/-- (O1) reach est correct : tout true correspond à un vrai chemin. -/
theorem reach_sound {G : CodeGraph} {ℓ : Label} {u v : Nat} :
    G.reach ℓ u v = true → Path G ℓ u v := by
  sorry

/-- (O2) reach est complet quand les extrémités des arêtes sont des nœuds
    déclarés (hypothèse de bonne formation du graphe) : la borne de fuel |V|
    suffit. -/
theorem reach_complete {G : CodeGraph} {ℓ : Label} {u v : Nat}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids)
    (hu : u ∈ G.ids) :
    Path G ℓ u v → G.reach ℓ u v = true := by
  sorry

/-- (O3) impact est clos par D⁻¹ (même argument de fuel que O2). -/
theorem impact_closed {G : CodeGraph} {T : List Nat}
    (hwf : ∀ e ∈ G.edges, e.src ∈ G.ids ∧ e.dst ∈ G.ids)
    (hT : ∀ t ∈ T, t ∈ G.ids) :
    ∀ v ∈ G.impact T, ∀ u ∈ G.impactedBy v, u ∈ G.impact T := by
  sorry

/-- (O4) Sûreté d'une règle. Il faut d'abord définir ce qu'est le
    « comportement observable » d'un nœud sur le graphe ; une définition
    raisonnable pour commencer : l'ensemble des (signature, étiquette) des
    nœuds atteignables depuis v. La règle stableSignature est sûre si,
    lorsqu'elle accepte v, cet ensemble est le même avant et après. -/
def observable (G : CodeGraph) (v : Nat) : List (String × Label) :=
  G.edges.filterMap fun e =>
    if e.src = v then (G.node? e.dst).map (fun n => (n.sig, e.lbl)) else none

theorem stableSignature_safe {c : Change} {v : Nat}
    (hv : v ∈ c.before.frontier c.touched)
    (hr : stableSignature c v = true) :
    observable c.before v = observable c.after v := by
  sorry

end Obligations

end IntentGraph
