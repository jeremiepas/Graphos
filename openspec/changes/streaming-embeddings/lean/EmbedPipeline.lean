/-!
# EmbedPipeline — validation Lean 4 des optimisations d'embedding

Le pipeline actuel de graphos embedde 109k nœuds un par un (un process curl
par nœud). La refonte le remplace par : déduplication des textes → batching
(input tableau OpenAI-compat) → cache de contenu → distribution des vecteurs
aux nœuds, avec complétion concurrente des batches.

Ce fichier prouve que **toutes ces optimisations préservent la sémantique** :
sous l'unique hypothèse de modélisation que le serveur d'embedding est une
fonction f : texte → résultat (déterministe à (modèle, texte) fixés), le
pipeline optimisé calcule exactement l'affectation nœud → vecteur de la
boucle séquentielle de référence (`baseline`). Tous les théorèmes sont
génériques en f et en β : instancier β := Option Vec modélise l'échec
par-texte, et la préservation vaut aussi pour *quels* nœuds obtiennent un
vecteur.

Théorèmes clés :
  * chunks_flatten            — le découpage en batches ne perd ni ne duplique rien ;
  * distribute_table_eq_baseline — théorème maître : toute table couvrant les
    textes des nœuds redonne la baseline ;
  * pipeline_eq_baseline      — dédup + batching ≡ baseline ;
  * pipeline_batch_size_irrelevant, pipeline_keys_irrelevant — la taille de
    batch, l'ordre de complétion, les retries dupliqués ne changent rien ;
  * runCached_fst / runCached_sound — un cache sain redonne la baseline et
    reste sain : le cache d'un run est valide pour le suivant.

Lean 4 core uniquement, pas de Mathlib.
-/

-- Les théorèmes génériques n'utilisent pas toujours DecidableEq α ;
-- l'inclusion automatique de la variable de section est inutile et bruyante.
set_option linter.unusedSectionVars false

namespace EmbedPipeline

variable {α β : Type} [DecidableEq α]

/-! ## 1. Batching : découpage en chunks de taille k+1 -/

/-- Découpage en chunks de taille k+1 (k+1 pour exclure la taille 0). -/
def chunks (k : Nat) : List α → List (List α)
  | [] => []
  | x :: xs => (x :: xs.take k) :: chunks k (xs.drop k)
termination_by l => l.length
decreasing_by simp [List.length_drop]; omega

/-- Recoller les batches redonne la liste : le batching ne perd ni ne
    duplique aucun texte. -/
theorem chunks_flatten (k : Nat) (l : List α) : (chunks k l).flatten = l := by
  induction l using chunks.induct k with
  | case1 => simp [chunks]
  | case2 x xs ih =>
    simp [chunks, ih, List.take_append_drop]

/-! ## 2. Table d'embeddings et distribution -/

/-- La table de référence : chaque clé associée à f(clé). -/
def table (f : α → β) (keys : List α) : List (α × β) :=
  keys.map (fun k => (k, f k))

/-- La table construite batch par batch (ce que fait le pipeline). -/
def batchedTable (k : Nat) (f : α → β) (keys : List α) : List (α × β) :=
  ((chunks k keys).map (table f)).flatten

/-- Construire la table par batches ou d'un coup : identique. -/
theorem batchedTable_eq_table (k : Nat) (f : α → β) (keys : List α) :
    batchedTable k f keys = table f keys := by
  have h : ∀ L : List (List α), (L.map (table f)).flatten = table f L.flatten := by
    intro L
    induction L with
    | nil => rfl
    | cons b bs ih => simp [table, ih]
  simpa [batchedTable, chunks_flatten] using h (chunks k keys)

/-- Chercher une clé présente dans la table donne f(clé) — quel que soit
    l'ordre de la table et même si la clé y figure plusieurs fois (retries). -/
theorem lookup_table (f : α → β) {keys : List α} {t : α} (ht : t ∈ keys) :
    (table f keys).lookup t = some (f t) := by
  induction keys with
  | nil => cases ht
  | cons x xs ih =>
    rcases List.mem_cons.mp ht with h0 | ht'
    · subst h0
      rw [table, List.map_cons, List.lookup]
      split
      · rfl
      · next hbeq => exact absurd rfl (not_eq_of_beq_eq_false hbeq)
    · rw [table, List.map_cons, List.lookup]
      split
      · next hbeq =>
        have hxe : x = t := (eq_of_beq hbeq).symm
        rw [hxe]
      · next hbeq => exact ih ht'

/-! ## 3. Déduplication -/

/-- Dédup (garde la dernière occurrence ; seule l'appartenance compte). -/
def dedup : List α → List α
  | [] => []
  | x :: xs => if x ∈ xs then dedup xs else x :: dedup xs

theorem mem_dedup {l : List α} {a : α} : a ∈ dedup l ↔ a ∈ l := by
  induction l with
  | nil => simp [dedup]
  | cons x xs ih =>
    by_cases hx : x ∈ xs
    · simp only [dedup, if_pos hx, ih, List.mem_cons]
      exact ⟨Or.inr, fun h => h.elim (fun he => he ▸ hx) id⟩
    · simp [dedup, if_neg hx, ih]

/-! ## 4. Le pipeline et son équivalence à la baseline -/

/-- La sémantique de référence : la boucle séquentielle actuelle,
    un appel par nœud. -/
def baseline (f : α → β) (nodes : List (Nat × α)) : List (Nat × β) :=
  nodes.map (fun n => (n.1, f n.2))

/-- Distribution : chaque nœud reçoit le vecteur de son texte dans la table. -/
def distribute (tbl : List (α × β)) (fb : β) (nodes : List (Nat × α)) :
    List (Nat × β) :=
  nodes.map (fun n => (n.1, (tbl.lookup n.2).getD fb))

/-- Le pipeline optimisé : dédup des textes, table par batches, distribution. -/
def pipeline (k : Nat) (f : α → β) (fb : β) (nodes : List (Nat × α)) :
    List (Nat × β) :=
  distribute (batchedTable k f (dedup (nodes.map Prod.snd))) fb nodes

/-- **Théorème maître** : toute table de la forme `table f keys` dont les clés
    couvrent les textes des nœuds distribue exactement la baseline. L'ordre
    des clés, les doublons (travail dupliqué par des batches concurrents ou
    des retries) et le fallback sont sans effet. -/
theorem distribute_table_eq_baseline (f : α → β) (fb : β)
    (nodes : List (Nat × α)) {keys : List α}
    (hcov : ∀ n ∈ nodes, n.2 ∈ keys) :
    distribute (table f keys) fb nodes = baseline f nodes := by
  unfold distribute baseline
  apply List.map_congr_left
  intro n hn
  rw [lookup_table f (hcov n hn)]
  rfl

/-- L'optimisation est correcte : le pipeline batché-dédupliqué calcule
    l'affectation de la boucle séquentielle, pour tout f, toute taille de
    batch, tout fallback. -/
theorem pipeline_eq_baseline (k : Nat) (f : α → β) (fb : β)
    (nodes : List (Nat × α)) :
    pipeline k f fb nodes = baseline f nodes := by
  unfold pipeline
  rw [batchedTable_eq_table]
  exact distribute_table_eq_baseline f fb nodes
    (fun n hn => mem_dedup.mpr (List.mem_map_of_mem hn))

/-- La taille de batch est sans effet sur le résultat. -/
theorem pipeline_batch_size_irrelevant (k₁ k₂ : Nat) (f : α → β) (fb : β)
    (nodes : List (Nat × α)) :
    pipeline k₁ f fb nodes = pipeline k₂ f fb nodes := by
  rw [pipeline_eq_baseline, pipeline_eq_baseline]

/-- Deux stratégies de collecte des clés (dédup global, dédup streaming par
    seen-set, ordre de complétion concurrent, batches retentés donc dupliqués)
    donnent le même résultat dès qu'elles couvrent les textes des nœuds. -/
theorem pipeline_keys_irrelevant (k₁ k₂ : Nat) (f : α → β) (fb : β)
    (nodes : List (Nat × α)) {keys₁ keys₂ : List α}
    (h₁ : ∀ n ∈ nodes, n.2 ∈ keys₁) (h₂ : ∀ n ∈ nodes, n.2 ∈ keys₂) :
    distribute (batchedTable k₁ f keys₁) fb nodes
      = distribute (batchedTable k₂ f keys₂) fb nodes := by
  rw [batchedTable_eq_table, batchedTable_eq_table,
      distribute_table_eq_baseline f fb nodes h₁,
      distribute_table_eq_baseline f fb nodes h₂]

/-! ## 5. Cache de contenu -/

/-- Un cache est sain pour f si chaque entrée stocke bien f(clé).
    (Modélise le cache disque SHA256(modèle, texte) → vecteur ; l'injectivité
    du hachage est l'hypothèse d'implémentation qui identifie clé et texte.) -/
def Sound (f : α → β) (cache : List (α × β)) : Prop :=
  ∀ p ∈ cache, p.2 = f p.1

theorem sound_table (f : α → β) (keys : List α) : Sound f (table f keys) := by
  intro p hp
  simp only [table, List.mem_map] at hp
  obtain ⟨k, _, rfl⟩ := hp
  rfl

theorem sound_append {f : α → β} {c₁ c₂ : List (α × β)}
    (h₁ : Sound f c₁) (h₂ : Sound f c₂) : Sound f (c₁ ++ c₂) := by
  intro p hp
  rcases List.mem_append.mp hp with h | h
  · exact h₁ p h
  · exact h₂ p h

theorem mem_of_lookup_eq_some {l : List (α × β)} {a : α} {b : β}
    (h : l.lookup a = some b) : (a, b) ∈ l := by
  induction l with
  | nil => cases h
  | cons p ps ih =>
    obtain ⟨k, v⟩ := p
    rcases beq : (a == k) with _ | _
    · have h' : ps.lookup a = some b := by
        rw [List.lookup, beq] at h; exact h
      exact List.mem_cons_of_mem _ (ih h')
    · have h' : some v = some b := by
        rw [List.lookup, beq] at h; exact h
      have hv : v = b := by simpa using h'
      have hka : k = a := (eq_of_beq beq).symm
      simp only [List.mem_cons]
      exact Or.inl (by simp [hka, hv])

theorem lookup_append_of_none {l₁ l₂ : List (α × β)} {a : α}
    (h : l₁.lookup a = none) : (l₁ ++ l₂).lookup a = l₂.lookup a := by
  induction l₁ with
  | nil => rfl
  | cons p ps ih =>
    obtain ⟨k, v⟩ := p
    rcases beq : (a == k) with _ | _
    · have h' : ps.lookup a = none := by
        rw [List.lookup, beq] at h; exact h
      have ih' : (ps ++ l₂).lookup a = l₂.lookup a := ih h'
      simp only [List.cons_append, List.lookup, beq]
      exact ih'
    · have h' : some v = none := by
        rw [List.lookup, beq] at h; exact h
      simp at h'

theorem lookup_append_of_some {l₁ l₂ : List (α × β)} {a : α} {b : β}
    (h : l₁.lookup a = some b) : (l₁ ++ l₂).lookup a = some b := by
  induction l₁ with
  | nil => cases h
  | cons p ps ih =>
    obtain ⟨k, v⟩ := p
    rcases beq : (a == k) with _ | _
    · have h' : ps.lookup a = some b := by
        rw [List.lookup, beq] at h; exact h
      have ih' : (ps ++ l₂).lookup a = some b := ih h'
      simp only [List.cons_append, List.lookup, beq]
      exact ih'
    · have h' : some v = some b := by
        rw [List.lookup, beq] at h; exact h
      simp only [List.cons_append, List.lookup, beq]
      exact h'

/-- Le run avec cache : seules les clés absentes du cache sont calculées
    (par batches) ; le cache s'enrichit ; la distribution lit le cache
    enrichi. Retourne (affectation, nouveau cache). -/
def runCached (k : Nat) (f : α → β) (cache : List (α × β)) (fb : β)
    (nodes : List (Nat × α)) : List (Nat × β) × List (α × β) :=
  let keys := dedup (nodes.map Prod.snd)
  let misses := keys.filter (fun t => (cache.lookup t).isNone)
  let cache' := cache ++ batchedTable k f misses
  (distribute cache' fb nodes, cache')

/-- Un cache sain s'enrichit en cache sain : le cache produit par un run est
    un cache valide pour le run suivant (réutilisation inter-runs, --fresh
    compris). -/
theorem runCached_sound (k : Nat) (f : α → β) {cache : List (α × β)} (fb : β)
    (nodes : List (Nat × α)) (hs : Sound f cache) :
    Sound f (runCached k f cache fb nodes).2 := by
  unfold runCached
  exact sound_append hs (by rw [batchedTable_eq_table]; exact sound_table f _)

/-- Avec un cache sain, le run caché calcule exactement la baseline : le
    cache est une pure optimisation, jamais une source de résultats. -/
theorem runCached_fst (k : Nat) (f : α → β) {cache : List (α × β)} (fb : β)
    (nodes : List (Nat × α)) (hs : Sound f cache) :
    (runCached k f cache fb nodes).1 = baseline f nodes := by
  unfold runCached distribute baseline
  apply List.map_congr_left
  intro n hn
  have hmem : n.2 ∈ dedup (nodes.map Prod.snd) :=
    mem_dedup.mpr (List.mem_map_of_mem hn)
  cases hc : cache.lookup n.2 with
  | some v =>
    have hv : v = f n.snd := hs _ (mem_of_lookup_eq_some hc)
    rw [lookup_append_of_some hc, Option.getD_some, hv]
  | none =>
    have hmiss : n.2 ∈ (dedup (nodes.map Prod.snd)).filter
        (fun t => (cache.lookup t).isNone) :=
      List.mem_filter.mpr ⟨hmem, by simp [hc]⟩
    rw [lookup_append_of_none hc, batchedTable_eq_table,
        lookup_table f hmiss]
    rfl

/-! ## 6. Échec par-texte

Tous les théorèmes sont génériques en β : instancier β := Option V et
f := la réponse du serveur (some vecteur ou none) montre que le pipeline
optimisé préserve aussi *exactement quels* nœuds obtiennent un vecteur —
un batch en échec dégrade ses propres textes et rien d'autre, comme la
boucle séquentielle. L'hypothèse est le déterminisme de f ; un échec
transitoire (retry réussi) est un f différent, couvert parce que le retry
passe par les mêmes théorèmes (clés dupliquées admises par lookup_table). -/

/-! ## 7. Instance jouet, vérifiée par le noyau -/

section Toy

def nodes : List (Nat × String) :=
  [(1, "getUser"), (2, "Promise"), (3, "Promise"), (4, "setUser"), (5, "Promise")]

def f (t : String) : Nat := t.length

-- Le pipeline (batches de 2, 5 nœuds mais 3 textes uniques) = la baseline.
example : pipeline 1 f 0 nodes = baseline f nodes := by
  rw [pipeline_eq_baseline]

-- Tailles de batch différentes : même résultat (toutes valent la baseline).
example : pipeline 0 f 0 nodes = pipeline 9 f 0 nodes := by
  rw [pipeline_eq_baseline, pipeline_eq_baseline]

-- Run caché depuis un cache vide : résultat = baseline.
example : (runCached 1 f [] 0 nodes).1 = baseline f nodes := by
  rw [runCached_fst]
  exact sound_table f []

-- Le cache produit resservi tel quel redonne encore la baseline
-- (réutilisation inter-runs) : le run interne fabrique un cache sain,
-- donc le run externe ne recalcule rien de neuf.
example :
    (runCached 1 f (runCached 1 f [] 0 nodes).2 0 nodes).1
      = baseline f nodes := by
  rw [runCached_fst]
  rw [runCached]
  simp only [List.nil_append, batchedTable_eq_table]
  exact sound_table f _

-- Échec par-texte : β := Option Nat, "Promise" échoue ; le pipeline préserve
-- exactement quels nœuds ont un vecteur.
def g (t : String) : Option Nat := if t = "Promise" then none else some t.length

example : pipeline 1 g none nodes = baseline g nodes := pipeline_eq_baseline 1 g none nodes

end Toy

end EmbedPipeline
