/-!
# ReviewGraph — modèle formel Lean 4 de `usable-graph-for-code-review`

Formalisation minimale et certifiée des garanties que la revue de code
attend du graphe (bug report 2026-10-08, solario-core PR #1357) :

  * **identité des nœuds** (D1) : la clé d'un nœud est
    `(dirHash, stem, kind, identifiant, rang de collision)` — jamais le texte
    de la déclaration ; deux déclarations d'un même fichier reçoivent des clés
    distinctes ; une déclaration dont le nom est unique a le rang 0 quelle que
    soit sa position ; ajouter une déclaration sans rapport ne change aucun rang ;
  * **arêtes d'import** (D2) : la cible calculée pour un spécificateur résolu
    est *la même clé* que le nœud module du fichier cible, donc une arête
    d'import résolue n'est jamais abandonnée au build, et toute chaîne d'imports
    entre fichiers se relève en chemin dans le graphe construit (AVI-518, INV-9) ;
  * **résolution exacte** (D3) : `symbols` / `explain` / `path` ne substituent
    jamais un nœud — un résultat `single` est un nœud du graphe qui correspond
    par id, par identifiant ou par identifiant insensible à la casse ; `notFound`
    signifie qu'aucun nœud ne correspond ; un id exact gagne toujours ;
  * **cypher** (D4) : filtrer *avant* de plafonner contient l'ancien résultat et
    retourne tous les matches quand ils tiennent dans le budget ; l'ancien ordre
    peut rendre vide une requête qui a des matches (contre-exemple décidé) ;
    une égalité sur `id` a au plus un candidat quand les clés sont distinctes ;
  * **observabilité** (D5) : `--no-observability` éteint la trace de debug
    quel que soit le niveau de log ; un répertoire relatif reste sous le
    répertoire de sortie ;
  * **rapport** (D6) : la table des ponts est bornée par la constante et ne
    contient que des degrés ≥ 2 ;
  * **granularité** (D7) : la résolution CLI → extension → global est une
    fonction pure, et le câblage transmet le niveau résolu (l'ancien câblage
    constant est réfuté sur `file`).

Méthodologie `lean-proof-methodology` : invocation goal-guided des théorèmes
(`rw`, `simp only`, `have` de defeq-forcing), pas d'application positionnelle
d'arguments à un théorème portant un binder auto-implicite, pas de `by decide`
sur des énoncés non réductibles par le noyau. Les primitives de liste sont des
helpers structuraux maison (`filt`, `tk`, `concatMap`, `Distinct`) qui
réduisent par `rfl`/`decide` ; les fonctions de rendu d'id et de normalisation
de casse sont des paramètres de section, les théorèmes valent pour toute
instance.

Lean 4 core uniquement, pas de Mathlib.
-/

set_option linter.unusedSectionVars false
set_option autoImplicit false

namespace ReviewGraph

/-! ## 0. Primitives de liste réductibles -/

section ListPrims
variable {α β : Type}

/-- Filtre structurel (match sur le booléen, pas de `if` coercé). -/
def filt (p : α → Bool) : List α → List α
  | [] => []
  | x :: xs =>
    match p x with
    | true => x :: filt p xs
    | false => filt p xs

/-- Préfixe de longueur au plus `n`. -/
def tk : Nat → List α → List α
  | 0, _ => []
  | _ + 1, [] => []
  | n + 1, x :: xs => x :: tk n xs

/-- Concaténation des images. -/
def concatMap (f : α → List β) : List α → List β
  | [] => []
  | x :: xs => f x ++ concatMap f xs

/-- Éléments deux à deux distincts. -/
def Distinct : List α → Prop
  | [] => True
  | x :: xs => x ∉ xs ∧ Distinct xs

theorem mem_filt {p : α → Bool} {x : α} :
    ∀ {l : List α}, x ∈ filt p l ↔ (x ∈ l ∧ p x = true)
  | [] => by simp [filt]
  | y :: ys => by
      have ih := @mem_filt p x ys
      cases hy : p y with
      | true =>
        simp only [filt, hy, List.mem_cons]
        constructor
        · intro h
          cases h with
          | inl hxy => exact ⟨Or.inl hxy, hxy ▸ hy⟩
          | inr hx => exact ⟨Or.inr (ih.mp hx).1, (ih.mp hx).2⟩
        · intro h
          obtain ⟨h1, h2⟩ := h
          cases h1 with
          | inl hxy => exact Or.inl hxy
          | inr hx => exact Or.inr (ih.mpr ⟨hx, h2⟩)
      | false =>
        simp only [filt, hy, List.mem_cons]
        constructor
        · intro h
          exact ⟨Or.inr (ih.mp h).1, (ih.mp h).2⟩
        · intro h
          obtain ⟨h1, h2⟩ := h
          cases h1 with
          | inl hxy => subst hxy; rw [hy] at h2; cases h2
          | inr hx => exact ih.mpr ⟨hx, h2⟩

theorem mem_concatMap {f : α → List β} {y : β} :
    ∀ {l : List α}, y ∈ concatMap f l ↔ ∃ x, x ∈ l ∧ y ∈ f x
  | [] => by simp [concatMap]
  | x :: xs => by
      simp only [concatMap, List.mem_append, List.mem_cons]
      constructor
      · intro h
        cases h with
        | inl hy => exact ⟨x, Or.inl rfl, hy⟩
        | inr hy =>
          obtain ⟨z, hz, hyz⟩ := (mem_concatMap (l := xs)).mp hy
          exact ⟨z, Or.inr hz, hyz⟩
      · intro h
        obtain ⟨z, hz, hyz⟩ := h
        cases hz with
        | inl hzx => subst hzx; exact Or.inl hyz
        | inr hzx => exact Or.inr ((mem_concatMap (l := xs)).mpr ⟨z, hzx, hyz⟩)

theorem mem_tk {x : α} : ∀ {n : Nat} {l : List α}, x ∈ tk n l → x ∈ l
  | 0, _, h => by simp [tk] at h
  | _ + 1, [], h => by simp [tk] at h
  | n + 1, y :: ys, h => by
      simp only [tk, List.mem_cons] at h ⊢
      cases h with
      | inl hxy => exact Or.inl hxy
      | inr hx => exact Or.inr (mem_tk hx)

theorem mem_tk_succ {x : α} : ∀ {n : Nat} {l : List α}, x ∈ tk n l → x ∈ tk (n + 1) l
  | 0, _, h => by simp [tk] at h
  | _ + 1, [], h => by simp [tk] at h
  | n + 1, y :: ys, h => by
      simp only [tk, List.mem_cons] at h ⊢
      cases h with
      | inl hxy => exact Or.inl hxy
      | inr hx => exact Or.inr (mem_tk_succ hx)

theorem tk_length_le {l : List α} : ∀ {n : Nat}, (tk n l).length ≤ n := by
  induction l with
  | nil => intro n; cases n <;> simp [tk]
  | cons y ys ih =>
    intro n
    cases n with
    | zero => simp [tk]
    | succ m => simp only [tk, List.length_cons]; exact Nat.succ_le_succ ih

theorem tk_of_length_le {l : List α} : ∀ {n : Nat}, l.length ≤ n → tk n l = l := by
  induction l with
  | nil => intro n _; cases n <;> rfl
  | cons y ys ih =>
    intro n h
    cases n with
    | zero => simp at h
    | succ m => simp only [tk]; rw [ih (Nat.le_of_succ_le_succ h)]

end ListPrims

/-! ## 1. Identité des nœuds (D1) -/

inductive Kind where
  | module | function | type | constant | import | external
  deriving DecidableEq, Repr

/-- Clé d'identité d'un nœud. Aucun champ ne contient le texte source. -/
structure NodeKey where
  dir   : Nat
  stem  : String
  kind  : Kind
  ident : String
  rank  : Nat
  deriving DecidableEq, Repr

/-- Une déclaration extraite : son texte n'entre jamais dans la clé. -/
structure Decl where
  kind  : Kind
  ident : String
  line  : Nat
  text  : String
  deriving DecidableEq, Repr

def sameName (k : Kind) (i : String) (d : Decl) : Bool :=
  decide (d.kind = k) && decide (d.ident = i)

/-- Nombre de déclarations de même (kind, identifiant) dans `ds`. -/
def countBefore (k : Kind) (i : String) : List Decl → Nat
  | [] => 0
  | d :: ds => (if sameName k i d then 1 else 0) + countBefore k i ds

/-- Clé d'une déclaration étant donné les déclarations déjà vues. -/
def keyOf (dir : Nat) (stem : String) (seen : List Decl) (d : Decl) : NodeKey :=
  ⟨dir, stem, d.kind, d.ident, countBefore d.kind d.ident seen⟩

/-- Attribution des clés dans l'ordre du fichier. -/
def assignKeys (dir : Nat) (stem : String) : List Decl → List Decl → List NodeKey
  | _, [] => []
  | seen, d :: ds => keyOf dir stem seen d :: assignKeys dir stem (seen ++ [d]) ds

/-- La clé ne dépend pas du texte de la déclaration. -/
theorem keyOf_text_free (dir : Nat) (stem : String) (seen : List Decl) (d : Decl) (t : String) :
    keyOf dir stem seen d = keyOf dir stem seen { d with text := t } := rfl

theorem countBefore_append (k : Kind) (i : String) :
    ∀ (xs ys : List Decl), countBefore k i (xs ++ ys) = countBefore k i xs + countBefore k i ys
  | [], ys => by simp [countBefore]
  | x :: xs, ys => by
      simp [countBefore, List.cons_append, countBefore_append k i xs ys, Nat.add_assoc]

/-- Une déclaration sans rapport ne change aucun rang. -/
theorem countBefore_unrelated (k : Kind) (i : String) (e : Decl) (seen : List Decl)
    (h : sameName k i e = false) : countBefore k i (seen ++ [e]) = countBefore k i seen := by
  rw [countBefore_append]; simp [countBefore, h]

theorem countBefore_same (k : Kind) (i : String) (e : Decl) (seen : List Decl)
    (h : sameName k i e = true) : countBefore k i (seen ++ [e]) = countBefore k i seen + 1 := by
  rw [countBefore_append]; simp [countBefore, h]

/-- Une déclaration dont le nom est unique a le rang 0, où qu'elle soit. -/
theorem rank_zero_of_unique (dir : Nat) (stem : String) (seen : List Decl) (d : Decl)
    (h : countBefore d.kind d.ident seen = 0) : (keyOf dir stem seen d).rank = 0 := h

theorem rank_ge (dir : Nat) (stem : String) :
    ∀ (seen ds : List Decl) (key : NodeKey), key ∈ assignKeys dir stem seen ds →
      countBefore key.kind key.ident seen ≤ key.rank
  | _, [], _, h => by simp [assignKeys] at h
  | seen, d :: ds, key, h => by
      simp only [assignKeys, List.mem_cons] at h
      cases h with
      | inl hk => subst hk; exact Nat.le_refl _
      | inr hk =>
        have ih := rank_ge dir stem (seen ++ [d]) ds key hk
        have hmono : countBefore key.kind key.ident seen
            ≤ countBefore key.kind key.ident (seen ++ [d]) := by
          rw [countBefore_append]; exact Nat.le_add_right _ _
        exact Nat.le_trans hmono ih

theorem head_ne_tail (dir : Nat) (stem : String) (seen : List Decl) (d : Decl) {ds : List Decl} :
    ∀ key ∈ assignKeys dir stem (seen ++ [d]) ds, keyOf dir stem seen d ≠ key := by
  intro key hk heq
  have hge := rank_ge dir stem (seen ++ [d]) ds key hk
  subst heq
  simp only [keyOf] at hge
  rw [countBefore_same d.kind d.ident d seen (by simp [sameName])] at hge
  exact Nat.not_succ_le_self _ hge

/-- Deux déclarations d'un même fichier reçoivent des clés distinctes. -/
theorem assignKeys_distinct (dir : Nat) (stem : String) :
    ∀ (seen ds : List Decl), Distinct (assignKeys dir stem seen ds)
  | _, [] => trivial
  | seen, d :: ds => by
      simp only [assignKeys, Distinct]
      exact ⟨fun h => head_ne_tail dir stem seen d _ h rfl,
             assignKeys_distinct dir stem (seen ++ [d]) ds⟩

/-! ## 2. Arêtes d'import et atteignabilité (D2) -/

/-- Hachage de répertoire : n'importe quelle fonction pure des segments. -/
def dirHash (segs : List String) : Nat := segs.foldl (fun h s => h * 31 + s.length) 7

def dirOf (p : List String) : List String := p.dropLast
def fileName (p : List String) : String := p.getLast?.getD ""

/-- Clé du nœud module d'un fichier : fonction du chemin seul. -/
def moduleKey (p : List String) : NodeKey :=
  ⟨dirHash (dirOf p), fileName p, .module, fileName p, 0⟩

/-- Cible d'un spécificateur résolu en chemin : la même fonction. -/
def importTarget (resolved : List String) : NodeKey := moduleKey resolved

theorem importTarget_eq_moduleKey (p : List String) : importTarget p = moduleKey p := rfl

/-- Ancien comportement : un nœud placeholder de kind `external`, jamais égal
    au nœud module du fichier cible. -/
def placeholderKey (resolved : List String) : NodeKey :=
  ⟨dirHash (dirOf resolved), fileName resolved, .external, fileName resolved, 0⟩

theorem placeholder_ne_moduleKey (p : List String) : placeholderKey p ≠ moduleKey p := by
  intro h
  simp [placeholderKey, moduleKey] at h

structure SourceFile where
  path    : List String
  decls   : List Decl
  imports : List (List String)
  deriving DecidableEq, Repr

def fileKeys (f : SourceFile) : List NodeKey :=
  moduleKey f.path :: assignKeys (dirHash (dirOf f.path)) (fileName f.path) [] f.decls

def nodeKeys (fs : List SourceFile) : List NodeKey := concatMap fileKeys fs

structure BEdge where
  src : NodeKey
  tgt : NodeKey
  deriving DecidableEq, Repr

def fileImportEdges (f : SourceFile) : List BEdge :=
  f.imports.map (fun t => ⟨moduleKey f.path, importTarget t⟩)

def importEdges (fs : List SourceFile) : List BEdge := concatMap fileImportEdges fs

/-- Le build ne garde qu'une arête dont les deux extrémités existent. -/
def keep (ks : List NodeKey) (e : BEdge) : Bool := decide (e.src ∈ ks) && decide (e.tgt ∈ ks)

def builtEdges (fs : List SourceFile) : List BEdge := filt (keep (nodeKeys fs)) (importEdges fs)

theorem moduleKey_mem_nodeKeys {fs : List SourceFile} {f : SourceFile} (hf : f ∈ fs) :
    moduleKey f.path ∈ nodeKeys fs :=
  mem_concatMap.mpr ⟨f, hf, List.mem_cons.mpr (Or.inl rfl)⟩

/-- Une arête d'import résolue vers un fichier extrait survit au build. -/
theorem resolved_import_kept {fs : List SourceFile} {f g : SourceFile}
    (hf : f ∈ fs) (hg : g ∈ fs) (ht : g.path ∈ f.imports) :
    (⟨moduleKey f.path, moduleKey g.path⟩ : BEdge) ∈ builtEdges fs := by
  apply mem_filt.mpr
  constructor
  · exact mem_concatMap.mpr ⟨f, hf, List.mem_map.mpr ⟨g.path, ht, rfl⟩⟩
  · simp only [keep, Bool.and_eq_true, decide_eq_true_eq]
    exact ⟨moduleKey_mem_nodeKeys hf, moduleKey_mem_nodeKeys hg⟩

/-- Atteignabilité = clôture réflexive-transitive des arêtes construites (AVI-518 §1.1). -/
inductive Reach (E : List BEdge) : NodeKey → NodeKey → Prop where
  | refl (a : NodeKey) : Reach E a a
  | step {a b c : NodeKey} : (⟨a, b⟩ : BEdge) ∈ E → Reach E b c → Reach E a c

/-- Chaîne d'imports entre fichiers extraits. -/
inductive ImportChain (fs : List SourceFile) : SourceFile → SourceFile → Prop where
  | refl (f : SourceFile) : ImportChain fs f f
  | step {f g h : SourceFile} :
      f ∈ fs → g ∈ fs → g.path ∈ f.imports → ImportChain fs g h → ImportChain fs f h

/-- Toute chaîne d'imports se relève en chemin du graphe construit. -/
theorem chain_lifts {fs : List SourceFile} {f h : SourceFile} (hc : ImportChain fs f h) :
    Reach (builtEdges fs) (moduleKey f.path) (moduleKey h.path) := by
  induction hc with
  | refl f => exact Reach.refl _
  | step hf hg ht _ ih => exact Reach.step (resolved_import_kept hf hg ht) ih

/-! ## 3. Résolution exacte des arguments (D3) -/

structure GNode where
  key   : NodeKey
  label : String
  deriving DecidableEq, Repr

def keysOf (G : List GNode) : List NodeKey := G.map (·.key)

def byKey (k : NodeKey) (G : List GNode) : List GNode := filt (fun n => decide (n.key = k)) G

theorem mem_keysOf {G : List GNode} {n : GNode} (h : n ∈ G) : n.key ∈ keysOf G :=
  List.mem_map.mpr ⟨n, h, rfl⟩

theorem byKey_nil_of_absent {k : NodeKey} : ∀ {G : List GNode}, k ∉ keysOf G → byKey k G = []
  | [], _ => rfl
  | n :: G, h => by
      have hk : n.key ≠ k := fun e => h (e ▸ mem_keysOf (List.mem_cons.mpr (Or.inl rfl)))
      have hrest : k ∉ keysOf G := fun e => h (List.mem_cons.mpr (Or.inr e))
      have hd : decide (n.key = k) = false := decide_eq_false hk
      simp only [byKey, filt, hd]
      exact byKey_nil_of_absent hrest

/-- Quand les clés sont distinctes, une égalité sur `id` a au plus un candidat. -/
theorem byKey_unique {k : NodeKey} : ∀ {G : List GNode}, Distinct (keysOf G) → (byKey k G).length ≤ 1
  | [], _ => Nat.zero_le _
  | n :: G, hd => by
      simp only [keysOf, List.map, Distinct] at hd
      obtain ⟨hnot, hrest⟩ := hd
      by_cases hk : n.key = k
      · have ht : decide (n.key = k) = true := decide_eq_true hk
        simp only [byKey, filt, ht]
        have hnil : byKey k G = [] := byKey_nil_of_absent (hk ▸ hnot)
        rw [show filt (fun m => decide (m.key = k)) G = byKey k G from rfl, hnil]
        exact Nat.le_refl _
      · have hf : decide (n.key = k) = false := decide_eq_false hk
        simp only [byKey, filt, hf]
        exact byKey_unique hrest

/-- Un nœud présent, clés distinctes : la recherche par clé le rend seul. -/
theorem byKey_of_mem {n : GNode} : ∀ {G : List GNode}, Distinct (keysOf G) → n ∈ G → byKey n.key G = [n]
  | [], _, h => by simp at h
  | m :: G, hd, h => by
      simp only [keysOf, List.map, Distinct] at hd
      obtain ⟨hnot, hrest⟩ := hd
      simp only [List.mem_cons] at h
      cases h with
      | inl hnm =>
        subst hnm
        have hnil : filt (fun m => decide (m.key = n.key)) G = [] := byKey_nil_of_absent hnot
        simp [byKey, filt, hnil]
      | inr hn =>
        have hne : m.key ≠ n.key := fun e => hnot (e ▸ mem_keysOf hn)
        have hf : decide (m.key = n.key) = false := decide_eq_false hne
        simp only [byKey, filt, hf]
        exact byKey_of_mem hrest hn

section Resolution
variable (render : NodeKey → String) (lower : String → String)

inductive Resolution where
  | single (k : NodeKey)
  | ambiguous (ks : List NodeKey)
  | notFound
  deriving DecidableEq, Repr

def exactId (G : List GNode) (arg : String) : List GNode :=
  filt (fun n => decide (render n.key = arg)) G

def exactLabel (G : List GNode) (arg : String) : List GNode :=
  filt (fun n => decide (n.label = arg)) G

def ciLabel (G : List GNode) (arg : String) : List GNode :=
  filt (fun n => decide (lower n.label = lower arg)) G

def outcome : List GNode → Resolution
  | [] => .notFound
  | [n] => .single n.key
  | ns => .ambiguous (keysOf ns)

def firstNonEmpty : List (List GNode) → List GNode
  | [] => []
  | [] :: rest => firstNonEmpty rest
  | (n :: ns) :: _ => n :: ns

/-- `resolveNodeArg` : id exact → identifiant exact → identifiant insensible à la casse. -/
def resolve (G : List GNode) (arg : String) : Resolution :=
  outcome (firstNonEmpty [exactId render G arg, exactLabel G arg, ciLabel lower G arg])

/-- Un nœud « correspond » à l'argument par l'un des trois critères. -/
def Matches (n : GNode) (arg : String) : Prop :=
  render n.key = arg ∨ n.label = arg ∨ lower n.label = lower arg

theorem firstNonEmpty_mem {x : GNode} :
    ∀ {ls : List (List GNode)}, x ∈ firstNonEmpty ls → ∃ l, l ∈ ls ∧ x ∈ l
  | [], h => by simp [firstNonEmpty] at h
  | [] :: rest, h => by
      obtain ⟨l, hl, hx⟩ := firstNonEmpty_mem (ls := rest) h
      exact ⟨l, List.mem_cons.mpr (Or.inr hl), hx⟩
  | (n :: ns) :: _, h => ⟨n :: ns, List.mem_cons.mpr (Or.inl rfl), h⟩

theorem firstNonEmpty_nil :
    ∀ {ls : List (List GNode)}, firstNonEmpty ls = [] → ∀ l ∈ ls, l = []
  | [], _, _, hl => by simp at hl
  | [] :: rest, h, l, hl => by
      simp only [List.mem_cons] at hl
      cases hl with
      | inl e => exact e
      | inr hr => exact firstNonEmpty_nil (ls := rest) h l hr
  | (_ :: _) :: _, h, _, _ => by simp [firstNonEmpty] at h

theorem candidate_matches {G : List GNode} {arg : String} {n : GNode}
    (h : n ∈ firstNonEmpty [exactId render G arg, exactLabel G arg, ciLabel lower G arg]) :
    n ∈ G ∧ Matches render lower n arg := by
  obtain ⟨l, hl, hn⟩ := firstNonEmpty_mem h
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hl
  rcases hl with e | e | e
  · subst e
    obtain ⟨hg, hp⟩ := mem_filt.mp hn
    exact ⟨hg, Or.inl (of_decide_eq_true hp)⟩
  · subst e
    obtain ⟨hg, hp⟩ := mem_filt.mp hn
    exact ⟨hg, Or.inr (Or.inl (of_decide_eq_true hp))⟩
  · subst e
    obtain ⟨hg, hp⟩ := mem_filt.mp hn
    exact ⟨hg, Or.inr (Or.inr (of_decide_eq_true hp))⟩

theorem outcome_single {l : List GNode} {k : NodeKey} (h : outcome l = .single k) :
    ∃ n, n ∈ l ∧ n.key = k := by
  match l, h with
  | [], h => simp [outcome] at h
  | [n], h =>
    simp only [outcome, Resolution.single.injEq] at h
    exact ⟨n, List.mem_cons.mpr (Or.inl rfl), h⟩
  | _ :: _ :: _, h => simp [outcome] at h

theorem outcome_notFound {l : List GNode} (h : outcome l = .notFound) : l = [] := by
  match l, h with
  | [], _ => rfl
  | [_], h => simp [outcome] at h
  | _ :: _ :: _, h => simp [outcome] at h

theorem outcome_ambiguous {l : List GNode} {ks : List NodeKey} (h : outcome l = .ambiguous ks) :
    2 ≤ ks.length := by
  match l, h with
  | [], h => simp [outcome] at h
  | [_], h => simp [outcome] at h
  | _ :: _ :: rest, h =>
    simp only [outcome, Resolution.ambiguous.injEq] at h
    subst h
    simp [keysOf, List.length_map]

/-- Un résultat `single` est un nœud du graphe qui correspond : jamais de substitution. -/
theorem resolve_single_sound {G : List GNode} {arg : String} {k : NodeKey}
    (h : resolve render lower G arg = .single k) :
    ∃ n, n ∈ G ∧ n.key = k ∧ Matches render lower n arg := by
  obtain ⟨n, hn, hk⟩ := outcome_single h
  obtain ⟨hg, hm⟩ := candidate_matches render lower hn
  exact ⟨n, hg, hk, hm⟩

/-- `notFound` signifie qu'aucun nœud ne correspond par aucun des trois critères. -/
theorem resolve_notFound_sound {G : List GNode} {arg : String}
    (h : resolve render lower G arg = .notFound) : ∀ n ∈ G, ¬ Matches render lower n arg := by
  intro n hn hm
  have hnil := firstNonEmpty_nil (outcome_notFound h)
  rcases hm with e | e | e
  · have : exactId render G arg = [] := hnil _ (List.mem_cons.mpr (Or.inl rfl))
    have hmem : n ∈ exactId render G arg := mem_filt.mpr ⟨hn, decide_eq_true e⟩
    rw [this] at hmem; simp at hmem
  · have : exactLabel G arg = [] :=
      hnil _ (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inl rfl))))
    have hmem : n ∈ exactLabel G arg := mem_filt.mpr ⟨hn, decide_eq_true e⟩
    rw [this] at hmem; simp at hmem
  · have : ciLabel lower G arg = [] :=
      hnil _ (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inl rfl))))))
    have hmem : n ∈ ciLabel lower G arg := mem_filt.mpr ⟨hn, decide_eq_true e⟩
    rw [this] at hmem; simp at hmem

/-- `ambiguous` porte au moins deux candidats. -/
theorem resolve_ambiguous_sound {G : List GNode} {arg : String} {ks : List NodeKey}
    (h : resolve render lower G arg = .ambiguous ks) : 2 ≤ ks.length :=
  outcome_ambiguous h

theorem exactId_eq_byKey (hinj : ∀ a b, render a = render b → a = b) (k : NodeKey) :
    ∀ (G : List GNode), exactId render G (render k) = byKey k G
  | [] => rfl
  | n :: G => by
      by_cases hk : n.key = k
      · have h1 : decide (render n.key = render k) = true := decide_eq_true (hk ▸ rfl)
        have h2 : decide (n.key = k) = true := decide_eq_true hk
        simp only [exactId, byKey, filt, h1, h2]
        exact congrArg _ (exactId_eq_byKey hinj k G)
      · have h1 : decide (render n.key = render k) = false :=
          decide_eq_false (fun e => hk (hinj _ _ e))
        have h2 : decide (n.key = k) = false := decide_eq_false hk
        simp only [exactId, byKey, filt, h1, h2]
        exact exactId_eq_byKey hinj k G

/-- Un id exact gagne toujours : clés distinctes et rendu injectif. -/
theorem resolve_exact_id {G : List GNode} {n : GNode}
    (hinj : ∀ a b, render a = render b → a = b) (hd : Distinct (keysOf G)) (hn : n ∈ G) :
    resolve render lower G (render n.key) = .single n.key := by
  have hex : exactId render G (render n.key) = [n] := by
    rw [exactId_eq_byKey render hinj n.key G]
    exact byKey_of_mem hd hn
  simp only [resolve, hex, firstNonEmpty, outcome]

end Resolution

/-! ## 4. Cypher : filtrer avant de plafonner (D4) -/

section Cypher
variable {α : Type}

/-- Ancien ordre : plafonner les bindings bruts, puis filtrer. -/
def evalOld (n : Nat) (p : α → Bool) (xs : List α) : List α := filt p (tk n xs)

/-- Nouvel ordre : filtrer, puis plafonner les lignes retenues. -/
def evalNew (n : Nat) (p : α → Bool) (xs : List α) : List α := tk n (filt p xs)

/-- Le nouvel ordre contient l'ancien. -/
theorem evalOld_subset_evalNew {p : α → Bool} {x : α} :
    ∀ {n : Nat} {xs : List α}, x ∈ evalOld n p xs → x ∈ evalNew n p xs
  | 0, _, h => by simp [evalOld, tk, filt] at h
  | _ + 1, [], h => by simp [evalOld, tk, filt] at h
  | n + 1, y :: ys, h => by
      unfold evalOld at h
      obtain ⟨hmem, hpx⟩ := mem_filt.mp h
      simp only [tk, List.mem_cons] at hmem
      unfold evalNew
      cases hy : p y with
      | true =>
        simp only [filt, hy, tk, List.mem_cons]
        cases hmem with
        | inl hxy => exact Or.inl hxy
        | inr hx =>
          have ih := evalOld_subset_evalNew (n := n) (xs := ys) (mem_filt.mpr ⟨hx, hpx⟩)
          exact Or.inr ih
      | false =>
        simp only [filt, hy]
        cases hmem with
        | inl hxy => subst hxy; rw [hy] at hpx; cases hpx
        | inr hx =>
          have ih := evalOld_subset_evalNew (n := n) (xs := ys) (mem_filt.mpr ⟨hx, hpx⟩)
          exact mem_tk_succ ih

/-- Quand les matches tiennent dans le budget, le nouvel ordre les rend tous. -/
theorem evalNew_complete {p : α → Bool} {xs : List α} {n : Nat}
    (h : (filt p xs).length ≤ n) : evalNew n p xs = filt p xs :=
  tk_of_length_le h

/-- Contre-exemple décidé : l'ancien ordre vide une requête qui a un match. -/
example : evalOld 1 (fun x => x == 2) [1, 2] = [] := by decide
example : evalNew 1 (fun x => x == 2) [1, 2] = [2] := by decide

end Cypher

/-! ## 5. Observabilité (D5) -/

inductive Level where
  | trace | debug | info
  deriving DecidableEq, Repr

def levelLeDebug : Level → Bool
  | .trace => true
  | .debug => true
  | .info => false

/-- Ancien câblage : le drapeau n'est pas consulté. -/
def traceEnabledOld (_noObs : Bool) (l : Level) (cfgDebug : Bool) : Bool :=
  levelLeDebug l || cfgDebug

/-- Nouveau câblage : `--no-observability` éteint la trace. -/
def traceEnabledNew (noObs : Bool) (l : Level) (cfgDebug : Bool) : Bool :=
  !noObs && (levelLeDebug l || cfgDebug)

theorem noObservability_disables (l : Level) (d : Bool) : traceEnabledNew true l d = false := by
  simp [traceEnabledNew]

theorem old_ignores_flag : traceEnabledOld true .info true = true := rfl

/-- Préfixe de segments. -/
def isPrefixSeg : List String → List String → Prop
  | [], _ => True
  | _ :: _, [] => False
  | a :: as, b :: bs => a = b ∧ isPrefixSeg as bs

theorem isPrefixSeg_append : ∀ (l m : List String), isPrefixSeg l (l ++ m)
  | [], _ => trivial
  | _ :: as, m => ⟨rfl, isPrefixSeg_append as m⟩

/-- Un répertoire relatif est résolu sous le répertoire de sortie. -/
def resolveTraceDir (out cfg : List String) (absolute : Bool) : List String :=
  if absolute then cfg else out ++ cfg

theorem relative_under_output (out cfg : List String) :
    isPrefixSeg out (resolveTraceDir out cfg false) :=
  show isPrefixSeg out (out ++ cfg) from isPrefixSeg_append out cfg

/-! ## 6. Table des ponts bornée (D6) -/

section Bridge
variable (sortDesc : List (NodeKey × Nat) → List (NodeKey × Nat))

def bridgeRows (cap : Nat) (pts : List (NodeKey × Nat)) : List (NodeKey × Nat) :=
  tk cap (sortDesc (filt (fun p => decide (2 ≤ p.2)) pts))

theorem bridgeRows_length_le (cap : Nat) (pts : List (NodeKey × Nat)) :
    (bridgeRows sortDesc cap pts).length ≤ cap := tk_length_le

theorem bridgeRows_degree
    (hsort : ∀ (l : List (NodeKey × Nat)) (x : NodeKey × Nat), x ∈ sortDesc l ↔ x ∈ l)
    (cap : Nat) (pts : List (NodeKey × Nat)) {r : NodeKey × Nat}
    (hr : r ∈ bridgeRows sortDesc cap pts) : 2 ≤ r.2 := by
  have h1 := mem_tk hr
  have h2 := (hsort _ _).mp h1
  have h3 := mem_filt.mp h2
  exact of_decide_eq_true h3.2

end Bridge

/-! ## 7. Granularité (D7) -/

inductive Gran where
  | fine | function | file
  deriving DecidableEq, Repr

/-- CLI → extension → global : fonction pure. -/
def resolveGran (cli perExt : Option Gran) (global : Gran) : Gran :=
  cli.getD (perExt.getD global)

theorem cli_wins (g : Gran) (pe : Option Gran) (gl : Gran) : resolveGran (some g) pe gl = g := rfl
theorem perExt_wins (g gl : Gran) : resolveGran none (some g) gl = g := rfl
theorem global_applies (gl : Gran) : resolveGran none none gl = gl := rfl

/-- Ancien câblage (`Wiring.hs:197`) : une constante. -/
def convertedLevelOld (_cli _perExt : Option Gran) (_global : Gran) : Gran := .function

/-- Nouveau câblage : le niveau résolu est transmis au convertisseur. -/
def convertedLevelNew (cli perExt : Option Gran) (global : Gran) : Gran := resolveGran cli perExt global

theorem wiring_threads_level (cli perExt : Option Gran) (global : Gran) :
    convertedLevelNew cli perExt global = resolveGran cli perExt global := rfl

theorem wiring_bug_refuted : convertedLevelOld none none .file ≠ .file := by decide
theorem wiring_fixed : convertedLevelNew none none .file = .file := rfl

/-! ## 8. Fixtures jouets (corpus `example/ts-lsp-test`) -/

def dbTs : SourceFile :=
  ⟨["src", "db.ts"],
   [⟨.function, "generateId", 4, "export function generateId(): UserId { return nextId++; }"⟩,
    ⟨.function, "dbGetById", 12, "export function dbGetById(id: UserId): User | undefined { … }"⟩],
   [["src", "types.ts"]]⟩

def typesTs : SourceFile :=
  ⟨["src", "types.ts"], [⟨.type, "User", 1, "export interface User { … }"⟩], []⟩

def indexTs : SourceFile :=
  ⟨["src", "index.ts"], [], [["src", "db.ts"], ["src", "types.ts"]]⟩

def corpus : List SourceFile := [indexTs, dbTs, typesTs]

/-- Les clés de `db.ts` sont deux à deux distinctes (instance du théorème général). -/
example : Distinct (assignKeys (dirHash (dirOf dbTs.path)) (fileName dbTs.path) [] dbTs.decls) :=
  assignKeys_distinct _ _ [] _

/-- `index.ts → db.ts → types.ts` : la chaîne d'imports devient un chemin du graphe. -/
example : Reach (builtEdges corpus) (moduleKey indexTs.path) (moduleKey typesTs.path) :=
  chain_lifts
    (ImportChain.step
      (List.mem_cons.mpr (Or.inl rfl))
      (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inl rfl))))
      (List.mem_cons.mpr (Or.inl rfl))
      (ImportChain.step
        (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inl rfl))))
        (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inr (List.mem_cons.mpr (Or.inl rfl))))))
        (List.mem_cons.mpr (Or.inl rfl))
        (ImportChain.refl _)))

/-- Le rendu d'id du modèle jouet : l'identifiant seul ; `lower := id`. -/
def toyNodes : List GNode :=
  [⟨⟨1, "db.ts", .function, "generateId", 0⟩, "generateId"⟩,
   ⟨⟨1, "db.ts", .function, "dbGetById", 0⟩, "dbGetById"⟩]

example : resolve (fun k => k.ident) id toyNodes "dbGetById"
    = .single ⟨1, "db.ts", .function, "dbGetById", 0⟩ := by decide

example : resolve (fun k => k.ident) id toyNodes "dbGetByIdentifier" = .notFound := by decide

end ReviewGraph
