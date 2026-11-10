/-!
# DocLink — modèle formel Lean 4 des passes doc↔code déterministes

Formalisation minimale et certifiée des trois passes de
`deterministic-doc-code-edges` :

  * **co-location** : un document (README à côté du code) est lié au code
    de son répertoire et de ses descendants — jamais hors de son sous-arbre ;
  * **symbol-mention** : un document qui nomme un identifiant whole-word
    (longueur ≥ 4) ayant exactement une définition est lié au nœud la
    définissant ; les identifiants ambigus ou trop courts sont ignorés ;
  * **path-reference** : un document qui cite un chemin de fichier
    explicite (séparateur `/`, extension source connue) est lié aux nœuds
    de ce fichier, même à travers les sous-arbres ; les chemins pendants
    et les noms de fichiers nus sont ignorés.

Chaque passe est une fonction pure du graphe, et chaque garantie est un
théorème prouvé une fois pour toutes :

  * `colocation_subtree`     — toute arête de co-location reste dans le
    sous-arbre du document ;
  * `symbolMention_uniqueDef` — toute arête de mention de symbole vise
    l'unique définition d'un identifiant whole-word de longueur ≥ 4 ;
  * `pathRef_resolves`       — toute arête de référence de chemin résout
    vers un `source_file` existant et porte la relation `documents` ;
  * `documents_semanticSafe` — toute arête émise a une confiance ≥ 0.7,
    donc survit au filtre `edges = semantic`.

Méthodologie `lean-proof-methodology` : invocation goal-guided des
théorèmes (`rw`, `have` de defeq-forcing), pas d'application positionnelle
d'arguments à un théorème portant un binder auto-implicite, et pas de
`by decide` sur des énoncés non réductibles par le noyau. Les primitives
String de la stdlib (`splitOn`, `isPrefixOf`, `take`, `endsWith`) ne se
réduisent pas sous le noyau de Lean 4.29.1 : ce fichier n'utilise que des
helpers structuraux maison (`splitChar`, `wordTokens`, `pathTokens`,
`endsWithStr`) qui, eux, réduisent par `rfl`/`decide`.

Lean 4 core uniquement, pas de Mathlib.
-/

set_option linter.unusedSectionVars false

namespace DocLink

/-! ## 1. Primitives réduisibles -/

/-- Découpe `s` sur le caractère `c`. Récursion structurelle, réduit par `rfl`. -/
def splitChar (c : Char) (s : String) : List String :=
  let rec go : List Char → List Char → List String
    | [], acc => if acc.isEmpty then [] else [String.ofList acc.reverse]
    | x::xs, acc =>
        if x == c then
          (if acc.isEmpty then [] else [String.ofList acc.reverse]) ++ go xs []
        else go xs (x :: acc)
  go s.toList []

/-- Segments de chemin : découpe sur `/`. Réduit par `rfl`. -/
def splitSlash (s : String) : List String := splitChar '/' s

/-- `p` est-il un préfixe (segments) de `t` ? Réduit par `rfl`. -/
def isPrefixL : List String → List String → Bool
  | [], _ => true
  | _::_, [] => false
  | a::ps, b::bs => (a == b) && isPrefixL ps bs

/-- `suffix` est-il un suffixe de `s` ? Via `toList` (réduit par `decide`). -/
def endsWithStr (suffix : String) (s : String) : Bool :=
  let cs := s.toList
  let k := suffix.toList.length
  let n := cs.length
  if k ≤ n then cs.drop (n - k) == suffix.toList else false

/-- Caractère d'identifiant : alphanumérique ou underscore. -/
def isWordChar (c : Char) : Bool := c.isAlphanum || c == '_'

/-- Caractère de token chemin : identifiant + `/ . -`. -/
def isPathChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '/' || c == '.' || c == '-'

/-- Tokens mot (whole-word) : suites de caractères d'identifiant. -/
def wordTokens (s : String) : List String :=
  let rec go : List Char → List Char → List String
    | [], acc => if acc.isEmpty then [] else [String.ofList acc.reverse]
    | x::xs, acc =>
        if isWordChar x then go xs (x :: acc)
        else (if acc.isEmpty then [] else [String.ofList acc.reverse]) ++ go xs []
  go s.toList []

/-- Tokens chemin-like : suites de caractères de chemin. -/
def pathTokens (s : String) : List String :=
  let rec go : List Char → List Char → List String
    | [], acc => if acc.isEmpty then [] else [String.ofList acc.reverse]
    | x::xs, acc =>
        if isPathChar x then go xs (x :: acc)
        else (if acc.isEmpty then [] else [String.ofList acc.reverse]) ++ go xs []
  go s.toList []

/-- Retire un préfixe `./` le cas échéant. -/
def dropDotSlashL (s : String) : String :=
  let cs := s.toList
  if 2 ≤ cs.length && cs.head? == some '.' && cs[1]? == some '/'
  then String.ofList (cs.drop 2) else s

/-- Retire une ponctuation de fin de phrase (`.` `,` `)` `;` `:` `'` `"`)
    le cas échéant — un chemin cité en pleine phrase. -/
def dropTrailingPunct (s : String) : String :=
  let cs := s.toList
  match cs.getLast? with
  | some c => if c == '.' || c == ',' || c == ')' || c == ';' || c == ':' || c == '\'' || c == '"'
              then String.ofList cs.dropLast else s
  | none => s

/-! ## 2. Le modèle de graphe -/

/-- Sortes de nœuds : document ou code. -/
inductive NodeKind where
  | doc | code
  deriving DecidableEq, Repr

/-- Relation d'une arête. -/
inductive Relation where
  | documents | inferred | references
  deriving DecidableEq, Repr

/-- Niveau de confiance d'une arête. Miroir kernel-décidable du `Double`
    Haskell : `documents'` correspond à une confiance ≥ 0.7 (déterministe,
    survit au filtre `edges = semantic`), `inferred'` à la similarité
    basse-confiance des arêtes d'inférence. -/
inductive Confidence where
  | inferred'
  | documents'
  deriving DecidableEq, Repr

/-- Une arête de confiance `documents'` survit-elle au filtre sémantique ? -/
def survivesSemantic : Confidence → Bool
  | .inferred' => false
  | .documents' => true

/-- Un nœud : identifiant, sorte, libellé, fichier source (chemin relatif
    au dépôt), texte (corps du document), et définition de symbole
    (identifiant défini par ce nœud, s'il en définit un). -/
structure Node where
  id       : Nat
  kind     : NodeKind
  label    : String
  source   : String
  text     : String := ""
  defines  : Option String := none
  deriving DecidableEq, Repr

/-- Une arête émise par les passes : source, cible, relation, confiance.
    (Pas de `deriving DecidableEq` sur le champ Float : la confiance est
    le niveau `Confidence`, kernel-décidable.) -/
structure DEdge where
  src : Nat
  dst : Nat
  rel : Relation
  conf : Confidence
  deriving DecidableEq, Repr

/-- Un graphe de code : nœuds déclarés. (Les arêtes émises sont le
    *résultat* des passes, pas une entrée.) -/
structure CodeGraph where
  nodes : List Node
  deriving Repr

namespace CodeGraph

def node? (G : CodeGraph) (v : Nat) : Option Node := G.nodes.find? (·.id = v)

/-- Segments du fichier source d'un nœud. -/
def segs (n : Node) : List String := splitSlash n.source

/-- Extensions de fichiers source reconnues pour la passe path-reference. -/
def sourceExts : List String := [".hs", ".rs", ".ts", ".js", ".py", ".go", ".zig", ".c", ".h", ".elm"]

/-- Un token est un chemin de fichier source : au moins un séparateur `/`,
    et une extension source connue en suffixe (après normalisation `./`
    et ponctuation de fin). -/
def isSourcePathToken (tok : String) : Bool :=
  let t := dropTrailingPunct (dropDotSlashL tok)
  let segs := splitSlash t
  1 < segs.length && sourceExts.any (fun ext => endsWithStr ext t)

end CodeGraph

/-! ## 3. Les trois passes -/

open CodeGraph

/-- Confiance haute portée par toute arête `documents` émise par les passes. -/
def documentsConf : Confidence := .documents'

/-- Passe 1 — co-location : lie chaque nœud document de `G` aux nœuds code
    dont le fichier source vit dans le même répertoire ou un descendant.
    Le garde `isPrefixL` sur les segments encode le sous-arbre. -/
def colocEdges (G : CodeGraph) : List DEdge :=
  G.nodes.flatMap fun d =>
    if d.kind = .doc then
      let dsegs := dropDotSlashL d.source |> splitSlash |> List.dropLast
      G.nodes.flatMap fun c =>
        if c.kind = .code
           && isPrefixL dsegs (splitSlash c.source)
           && d.id ≠ c.id then
          [{ src := d.id, dst := c.id, rel := .documents, conf := documentsConf }]
        else []
    else []

/-- Noms vides ou trop courts (longueur < 4) : jamais des mentions. -/
def mentionable (s : String) : Bool := 4 ≤ s.length

/-- Index de définitions : identifiant → liste des nœuds le définissant. -/
def defIndex (G : CodeGraph) : List (String × List Nat) :=
  let defs := G.nodes.filterMap fun n =>
    match n.defines with
    | some s => some (s, n.id)
    | none => none
  let ids := defs.map (·.1) |>.eraseDups
  ids.map fun s => (s, defs.filterMap fun (s', v) => if s' == s then some v else none)

/-- Résolution unique : `some v` si exactement un nœud définit `s`, sinon `none`. -/
def uniqueDef (G : CodeGraph) (s : String) : Option Nat :=
  match defIndex G |>.lookup s with
  | some [v] => some v
  | _ => none

/-- Passe 2 — symbol-mention : lie chaque document à l'unique nœud définissant
    un identifiant whole-word (longueur ≥ 4) apparaissant dans son texte. -/
def symbolEdges (G : CodeGraph) : List DEdge :=
  G.nodes.flatMap fun d =>
    if d.kind = .doc then
      (wordTokens d.text).eraseDups.flatMap fun tok =>
        if mentionable tok then
          match uniqueDef G tok with
          | some v => if v ≠ d.id then
              [{ src := d.id, dst := v, rel := .documents, conf := documentsConf }]
            else []
          | none => []
        else []
    else []

/-- Passe 3 — path-reference : lie chaque document citant un chemin de
    fichier explicite aux nœuds de ce fichier — à travers les sous-arbres.
    Les chemins pendants et les noms de fichiers nus sont ignorés. -/
def pathRefEdges (G : CodeGraph) : List DEdge :=
  G.nodes.flatMap fun d =>
    if d.kind = .doc then
      (pathTokens d.text).eraseDups.flatMap fun tok =>
        let t := dropTrailingPunct (dropDotSlashL tok)
        if isSourcePathToken tok then
          G.nodes.flatMap fun c =>
            if c.kind = .code && c.source == t && c.id ≠ d.id then
              [{ src := d.id, dst := c.id, rel := .documents, conf := documentsConf }]
            else []
        else []
    else []

/-- Toutes les arêtes déterministes émises par les trois passes. -/
def docCodeEdges (G : CodeGraph) : List DEdge :=
  colocEdges G ++ symbolEdges G ++ pathRefEdges G

/-! ## 4. Garanties

Preuves par inversion de l'appartenance : chaque passe construit ses
arêtes comme un `flatMap` de singletons conditionnels, donc une arête
membre force les gardes à être vraies (`simp only [...] at h`,
`split at h`, `List.mem_flatMap`, `List.mem_singleton`,
`Bool.and_eq_true`, `decide_eq_true_eq`) et la forme du singleton se
substitue dans le but. Les invocations de lemmes sont goal-guided
(`simp only`, `obtain`, `subst`) : aucun argument positionnel à un
théorème porteur de binder, conformément à `lean-proof-methodology`. -/

section Guarantees

/-- Forme d'une arête de co-location : les gardes du constructeur. -/
theorem colocEdges_shape {G : CodeGraph} {e : DEdge}
    (he : e ∈ colocEdges G) :
    ∃ d c : Node, d ∈ G.nodes ∧ c ∈ G.nodes
      ∧ d.kind = .doc ∧ c.kind = .code
      ∧ e.src = d.id ∧ e.dst = c.id
      ∧ isPrefixL (List.dropLast (splitSlash (dropDotSlashL d.source)))
                  (splitSlash c.source) = true := by
  simp only [colocEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨c, hc, he⟩ := he
    split at he
    · rename_i hguard
      simp only [Bool.and_eq_true, decide_eq_true_eq] at hguard
      obtain ⟨⟨hc1, hc2⟩, hne⟩ := hguard
      simp only [List.mem_singleton] at he
      subst he
      exact ⟨d, c, hd, hc, hk, hc1, rfl, rfl, hc2⟩
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- (a) Toute arête de co-location reste dans le sous-arbre du document :
    les segments du répertoire du document sont un préfixe des segments du
    fichier source du code lié. (Le répertoire du document est calculé sur
    son chemin normalisé, comme dans la passe.) -/
theorem colocEdges_subtree {G : CodeGraph} {e : DEdge}
    (he : e ∈ colocEdges G) :
    ∃ d c : Node, d ∈ G.nodes ∧ c ∈ G.nodes
      ∧ d.kind = .doc ∧ c.kind = .code
      ∧ e.src = d.id ∧ e.dst = c.id
      ∧ isPrefixL (List.dropLast (splitSlash (dropDotSlashL d.source)))
                  (splitSlash c.source) = true :=
  colocEdges_shape he

/-- (b) Toute arête de mention de symbole vise l'unique définition d'un
    identifiant whole-word de longueur ≥ 4 cité dans le texte du document. -/
theorem symbolEdges_uniqueDef {G : CodeGraph} {e : DEdge}
    (he : e ∈ symbolEdges G) :
    ∃ d : Node, d ∈ G.nodes ∧ d.kind = .doc
      ∧ ∃ tok, tok ∈ wordTokens d.text ∧ mentionable tok
        ∧ uniqueDef G tok = some e.dst
        ∧ e.rel = .documents := by
  simp only [symbolEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨tok, htok, he⟩ := he
    have htok' : tok ∈ wordTokens d.text := by
      rw [List.mem_eraseDups] at htok; exact htok
    split at he
    · next hment =>
      split at he
      · next hv =>
        split at he
        · next hne =>
          simp only [List.mem_singleton] at he
          subst he
          exact ⟨d, hd, hk, tok, htok', hment, hv, rfl⟩
        · exact absurd he (by simp)
      · exact absurd he (by simp)
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- (c) Toute arête de référence de chemin résout vers un nœud code dont le
    `source` est le chemin cité (normalisé de son préfixe `./` et de sa
    ponctuation de fin), et porte la relation `documents`. -/
theorem pathRefEdges_resolves {G : CodeGraph} {e : DEdge}
    (he : e ∈ pathRefEdges G) :
    ∃ d c : Node, d ∈ G.nodes ∧ c ∈ G.nodes
      ∧ d.kind = .doc ∧ c.kind = .code
      ∧ e.rel = .documents
      ∧ ∃ tok, tok ∈ pathTokens d.text ∧ isSourcePathToken tok
        ∧ c.source = dropTrailingPunct (dropDotSlashL tok) := by
  simp only [pathRefEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨tok, htok, he⟩ := he
    have htok' : tok ∈ pathTokens d.text := by
      rw [List.mem_eraseDups] at htok; exact htok
    split at he
    · next hp =>
      simp only [List.mem_flatMap] at he
      obtain ⟨c, hc, he⟩ := he
      split at he
      · rename_i hguard
        simp only [Bool.and_eq_true, decide_eq_true_eq] at hguard
        obtain ⟨⟨hkc, hsrc⟩, hne⟩ := hguard
        -- hsrc : (c.source == t) = true — via LawfulBEq String, extraire l'égalité
        have hsrc' : c.source = dropTrailingPunct (dropDotSlashL tok) :=
          eq_of_beq hsrc
        simp only [List.mem_singleton] at he
        subst he
        exact ⟨d, c, hd, hc, hk, hkc, rfl, tok, htok', hp, hsrc'⟩
      · exact absurd he (by simp)
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- Toute arête de co-location porte la relation `documents` et la
    confiance haute. -/
theorem colocEdges_semanticShape {G : CodeGraph} {e : DEdge}
    (he : e ∈ colocEdges G) :
    e.rel = .documents ∧ survivesSemantic e.conf = true := by
  simp only [colocEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨c, hc, he⟩ := he
    split at he
    · simp only [List.mem_singleton] at he
      subst he
      exact ⟨rfl, rfl⟩
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- Toute arête de mention de symbole porte la relation `documents` et la
    confiance haute. -/
theorem symbolEdges_semanticShape {G : CodeGraph} {e : DEdge}
    (he : e ∈ symbolEdges G) :
    e.rel = .documents ∧ survivesSemantic e.conf = true := by
  simp only [symbolEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨tok, htok, he⟩ := he
    split at he
    · next hment =>
      split at he
      · next hv =>
        split at he
        · next hne =>
          simp only [List.mem_singleton] at he
          subst he
          exact ⟨rfl, rfl⟩
        · exact absurd he (by simp)
      · exact absurd he (by simp)
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- Toute arête de référence de chemin porte la relation `documents` et la
    confiance haute. -/
theorem pathRefEdges_semanticShape {G : CodeGraph} {e : DEdge}
    (he : e ∈ pathRefEdges G) :
    e.rel = .documents ∧ survivesSemantic e.conf = true := by
  simp only [pathRefEdges, List.mem_flatMap] at he
  obtain ⟨d, hd, he⟩ := he
  split at he
  · next hk =>
    simp only [List.mem_flatMap] at he
    obtain ⟨tok, htok, he⟩ := he
    split at he
    · next hp =>
      simp only [List.mem_flatMap] at he
      obtain ⟨c, hc, he⟩ := he
      split at he
      · simp only [List.mem_singleton] at he
        subst he
        exact ⟨rfl, rfl⟩
      · exact absurd he (by simp)
    · exact absurd he (by simp)
  · exact absurd he (by simp)

/-- Toute arête émise par les trois passes porte la relation `documents`
    et survit au filtre `edges = semantic` (confiance `documents'`, i.e.
    l'équivalent formel de confiance ≥ 0.7). -/
theorem docCodeEdges_semanticSafe {G : CodeGraph} {e : DEdge}
    (he : e ∈ docCodeEdges G) :
    e.rel = .documents ∧ survivesSemantic e.conf = true := by
  simp only [docCodeEdges, List.mem_append] at he
  rcases he with (he | he) | he
  · exact colocEdges_semanticShape he
  · exact symbolEdges_semanticShape he
  · exact pathRefEdges_semanticShape he

end Guarantees

/-! ## 5. Instance jouet -/

section Toy

/-- Un dépôt jouet : README de la bibliothèque jwt (frère du code source),
    ADR central citant un chemin dans un autre sous-arbre, code Haskell et
    Rust, et un doc hors-sujet. -/
def G : CodeGraph :=
  { nodes :=
      [ { id := 100, kind := .doc,  label := "JWT README"
        , source := "libraries/jwt-verifier/README.md"
        , text := "The verifier exposes validateToken and parseConfig. See ./src/lib.rs for details." }
      , { id := 101, kind := .doc,  label := "ADR 0007"
        , source := "docs/adr/0007-task-model.md"
        , text := "The task model lives in src/domain/workflow/task-definition.ts. index.ts is too ambiguous." }
      , { id := 102, kind := .doc,  label := "Unrelated doc"
        , source := "other/NOTES.md"
        , text := "Nothing relevant. Common words like config and handler everywhere." }
      , { id := 200, kind := .code, label := "lib.rs", source := "libraries/jwt-verifier/src/lib.rs"
        , defines := some "validateToken" }
      , { id := 201, kind := .code, label := "Main.hs", source := "libraries/jwt-verifier/app/Main.hs"
        , defines := some "parseConfig" }
      , { id := 202, kind := .code, label := "task-definition.ts"
        , source := "src/domain/workflow/task-definition.ts"
        , defines := some "createTask" }
      ] }

-- Les verdicts des passes, acceptés par le noyau (rfl/decide, pas #eval seul).
example : (colocEdges G).map (·.dst) = [200, 201] := by rfl
example : (symbolEdges G).map (·.dst) = [200, 201] := by rfl
example : (pathRefEdges G).map (·.dst) = [202] := by rfl

-- Garanties jouet, toutes kernel-acceptées :
-- (1) README frère → lié au code de son sous-arbre (200, 201), jamais au
--     code d'un autre sous-arbre (202 absent), ni aux docs (101, 102).
example : (colocEdges G).map (fun e => e.dst) = [200, 201] := by rfl
example : (List.map DEdge.dst (colocEdges G)).contains 202 = false := by rfl
-- (2) mention de symbole unique → lié ; mots communs (config, handler)
--     jamais définis de façon unique → ignorés ; l'unique citation
--     createTask n'apparaît dans AUCUN texte → pas d'arête vers 202.
example : (symbolEdges G).all (fun e => e.conf = .documents') = true := by rfl
-- (3) ADR cite src/domain/workflow/task-definition.ts (autre sous-arbre)
--     → arête 101→202 ; index.ts (pas de séparateur) ignoré ;
--     ./src/lib.rs (dangling pour 100 : pas de nœud de ce chemin) ignoré.
example : (pathRefEdges G).map (·.src) = [101] := by rfl
-- (4) toutes les arêtes émissions portent rel = documents et survivent
--     au filtre sémantique :
example : (docCodeEdges G).all (fun e => e.rel = .documents) = true := by rfl
example : (docCodeEdges G).all (fun e => survivesSemantic e.conf) = true := by rfl
-- Le doc hors-sujet (102) ne produit aucune arête par aucune passe.
example : (docCodeEdges G).all (fun e => e.src ≠ 102) = true := by rfl

end Toy

end DocLink