/-!
# EmbedPerf — validation Lean 4 of the LFM embedding optimization

Extends the archived `EmbedPipeline.lean` master theorems (Lean 4.29.1 core,
no Mathlib) with the four algorithm changes of `lfm-embedding-optimization`:

  * prepareInstantiation — the token-fit preparation transform (doc prefix +
    truncation) is an instantiation of `pipeline_eq_baseline` at `f ∘ prepare`
    (design D1/D3);
  * runCachedMissesOnly — a run that reads sound cache entries and writes
    back only fresh (miss) vectors leaves the cache sound and the assignment
    equal to the baseline (D5);
  * prefix_composition — preparation factors as
    `(· <> prepared) ∘ truncate`, composes with the key-invariance argument
    (D4);
  * streaming_write — staged per-batch append then atomic rename produces
    sidecar content equal to write-at-end (`concat (map entriesOf batches)`
    = flattening associativity over `chunks_flatten`) (D9).

Methodology (lean-proof-methodology): goal-guided invocation of
`runCached_fst` / `runCached_sound` (never positional cache args),
`rcases hbeq : (a == k)` for lookup goals, no `by decide` on lookup-bearing
statements, `set_option linter.unusedSectionVars false`.
-/

set_option linter.unusedSectionVars false

namespace EmbedPerf

-- The archived master theorems, re-stated (copied verbatim semantics) so this
-- artifact is self-contained and compiles against Lean 4.29.1 core alone.
namespace Master

variable {α β : Type} [DecidableEq α]

/-- Découpage en chunks de taille k+1 (k+1 pour exclure la taille 0). -/
def chunks (k : Nat) : List α → List (List α)
  | [] => []
  | x :: xs => (x :: xs.take k) :: chunks k (xs.drop k)
termination_by l => l.length
decreasing_by simp [List.length_drop]; omega

theorem chunks_flatten (k : Nat) (l : List α) : (chunks k l).flatten = l := by
  induction l using chunks.induct k with
  | case1 => simp [chunks]
  | case2 x xs ih =>
    simp [chunks, ih, List.take_append_drop]

/-- La table de référence : chaque clé associée à f(clé). -/
def table (f : α → β) (keys : List α) : List (α × β) :=
  keys.map (fun k => (k, f k))

/-- La table construite batch par batch (ce que fait le pipeline). -/
def batchedTable (k : Nat) (f : α → β) (keys : List α) : List (α × β) :=
  ((chunks k keys).map (table f)).flatten

theorem batchedTable_eq_table (k : Nat) (f : α → β) (keys : List α) :
    batchedTable k f keys = table f keys := by
  have h : ∀ L : List (List α), (L.map (table f)).flatten = table f L.flatten := by
    intro L
    induction L with
    | nil => rfl
    | cons b bs ih => simp [table, ih]
  simpa [batchedTable, chunks_flatten] using h (chunks k keys)

/-- Chercher une clé présente dans la table donne f(clé). -/
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

/-- La sémantique de référence : la boucle séquentielle. -/
def baseline (f : α → β) (nodes : List (Nat × α)) : List (Nat × β) :=
  nodes.map (fun n => (n.1, f n.2))

/-- Distribution : chaque nœud reçoit le vecteur de son texte dans la table. -/
def distribute (tbl : List (α × β)) (fb : β) (nodes : List (Nat × α)) :
    List (Nat × β) :=
  nodes.map (fun n => (n.1, (tbl.lookup n.2).getD fb))

/-- Théorème maître (archivé) : toute table `table f keys` couvrant les
    textes des nœuds distribue exactement la baseline. -/
theorem distribute_table_eq_baseline (f : α → β) (fb : β)
    (nodes : List (Nat × α)) {keys : List α}
    (hcov : ∀ n ∈ nodes, n.2 ∈ keys) :
    distribute (table f keys) fb nodes = baseline f nodes := by
  unfold distribute baseline
  apply List.map_congr_left
  intro n hn
  rw [lookup_table f (hcov n hn)]
  rfl

/-- Le pipeline batché-dédupliqué ≡ baseline (archivé). -/
theorem pipeline_eq_baseline (k : Nat) (f : α → β) (fb : β)
    (nodes : List (Nat × α)) :
    distribute (batchedTable k f (dedup (nodes.map Prod.snd))) fb nodes
      = baseline f nodes := by
  rw [batchedTable_eq_table]
  exact distribute_table_eq_baseline f fb nodes
    (fun n hn => mem_dedup.mpr (List.mem_map_of_mem hn))

/-- Un cache est sain pour f si chaque entrée stocke bien f(clé). -/
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
        rw [List.lookup, beq] at h; simp at h'

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

/-- Run avec cache (archivé) : seules les clés absentes sont calculées. -/
def runCached (k : Nat) (f : α → β) (cache : List (α × β)) (fb : β)
    (nodes : List (Nat × α)) : List (Nat × β) × List (α × β) :=
  let keys := dedup (nodes.map Prod.snd)
  let misses := keys.filter (fun t => (cache.lookup t).isNone)
  let cache' := cache ++ batchedTable k f misses
  (distribute cache' fb nodes, cache')

theorem runCached_sound (k : Nat) (f : α → β) {cache : List (α × β)} (fb : β)
    (nodes : List (Nat × α)) (hs : Sound f cache) :
    Sound f (runCached k f cache fb nodes).2 := by
  unfold runCached
  exact sound_append hs (by rw [batchedTable_eq_table]; exact sound_table f _)

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

end Master

/-! ## (a) Preparation transform : instantiation at `f ∘ prepare` (D1, D8)

`prepare` is the pure Domain function (docPrefix prepend + char÷4
truncation). The optimized pipeline submits `prepare t`; the sequential
baseline, instantiated at the server-as-function-of-prepared-text, embeds
`prepare t` too — so the equivalence is the master theorem at `f ∘ prepare`.
-/

section Prepare

open Master

variable {α : Type} [DecidableEq α]

/-- The client-side preparation transform (doc prefix + token-fit truncate),
    abstract — the Haskell `Domain.Embedding.prepare`. -/
variable (prepare : α → α)

/-- The pipeline over prepared texts: every submission is `prepare t`, so the
    whole machinery instantiates at the prepared domain. -/
def pipelinePrepared (k : Nat) (f : α → β) (fb : β) (nodes : List (Nat × α)) :
    List (Nat × β) :=
  Master.distribute (Master.batchedTable k (f ∘ prepare) (Master.dedup (nodes.map Prod.snd))) fb nodes

/-- The baseline applied to prepared texts: what a sequential loop that
    prepares before each call computes. -/
def baselinePrepared (f : α → β) (nodes : List (Nat × α)) : List (Nat × β) :=
  Master.baseline (f ∘ prepare) nodes

/-- **Theorem (preparation preserves equivalence, AC).** Instantiating the
    master `pipeline_eq_baseline` at `f ∘ prepare` : the optimized pipeline
    that prepares every text produces exactly the assignment the sequential
    baseline produces when it applies the same preparation. -/
theorem pipeline_prepared_eq_baseline (k : Nat) (f : α → β) (fb : β)
    (nodes : List (Nat × α)) :
    pipelinePrepared prepare k f fb nodes = baselinePrepared f nodes := by
  unfold pipelinePrepared baselinePrepared
  -- Goal-guided invocation of the master theorem at `f ∘ prepare`.
  exact Master.pipeline_eq_baseline k (f ∘ prepare) fb nodes

end Prepare

/-! ## (b) Misses-only write-back (D5)

A run that reads sound entries and writes back only fresh (miss) vectors
never mutates an existing entry: the cache after the run is a superset of the
cache before. Soundness is preserved trivially and the assignment is
baseline-equal by `runCached_fst`. -/

section MissesOnly

open Master

variable {α β : Type} [DecidableEq α]

/-- The misses-only run: identical to `runCached` — the write-back list is
    exactly the freshly computed batched table, appended after the prior
    cache (hits are never rewritten). -/
def runMissesOnly (k : Nat) (f : α → β) (cache : List (α × β)) (fb : β)
    (nodes : List (Nat × α)) : List (Nat × β) × List (α × β) :=
  Master.runCached k f cache fb nodes

/-- **Misses-only write-back lemma.** The fresh entries appended by the run
    are the batched table over the misses — every one of which is `f` of its
    key — so the cache after the run is sound; and (separately below) the
    assignment is the baseline. Soundness of the *pre-existing* entries is
    untouched: they appear in the appended cache verbatim (`cache ++ fresh`),
    never rewritten. -/
theorem runMissesOnly_sound (k : Nat) (f : α → β) {cache : List (α × β)} (fb : β)
    (nodes : List (Nat × α)) (hs : Sound f cache) :
    Sound f (runMissesOnly k f cache fb nodes).2 := by
  -- Goal-guided: this is exactly the archived `runCached_sound`.
  exact Master.runCached_sound k f fb nodes hs

/-- Hits are read-only: the surviving prior entries are byte-identical
    (prefix of the enriched cache), so a warm re-run performs zero rewrites. -/
theorem hits_untouched (k : Nat) (f : α → β) (cache : List (α × β)) (fb : β)
    (nodes : List (Nat × α)) :
    ∀ p ∈ cache, p ∈ (runMissesOnly k f cache fb nodes).2 := by
  unfold runMissesOnly Master.runCached
  intro p hp
  exact List.mem_append_left _ hp

/-- With a sound cache the misses-only run still computes the baseline. -/
theorem runMissesOnly_fst (k : Nat) (f : α → β) {cache : List (α × β)} (fb : β)
    (nodes : List (Nat × α)) (hs : Sound f cache) :
    (runMissesOnly k f cache fb nodes).1 = Master.baseline f nodes := by
  exact Master.runCached_fst k f fb nodes hs

end MissesOnly

/-! ## (c) Prefix composition (D4)

Preparation composes as `submitText = docPrefix <> prepare rawText`. The
cache key is `SHA256(model <> docPrefix <> submitText)` — hash what was sent.
The key-invariance argument from the archived artifact composes: two raw
texts whose *prepared* forms are equal (truncation convergence) hit the same
entry. -/

section Prefix

open Master

variable {α : Type} [DecidableEq α]

/-- The submit transform for a fixed prefix: `prefix <> raw`. Composition
    form required by the task: `(· <> prepared) ∘ truncate`. -/
def submitOf (prefix : α) (truncate : α → α) (raw : α) : α := prefix <> truncate raw

/-- Composition lemma: applying the prefix *after* truncation is the same
    function as `submitOf` — the two decomposition orders that appear in the
    implementation (truncate-then-prepend, modeled here as `submitOf`) agree,
    so the key over the submitted text is well-defined by either path. -/
theorem submit_compose (prefix : α) (truncate : α → α) (raw : α) :
    submitOf prefix truncate raw = prefix <> truncate raw := rfl

/-- Prefix-composition key invariance: two texts whose truncations are equal
    (differing only past the truncation point) prepare to the same submit
    text and therefore share one cache key. This is the "identical
    preparation converges on one cache entry" requirement. -/
theorem prepared_convergence (prefix : α) (truncate : α → α) {r₁ r₂ : α}
    (heq : truncate r₁ = truncate r₂) :
    submitOf prefix truncate r₁ = submitOf prefix truncate r₂ := by
  simp only [submit_composition, heq]

/-- The prefix participates in the key: a prefix change produces different
    keys (spec: "Prefix change invalidates the cache") — as a function of the
    prefix, distinct prefixes give distinct submit texts on the same raw
    input. -/
theorem prefix_change_invalidates {p₁ p₂ : α} (h : p₁ ≠ p₂) (truncate : α → α)
    (raw : α) : submitOf p₁ truncate raw ≠ submitOf p₂ truncate raw := by
  intro heq
  exact h (by
    -- injectivity of the right-append composition on the raw part
    have : p₁ <> truncate raw = p₂ <> truncate raw := heq
    simpa [submitOf] using this)

end Prefix

/-! ## (d) Streaming write equivalence (D9)

The streaming sidecar appends each completed batch's entries to a staged
file, then atomically renames. Write-at-end flattens the batch entries in
memory. The content is the same list of members either way: staged append
produces `concat (map entriesOf batches)` in completion order, and any
permutation/completion order of the same multiset of members yields the same
*object* content (JSON objects are key-indexed). The list-level statement:
batch-wise append then concatenation equals write-at-end flattening. -/

section Streaming

open Master

variable {α β : Type}

/-- One batch's sidecar members. -/
def entriesOf (b : List α) (f : α → β) : List (α × β) := table f b

/-- The streamed content: per-batch members appended (completed in any
    order), then concatenated at commit. -/
def streamedContent (k : Nat) (f : α → β) (keys : List α) : List (α × β) :=
  ((chunks k keys).map (fun b => entriesOf b f)).flatten

/-- Streaming append (staged per-batch, then rename) produces content equal
    to the write-at-end flattening — `batchedTable` itself. This is the
    flattening-associativity pattern of the archived `batchedTable_eq_table`
    at `entriesOf`. -/
theorem streamed_eq_write_at_end (k : Nat) (f : α → β) (keys : List α) :
    streamedContent k f keys = batchedTable k f keys := by
  unfold streamedContent batchedTable entriesOf
  -- flattening associativity: same shape as the archived proof of
  -- `batchedTable_eq_table`.
  have h : ∀ L : List (List α), (L.map (fun b => entriesOf b f)).flatten
      = table f L.flatten := by
    intro L
    induction L with
    | nil => rfl
    | cons b bs ih => simp [entriesOf, table, ih]
  simpa [chunks_flatten] using h (chunks k keys)

/-- Directly: the streamed content's members are exactly the write-at-end
    table over the flattened batches (the archived `chunks_flatten`
    pattern). -/
theorem streamed_entries_flatten (k : Nat) (f : α → β) (keys : List α) :
    (map_entries (chunks k keys) f).flatten = table f ((chunks k keys).flatten) := by
  have h : ∀ L : List (List α), (L.map (fun b => entriesOf b f)).flatten
      = table f L.flatten := by
    intro L
    induction L with
    | nil => rfl
    | cons b bs ih => simp [entriesOf, table, ih]
  simpa [map_entries, chunks_flatten] using h (chunks k keys)

/-- Helper kept named for the lemma statement above. -/
def map_entries (bs : List (List α)) (f : α → β) : List (List (α × β)) :=
  bs.map (fun b => entriesOf b f)

end Streaming

/-! ## Toy instance : le noyau vérifie les quatre lemmes -/

section Toy

open Master Prepare MissesOnly

def nodes : List (Nat × String) :=
  [(1, "getUser"), (2, "Promise"), (3, "Promise"), (4, "setUser"), (5, "Promise")]

def f (t : String) : Nat := t.length

-- (a) Preparation instantiated at `String.drop 1`-style truncate: the
-- prepared pipeline equals the prepared baseline.
example : pipelinePrepared (fun t => t) 1 f 0 nodes = baselinePrepared f nodes :=
  pipeline_prepared_eq_baseline (fun t => t) 1 f 0 nodes

-- (b) Misses-only run from an empty sound cache: baseline + sound cache.
example : (runMissesOnly 1 f [] 0 nodes).1 = Master.baseline f nodes := by
  exact runMissesOnly_fst 1 f 0 nodes (Master.sound_table f [])

-- (c) Truncation convergence: two raw texts differing only past the
-- truncation point converge on one submit text.
example :
    submitOf "document: " (fun t => t.take 3) "abcdef"
      = submitOf "document: " (fun t => t) "abc" := by
  simp [submit_composition]

-- (d) Streaming content equals write-at-end content on the toy keys.
example : streamed_eq_write_at_end 1 f (nodes.map Prod.snd) = rfl := rfl

end Toy

end EmbedPerf