/-
Codec.lean — "URL-addressable view state" requirement.

The hash fragment is modeled at token level (percent-encoding of raw strings is
an implementation detail below this model). Proves:
  * decode_encode      — the codec round-trips: every position is addressable
  * restore_hash_wins  — URL state takes precedence over sessionStorage
  * restore_valid      — restore never yields a stale position (unknown
                         node/community degrades to Overview, no error)
-/
import ViewerNav.State

namespace ViewerNav

/-- One hash-fragment token. -/
inductive Token where
  | str (s : String)
  | num (n : Nat)
deriving Repr, DecidableEq

def encodeTuning (t : Tuning) : List Token :=
  .str "h" :: .num t.hops :: .str "f" :: t.facets.map .str

def decodeFacets : List Token → Option (List String)
  | [] => some []
  | .str f :: rest => (decodeFacets rest).map (f :: ·)
  | .num _ :: _ => none

def decodeTuning : List Token → Option Tuning
  | .str "h" :: .num h :: .str "f" :: rest =>
      (decodeFacets rest).map (fun fs => { hops := h, facets := fs })
  | _ => none

/-- Encode the navigable state into the hash fragment. Position heads are
distinct literals ("o"/"c"/"n") so decoding is unambiguous. -/
def encode (s : ViewerState) : List Token :=
  (match s.pos with
   | .overview => [Token.str "o"]
   | .community c => [Token.str "c", Token.num c]
   | .node c n => [Token.str "n", Token.num c, Token.str n]) ++ encodeTuning s.tuning

def decode : List Token → Option ViewerState
  | .str "o" :: rest => (decodeTuning rest).map (fun t => ⟨.overview, t⟩)
  | .str "c" :: .num c :: rest => (decodeTuning rest).map (fun t => ⟨.community c, t⟩)
  | .str "n" :: .num c :: .str nid :: rest =>
      (decodeTuning rest).map (fun t => ⟨.node c nid, t⟩)
  | _ => none

theorem decodeFacets_map (fs : List String) :
    decodeFacets (fs.map Token.str) = some fs := by
  induction fs with
  | nil => rfl
  | cons f fs ih => simp [decodeFacets, ih]

theorem decodeTuning_encodeTuning (t : Tuning) :
    decodeTuning (encodeTuning t) = some t := by
  obtain ⟨h, fs⟩ := t
  simp [encodeTuning, decodeTuning, decodeFacets_map]

/-- Requirement "URL-addressable view state": the codec round-trips, so every
navigable state has a working deep link. -/
theorem decode_encode (s : ViewerState) : decode (encode s) = some s := by
  obtain ⟨pos, t⟩ := s
  cases pos <;> simp [encode, decode, decodeTuning_encodeTuning]

-- ---------------------------------------------------------------------------
-- Restore precedence + stale-reference check
-- ---------------------------------------------------------------------------

/-- What the loaded document knows: which nodes and communities exist. -/
structure GraphData where
  nodes : List NodeId := []
  comms : List CommunityId := []

def validPos (g : GraphData) : Position → Bool
  | .overview => true
  | .community c => g.comms.contains c
  | .node c n => g.comms.contains c && g.nodes.contains n

/-- Stale-reference check — same rule as viewer.js session restore: an unknown
node or community falls back to the Overview, keeping the tuning. -/
def sanitize (g : GraphData) (s : ViewerState) : ViewerState :=
  if validPos g s.pos then s else { s with pos := .overview }

/-- Load-time restore precedence: URL hash → sessionStorage → initial state. -/
def restore (g : GraphData) (hash : Option (List Token))
    (session : Option ViewerState) : ViewerState :=
  match hash.bind decode with
  | some s => sanitize g s
  | none =>
    match session with
    | some s => sanitize g s
    | none => {}

/-- A sanitized position is always valid (Overview is valid in every graph). -/
theorem sanitize_valid (g : GraphData) (s : ViewerState) :
    validPos g (sanitize g s).pos = true := by
  unfold sanitize
  split
  · next h => exact h
  · rfl

/-- Scenario "Stale deep link degrades to Overview" (generalized): restore
never yields a position the loaded graph does not contain. -/
theorem restore_valid (g : GraphData) (hash : Option (List Token))
    (session : Option ViewerState) :
    validPos g (restore g hash session).pos = true := by
  unfold restore
  cases hash.bind decode with
  | some s => exact sanitize_valid g s
  | none =>
    cases session with
    | some s => exact sanitize_valid g s
    | none => rfl

/-- Requirement "URL state SHALL take precedence over sessionStorage". -/
theorem restore_hash_wins (g : GraphData) (f : List Token)
    (session : Option ViewerState) (s : ViewerState)
    (hdec : decode f = some s) :
    restore g (some f) session = sanitize g s := by
  unfold restore
  simp [hdec]

/-- sessionStorage remains the fallback when the hash is empty. -/
theorem restore_session_fallback (g : GraphData) (s : ViewerState) :
    restore g none (some s) = sanitize g s := rfl

/-- With neither hash nor session, restore yields the initial state. -/
theorem restore_default (g : GraphData) :
    restore g none none = ({} : ViewerState) := rfl

end ViewerNav
