/-
Keyboard.lean — "Keyboard navigation map" requirement.

Models the declarative shortcut table (design Decision 4) and the
inert-while-typing guard. Proves:
  * handleKey_inert_while_typing — bound keys typed into an input do nothing
  * handleKey_escape_active      — Escape stays active while typing
  * escape_closes_overlay_first  — Escape closes only the overlay when open
  * overlay_complete             — the help overlay lists every binding
                                   (generated from the same table, cannot drift)
-/
import ViewerNav.State
import ViewerNav.Breadcrumb

namespace ViewerNav

inductive Key where
  | escapeKey
  | search
  | fit
  | help
  | char (c : Char)
deriving Repr, DecidableEq

/-- UI state the shortcuts act on. `fit` changes only the viewport, which this
model does not track. -/
structure UIState where
  pos : Position := .overview
  searchFocused : Bool := false
  helpOpen : Bool := false
deriving Repr, DecidableEq

/-- What each key does. Escape is hierarchical dismissal: overlay first, then
the position hierarchy (panel → level up → stay, via `escape`). -/
def keyAction (k : Key) (u : UIState) : UIState :=
  match k with
  | .escapeKey =>
      if u.helpOpen then { u with helpOpen := false }
      else { u with pos := escape u.pos }
  | .search => { u with searchFocused := true }
  | .fit => u
  | .help => { u with helpOpen := true }
  | .char _ => u

/-- The document-level handler: all shortcuts except Escape are inert while
focus is inside a text input. -/
def handleKey (typing : Bool) (k : Key) (u : UIState) : UIState :=
  if typing && !(k == Key.escapeKey) then u else keyAction k u

/-- Scenario "Shortcuts inert while typing": a bound non-Escape key pressed
while typing changes nothing (the character just lands in the input). -/
theorem handleKey_inert_while_typing (k : Key) (u : UIState)
    (h : (k == Key.escapeKey) = false) : handleKey true k u = u := by
  simp [handleKey, h]

/-- Escape remains active even while typing (it is the dismissal key). -/
theorem handleKey_escape_active (u : UIState) :
    handleKey true Key.escapeKey u = keyAction Key.escapeKey u := by
  simp [handleKey]

/-- Scenario "Help overlay lists bindings" (dismissal half): when the overlay
is open, Escape closes only the overlay — the position does not move. -/
theorem escape_closes_overlay_first (u : UIState) (h : u.helpOpen = true) :
    (keyAction Key.escapeKey u).pos = u.pos ∧
    (keyAction Key.escapeKey u).helpOpen = false := by
  simp [keyAction, h]

-- ---------------------------------------------------------------------------
-- Declarative shortcut table (design Decision 4)
-- ---------------------------------------------------------------------------

structure Shortcut where
  key : Key
  description : String
deriving Repr, DecidableEq

/-- The single source of truth for bindings AND the overlay. -/
def shortcutTable : List Shortcut :=
  [ ⟨.search, "focus search"⟩
  , ⟨.escapeKey, "close panel / up one level"⟩
  , ⟨.fit, "fit graph to viewport"⟩
  , ⟨.help, "show this overlay"⟩ ]

/-- The overlay is generated from the table. -/
def overlayEntries : List String := shortcutTable.map (·.description)

/-- Scenario "Help overlay lists bindings": one overlay line per binding —
by construction they cannot drift. -/
theorem overlay_complete : overlayEntries.length = shortcutTable.length := by
  simp [overlayEntries]

end ViewerNav
