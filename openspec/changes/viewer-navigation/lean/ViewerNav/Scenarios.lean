/-
Scenarios.lean — every scenario of specs/viewer-navigation/spec.md validated as
an executable example over concrete fixtures. Each block names the spec
scenario it checks. Scenarios about the browser environment itself (no network
on file://, focus landing in the input) are outside this model and noted where
they occur.
-/
import ViewerNav.State
import ViewerNav.Codec
import ViewerNav.History
import ViewerNav.Breadcrumb
import ViewerNav.Keyboard
import ViewerNav.Path

namespace ViewerNav.Scenarios
open ViewerNav

-- Fixtures ------------------------------------------------------------------

/-- A 3-hop chain mod_Auth—fn_verify—mod_Config—doc_readme, communities 483
(code) and 7 (docs), plus an isolated node. -/
def fixG : Graph :=
  [("mod_Auth", "fn_verify"), ("fn_verify", "mod_Config"), ("mod_Config", "doc_readme")]

def fixData : GraphData :=
  { nodes := ["mod_Auth", "fn_verify", "mod_Config", "doc_readme", "zz_isolated"]
  , comms := [483, 7] }

/-- Drill-down into community 483, node mod_Auth selected, doc facet active. -/
def deepState : ViewerState := ⟨.node 483 "mod_Auth", { hops := 2, facets := ["doc"] }⟩

-- Requirement: URL-addressable view state ------------------------------------

/-- Scenario "Deep link restores position". -/
example : restore fixData (some (encode deepState)) none = deepState := by decide

/-- Scenario "URL reflects navigation": copying the URL into a new tab
reproduces the same view. -/
example :
    restore fixData (some (encode ⟨.community 483, {}⟩)) none
      = ⟨.community 483, {}⟩ := by decide

/-- Requirement text "URL state SHALL take precedence over sessionStorage":
a session state pointing elsewhere loses to the hash. -/
example :
    restore fixData (some (encode deepState)) (some ⟨.community 7, {}⟩)
      = deepState := by decide

/-- Scenario "Stale deep link degrades to Overview": unknown node id. -/
example :
    (restore fixData (some (encode ⟨.node 999 "ghost", {}⟩)) none).pos
      = .overview := by decide

-- Scenario "Works on file://": the model has no network by construction —
-- `restore` is a pure function of (hash, session, loaded data).

-- Requirement: History-integrated back and forward ----------------------------

/-- Overview → community 483 → node mod_Auth, as dispatcher steps. -/
def nav : History :=
  steps (History.fresh {})
    [.goto (.community 483), .goto (.node 483 "mod_Auth")]

/-- Scenario "Back returns to previous position": first Back → the community,
second Back → the Overview; the page is never unloaded (goBack is total on the
model — there is no "leave the page" outcome). -/
example : nav.goBack.map (·.current.pos) = some (.community 483) := by decide
example :
    (nav.goBack.bind History.goBack).map (·.current.pos) = some .overview := by decide

/-- Scenario "Forward replays a step back": Back then Forward is exactly the
original history — node re-selected, panel state implied by position. -/
example : nav.goBack.bind History.goForward = some nav := by decide

/-- Scenario "Facet toggles do not pollute history": four tuning actions, then
Back skips all of them and returns to the pre-community position. -/
def tuned : History :=
  steps nav [.toggleFacet "doc", .toggleFacet "code", .setHops 3, .toggleFacet "doc"]

example : tuned.goBack.map (·.current.pos) = nav.goBack.map (·.current.pos) := by decide

/-- Scenario "On-screen controls mirror history". -/
example : (History.fresh {}).canGoBack = false := by decide
example : ((History.fresh {}).push ⟨.community 483, {}⟩).canGoBack = true := by decide

-- Requirement: Breadcrumb trail ----------------------------------------------

/-- Scenario "Trail reflects drill-down": Overview ▸ community ▸ node. -/
example :
    breadcrumb (.node 483 "resolveLoggingConfig")
      = [.overview, .community 483, .node 483 "resolveLoggingConfig"] := by decide

/-- Scenario "Clicking an ancestor navigates": the click is a normal
history-pushing step, so Back returns to the node. -/
example :
    (step nav (.goto (.community 483))).goBack.map (·.current.pos)
      = some (.node 483 "mod_Auth") := by decide

/-- Scenario "Trail present after deep-link restore": all three segments,
no interaction needed. -/
example :
    (breadcrumb (restore fixData (some (encode deepState)) none).pos).length
      = 3 := by decide

-- Requirement: Keyboard navigation map ----------------------------------------

/-- Scenario "Focus search from anywhere" (model half): the shortcut focuses
the search input. (That the key is not typed into it is a DOM concern.) -/
example :
    (handleKey false .search { pos := .community 483 }).searchFocused = true := by
  decide

/-- Scenario "Escape walks up the hierarchy": panel closes first, then up. -/
example : escape (.node 483 "mod_Auth") = .community 483 := by decide
example : escape (escape (.node 483 "mod_Auth")) = .overview := by decide

/-- Scenario "Shortcuts inert while typing". -/
example :
    handleKey true .search { pos := .community 483 } = { pos := .community 483 } := by
  decide

/-- Scenario "Help overlay lists bindings": every table entry appears, and
Escape closes only the overlay. -/
example : overlayEntries.length = 4 := by decide
example :
    keyAction .escapeKey { pos := deepState.pos, helpOpen := true }
      = { pos := deepState.pos, helpOpen := false } := by decide

-- Requirement: In-viewer shortest path between two nodes -----------------------

/-- Scenario "Path found and highlighted": the exact shortest path... -/
example :
    findPath fixG "mod_Auth" "doc_readme"
      = some ["mod_Auth", "fn_verify", "mod_Config", "doc_readme"] := by decide

/-- ...and it reports the hop count (3 hops). -/
example :
    (findPath fixG "mod_Auth" "doc_readme").map (fun p => p.length - 1)
      = some 3 := by decide

/-- Scenario "No path exists": explicit none... -/
example : findPath fixG "mod_Auth" "zz_isolated" = none := by decide

/-- ...and the current view's highlighting is unchanged. -/
def someView : PathView := { base := deepState }
example : requestPath fixG someView "mod_Auth" "zz_isolated" = someView := by decide

/-- Scenario "Cross-community path is visible": mod_Auth sits in community 483,
doc_readme in community 7 — `findPath` takes no community or facet input (see
`Graph`), so the path is found regardless of the current drill-down scope. -/
example : (findPath fixG "mod_Auth" "doc_readme").isSome = true := by decide

/-- Scenario "Clearing the path restores the view". -/
example :
    clearPath (requestPath fixG someView "mod_Auth" "doc_readme") = someView := by
  decide

end ViewerNav.Scenarios
