# Formal validation of the viewer-navigation proposal (Lean 4)

A dependency-free Lean 4 model of the navigation state machine proposed in
`../specs/viewer-navigation/spec.md`. Every requirement is proved as a theorem
over the model; every spec scenario is checked as an executable example
(`Scenarios.lean`). No `sorry`, no axioms, no external libraries.

## Run

```bash
lake build                          # with a Lean 4 toolchain (≥ 4.29)
nix shell nixpkgs#lean4 -c lake build   # or via nix
```

A successful build **is** the validation — every theorem and example is checked
by the Lean kernel.

## Requirement → theorem map

| Spec requirement | Theorems | Module |
|---|---|---|
| URL-addressable view state | `decode_encode` (round-trip: every state has a working deep link), `restore_hash_wins` (URL > sessionStorage), `restore_session_fallback`, `restore_default`, `restore_valid` (stale link degrades to Overview, generalized) | `Codec.lean` |
| History-integrated back/forward | `goBack_push` (Back = previous position), `goForward_goBack` (Forward replays exactly), `steps_nonpos_back` + `steps_nonpos_backPosition` (facet/hop actions never touch the back stack), `fresh_canGoBack` / `push_canGoBack` (controls mirror history) | `History.lean` |
| Breadcrumb trail | `breadcrumb_length_le` (≤ 3 segments), `breadcrumb_head` (starts at Overview), `breadcrumb_last` (ends at current), `breadcrumb_escape` (parent click ≡ one Escape — breadcrumb and Escape cannot disagree) | `Breadcrumb.lean` |
| Keyboard navigation map | `handleKey_inert_while_typing`, `handleKey_escape_active`, `escape_closes_overlay_first`, `overlay_complete` (overlay generated from the binding table — cannot drift), `escape_escape` (≤ 2 Escapes reach Overview), `escape_rank_lt` (Escape never loops) | `Keyboard.lean`, `Breadcrumb.lean` |
| In-viewer shortest path | `findPath_sound` (a returned path is a genuine undirected walk s→t), `requestPath_nopath` (no path ⇒ view untouched), `clearPath_base` / `clearPath_overlay` / `clearPath_setPath` (clearing restores the prior view exactly) | `Path.lean` |
| Dispatcher invariants | `applyAction_nonpos_pos` (tuning actions never move the position), `applyAction_hops_range` (hops always in [1,6]) | `State.lean` |

`Scenarios.lean` re-checks all 19 spec scenarios on concrete fixtures (3-hop
chain graph, communities 483/7, an isolated node), including the exact shortest
path, its hop count, the disconnected no-path case and cross-community reach.

## What the model abstracts

- **Percent-encoding**: the hash is modeled at token level (`Token`); escaping
  raw strings into URL-safe characters is an implementation detail below the
  model.
- **The DOM/network**: "no network request on `file://`" holds by construction
  (`restore` is a pure function); focus landing inside the input and viewport
  fitting are browser concerns.
- **BFS minimality**: `findPath` is sound by construction (results are
  re-checked); *shortest*-ness is validated on fixtures in `Scenarios.lean`,
  not proved in general.
