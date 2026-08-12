# Task 1 — Add depth selector markup + CSS to htmlHeader — PLAN

**Task slug**: `01-add-depth-selector-markup-css`
**Attempt**: 1
**Status**: pending

## Summary

Add a `<select id="depthSelector">` with four options (Overview, Community, Full, Custom) to the header in `htmlHeader`, plus a hidden `<input id="neighborhoodHops">` for Custom mode. Add CSS rules matching the existing dark theme. Remove the `<button id="btnBack">` markup. No JavaScript yet.

## Detail

### Scope

- File: `src/Graphos/Infrastructure/Export/HTML.hs`
- Changes to `htmlHeader`: (a) remove `btnBack` button line from the `<header>` block; (b) add `<select id="depthSelector">` with four `<option>` elements in order; (c) add `<input id="neighborhoodHops" type="number" min="1" max="6" value="2">` after the search input within the `.search-box` div; (d) add `.depth-selector` and `.neighborhood-input` CSS rules to the `<style>` block (`.neighborhood-input` hidden by default via `display: none`, shown as `display: inline-block` when `.active` class is applied).

### Check Criteria

**Tests/gates:**
- Command: `cabal build` — must complete with zero warnings (uses `-Wall -Wcompat -Werror` with `--flag dev`)
- Command: `cabal test` — must exit with code 0

**Spec scenarios satisfied:**
- `html-depth-selector/spec.md` — "No back button in the DOM" (partial): removing btnBack satisfies the "no element with id `btnBack` exists" condition
- `html-depth-selector/spec.md` — "Selector present on load and defaults to Overview": adds the `<select>` element (full scenario validated in Task 6)
- `html-depth-selector/spec.md` — "Custom depth shows neighborhood input": adds the `<input>` element (full scenario validated in Task 6)

**PASS conditions:**
1. Generated `graph.html` contains `<select id="depthSelector">` with exactly four `<option>` elements: `Overview`, `Community`, `Full`, `Custom` in that order, and `Overview` is selected by default
2. Generated `graph.html` contains `<input id="neighborhoodHops" type="number" min="1" max="6" value="2">`
3. `rg -c "btnBack" graphos-out/graph.html` returns 0 (zero matches)
4. `cabal build` exits with zero warnings
5. `cabal test` exits with code 0

**FAIL boundaries:**
- FAIL if `<select>` has fewer than 4 options or wrong order
- FAIL if `btnBack` still appears anywhere in the generated HTML (markup, CSS, or JS strings)
- FAIL if `cabal build` produces any warnings (treated as errors via `-Werror`)
- FAIL if `cabal test` exits non-zero

### Affected modules

- `src/Graphos/Infrastructure/Export/HTML.hs` — `htmlHeader` function (template strings for CSS + header markup)

### Prerequisites

- GHC 9.10 via Nix shell (`nix-shell shell.nix`)
- Existing build: `cabal build` passes on the base branch

### Risks

- **CSS specificity**: The new `.depth-selector` and `.neighborhood-input` rules must not conflict with existing `.search-box` styles. Mitigation: inspect the existing `<style>` block structure in `htmlHeader` and add rules following the same cascade pattern.
- **Markup ordering**: The `<select>` must appear in the correct position within the `.search-box` div (after search input, before any existing controls). Mitigation: review the existing `htmlHeader` template to identify the insertion point.
- **Stale references**: The `btnBack` removal is straightforward, but any JS string references to `"btnBack"` in `htmlBody` would create orphan IDs. Mitigation: verify no JS in `htmlBody` references `btnBack` before proceeding.

## Result

<!-- Pending implementation -->
