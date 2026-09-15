# studio-design-system

The visual foundation of Graphos Studio: tokens, components, theming. Every studio surface is
built from it — no ad-hoc styling.

## ADDED Requirements

### Requirement: Design tokens

The studio SHALL define its visual language as a single token module — color roles (surface,
text, accent, success/warning/danger, graph-canvas), a type scale, a spacing scale, radii and
elevation — and all studio styling SHALL reference tokens, never literal values. Community
colors from the loaded graph SHALL enter the system as a dynamic palette role, harmonized for
contrast against both themes rather than used raw.

- **Plan**: One place to change the look; the token module is to CSS what the shortcut table
  was to keybindings — generated surfaces cannot drift.
- **Do**: `Studio.DesignSystem.Tokens` consumed by all view modules; a lint check (elm-review
  rule or grep test) forbids literal colors outside the token module.
- **Check**: Scenarios below.
- **Act**: If community palettes clash with the accent role, add an automatic
  contrast-adjustment step and record the algorithm.

#### Scenario: No literal styling outside tokens

- **WHEN** the studio sources are checked for hex colors or px literals outside the token
  module
- **THEN** none are found

#### Scenario: Community colors are harmonized

- **WHEN** a graph whose community colors have poor contrast against the dark theme is loaded
- **THEN** rendered community colors are adjusted to meet the token contrast floor while
  staying distinguishable

### Requirement: Component library

The studio SHALL provide a component module — buttons, inputs, select, sliders, panels,
tabs, dialogs, toasts, badges, empty states, loading states — and studio views SHALL compose
these components rather than raw HTML for interactive elements. Components SHALL be keyboard
accessible (focus order, Enter/Space activation, Escape dismissal for dialogs) and SHALL
render correctly in both themes.

- **Plan**: The workbench has many panels (editor, groups, extraction, connection); a
  component library keeps them one product instead of five widgets.
- **Do**: `Studio.DesignSystem.Components`, each component themed from tokens only.
- **Check**: Scenarios below.
- **Act**: Grow the library on demand; a view needing a third variant of something is the
  trigger to generalize, not to inline styles.

#### Scenario: Dialogs are keyboard-complete

- **WHEN** a confirmation dialog is open
- **THEN** focus is trapped inside it, Enter confirms, Escape cancels, and focus returns to
  the invoking control on close

#### Scenario: Components are theme-correct

- **WHEN** the theme toggles while any component gallery view is rendered
- **THEN** every component re-renders with the other theme's tokens and no unstyled element
  appears

### Requirement: Light and dark themes

The studio SHALL ship light and dark themes selected by explicit user choice with an
operating-system-preference default, persisted across sessions, and applied without reload —
including the graph canvas (background, edge and label colors re-derived from tokens).

- **Plan**: Long sessions staring at graphs are exactly where dark mode matters; the canvas
  must follow or the app looks broken.
- **Do**: Theme = token binding; the renderer port receives canvas tokens on theme change.
- **Check**: Scenarios below.
- **Act**: If per-graph theme preference is requested later, extend the persistence key —
  the token architecture does not change.

#### Scenario: Theme follows the user

- **WHEN** a user with OS dark preference first opens the studio, then explicitly selects
  light
- **THEN** the first render is dark, the switch to light applies without reload including the
  canvas, and light is restored on the next visit

#### Scenario: Canvas obeys the theme

- **WHEN** the theme changes while a community drill-down is rendered
- **THEN** canvas background, edge colors and label colors update to the new theme's tokens
  without re-fetching or re-laying-out the graph
