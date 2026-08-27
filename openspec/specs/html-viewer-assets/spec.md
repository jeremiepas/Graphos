# html-viewer-assets Specification

## Purpose
Make the viewer editable and testable. Today 675 lines of CSS and JavaScript live as Haskell
string literals inside a 983-line module (`HTML.hs:60–173` for `<head>` and CSS,
`HTML.hs:176–804` for the application), with no linting, no formatting, no tests, and a renderer
loaded from an unpinned CDN URL (`HTML.hs:68`). The consequences are visible in the file: three
near-identical vis-network `options` blocks (`:352–384`, `:429–470`, `:713–754`), options placed
in the wrong section, and emitted CSS classes with no matching rules.
## Requirements
### Requirement: Viewer CSS and JavaScript are source files embedded at compile time

Viewer CSS and JavaScript SHALL live in dedicated `.css` and `.js` source files under an
`assets/viewer/` directory and SHALL be embedded into the binary at compile time. The Haskell
exporter SHALL NOT contain viewer CSS or JavaScript as string literals.

- **Plan**: String-literal JS cannot be linted, formatted, diffed usefully, or covered by any
  tooling; `file-embed` is already an accepted mechanism in this repository.
- **Do**: Move the assets out, embed them, assemble the document from header + payload + assets.
- **Check**: Scenarios below; a grep-based test asserts no `<script>`/`<style>` body text remains
  in the Haskell module.
- **Act**: If embedding inflates build time noticeably, split assets so only the viewer bundle is
  embedded and measure again.

#### Scenario: No viewer code in the Haskell module

- **WHEN** the exporter module is searched for JavaScript statements or CSS rules
- **THEN** none are found; the module contains only document assembly and payload projection

#### Scenario: Rebuild picks up asset edits

- **WHEN** `assets/viewer/viewer.js` is modified and the project is rebuilt
- **THEN** the emitted `graph.html` contains the modified script

#### Scenario: Emitted assets are byte-identical to their sources

- **WHEN** `graph.html` is generated
- **THEN** the embedded CSS and JS are byte-identical to the corresponding asset files, apart
  from the surrounding `<style>`/`<script>` tags

### Requirement: The renderer is vendored, not fetched from a CDN

The rendering library SHALL be vendored into the repository at a pinned version and embedded in
the emitted document. The document SHALL NOT reference any external origin, and opening it from
`file://` SHALL issue zero network requests. The vendored library's license SHALL be recorded
alongside it.

- **Plan**: `HTML.hs:68` loads `https://unpkg.com/vis-network/standalone/umd/vis-network.min.js`
  unpinned, with a `window._visLoadFailed` fallback that only prints an error — so the artifact
  the spec calls "self-contained" (`html-lod-viewer/spec.md:72–74`) is unusable offline or when
  unpkg changes its bundle.
- **Do**: Vendor a pinned bundle, embed it, delete the CDN tag and the load-failure path.
- **Check**: Scenarios below.
- **Act**: If the embedded renderer pushes small graphs over an acceptable floor size, add an
  opt-in `--external-renderer` flag rather than reinstating the CDN by default.

#### Scenario: No external references

- **WHEN** the emitted document is searched for `http://` or `https://` in `src`/`href`
  attributes
- **THEN** none are found

#### Scenario: Offline open works

- **WHEN** the document is opened from `file://` with networking disabled
- **THEN** the graph renders and no request is attempted

#### Scenario: Renderer version is pinned and recorded

- **WHEN** the vendored renderer is inspected
- **THEN** its version is recorded in the repository along with its license file, and the
  emitted document reports that version in a comment or meta tag

### Requirement: Renderer options are defined once

Renderer configuration SHALL be defined in exactly one object in the viewer source, with per-depth
differences expressed as overrides of that object. Interaction options SHALL be placed in the
section the renderer expects.

- **Plan**: Three near-identical `options` literals exist (`HTML.hs:352–384`, `:429–470`,
  `:713–754`), and `hideEdgesOnDrag`/`hideEdgesOnZoom` sit inside `physics` (`:458–459`,
  `:742–743`) instead of `interaction`, so the documented anti-freeze mitigation required by
  `html-lod-viewer/spec.md:44` is inert.
- **Do**: Single base options object plus named overrides; move the interaction keys.
- **Check**: Scenarios below.
- **Act**: Add a lint rule or test asserting a single options definition so duplication cannot
  reappear.

#### Scenario: One options definition

- **WHEN** the viewer source is searched for renderer options objects
- **THEN** exactly one base definition exists, and each depth level applies a named override of it

#### Scenario: Drag optimisation is active

- **WHEN** the viewer renders at any depth
- **THEN** `hideEdgesOnDrag` and `hideEdgesOnZoom` are set within the interaction section and take
  effect during pan and zoom

### Requirement: Emitted CSS covers every emitted class

Every CSS class name written by the viewer JavaScript SHALL have a matching rule in the viewer
stylesheet.

- **Plan**: `renderApiResults` emits `.search-verdict` (`HTML.hs:565`), `.search-suggestions`
  (`:568`) and `.result-item.scored` (`:572`), none of which appear in the stylesheet
  (`HTML.hs:69–120`), so API search results render unstyled.
- **Do**: Add the missing rules; add a test that cross-checks class names against the stylesheet.
- **Check**: Scenario below.
- **Act**: Keep the cross-check test in CI so new classes cannot ship unstyled.

#### Scenario: No unstyled classes

- **WHEN** the class names used in the viewer JavaScript are compared with the selectors in the
  viewer stylesheet
- **THEN** every used class has at least one matching rule

### Requirement: The generated document is syntactically valid and tested

The build SHALL include automated tests over a generated document produced from a non-empty
graph, covering payload shape, budget conformance and JavaScript syntax validity.

- **Plan**: The only existing test module builds an *empty* graph and asserts label fallback
  (`tests/Graphos/Infrastructure/Export/HTMLSpec.hs`, 37 lines); both prior HTML changes waived
  viewer tests, and the resulting defects shipped.
- **Do**: Add golden-file and property tests plus a syntax check of the emitted script.
- **Check**: Scenarios below; `cabal test` green.
- **Act**: Every viewer defect fixed after this change gains a regression test in the same suite.

#### Scenario: Non-empty graph is exercised

- **WHEN** the test suite runs
- **THEN** at least one test exports a graph with nodes, edges and communities and asserts on the
  emitted document

#### Scenario: Emitted script parses

- **WHEN** the emitted document's script content is parsed by a JavaScript parser
- **THEN** parsing succeeds with no syntax errors

#### Scenario: Golden payload shape is pinned

- **WHEN** the payload projection changes in a way that alters emitted keys
- **THEN** the golden test fails, requiring the change to be acknowledged explicitly

