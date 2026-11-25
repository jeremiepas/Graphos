## REMOVED Requirements

### Requirement: Viewer CSS and JavaScript are source files embedded at compile time

**Reason**: The viewer moves to the `graphos-studio` repository and is served separately at runtime, so it is no longer embedded into the Haskell binary at compile time.

**Migration**: Build and publish the viewer assets from `graphos-studio`; `graphos serve` serves them at runtime instead of the Haskell exporter embedding them with `file-embed`.

## ADDED Requirements

### Requirement: Viewer CSS and JavaScript are served at runtime from the frontend repository

Viewer CSS and JavaScript SHALL live in source files owned by the `graphos-studio` repository and SHALL be served by `graphos serve` at runtime. The Haskell exporter SHALL NOT contain viewer CSS or JavaScript as string literals and SHALL NOT embed viewer assets into the binary at compile time.

#### Scenario: No viewer code in the Haskell module

- **WHEN** the Haskell exporter module is searched for JavaScript statements or CSS rules
- **THEN** none are found; the module contains only document assembly and payload projection

#### Scenario: Viewer edits take effect on serve

- **WHEN** the viewer source is modified in the `graphos-studio` repository and `graphos serve` is run against the published bundle
- **THEN** the served document uses the modified viewer without rebuilding the Haskell binary

## MODIFIED Requirements

### Requirement: The renderer is vendored, not fetched from a CDN

The rendering library SHALL be vendored at a pinned version in the `graphos-studio` repository and served from the local `graphos serve` origin. The served document SHALL NOT reference any external CDN origin, and the vendored library's license SHALL be recorded alongside it. The renderer is no longer embedded into the emitted document; it is served at runtime.

#### Scenario: No external references

- **WHEN** the served document is searched for `http://` or `https://` in `src`/`href` attributes pointing at a CDN
- **THEN** none are found; the only network origin is the local `graphos serve` server

#### Scenario: Offline open works

- **WHEN** the document is served by the local `graphos serve` origin with no external network available
- **THEN** the graph renders and no request beyond the local origin is attempted

#### Scenario: Renderer version is pinned and recorded

- **WHEN** the vendored renderer is inspected
- **THEN** its version is recorded in the repository along with its license file, and the served document reports that version in a comment or meta tag
