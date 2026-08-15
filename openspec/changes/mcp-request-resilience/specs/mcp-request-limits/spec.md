## ADDED Requirements

### Requirement: Per-request node cap

The system SHALL bound the number of nodes expanded by an MCP request to a
configurable `maxRequestNodes`, returning partial results with truncation
metadata when the cap is reached.

#### Scenario: expansion hits node cap
- **WHEN** `get_neighbors` targets a node whose neighborhood exceeds `maxRequestNodes`
- **THEN** at most `maxRequestNodes` nodes are returned
- **AND** the response includes `truncated: true` and an `omitted` count

#### Scenario: small neighborhood not truncated
- **WHEN** a neighborhood is within the cap
- **THEN** the full result is returned with `truncated: false`

### Requirement: Per-request timeout

The system SHALL bound each MCP request to a configurable wall-clock timeout and
return a well-formed partial result on expiry instead of a transport error.

#### Scenario: request exceeds timeout
- **WHEN** a `select_context` request exceeds the configured timeout
- **THEN** the tool returns the results gathered so far with `truncated: true`
- **AND** does not return a `-32001` timeout error

#### Scenario: fast request unaffected
- **WHEN** a request completes before the timeout
- **THEN** it returns normally with no truncation flag set

### Requirement: Community expansion guard

The system SHALL cap the number of community members returned by
`get_community` and report omissions.

#### Scenario: mega-community query bounded
- **WHEN** `get_community` targets a community of 40,000 members
- **THEN** at most `maxRequestNodes` members are returned with an `omitted` count

### Requirement: Configurable limits

The system SHALL expose `maxRequestNodes` and request timeout via config and MCP
tool parameters.

#### Scenario: caller overrides cap
- **WHEN** a caller passes a smaller `maxRequestNodes` parameter
- **THEN** the request honors the smaller value
