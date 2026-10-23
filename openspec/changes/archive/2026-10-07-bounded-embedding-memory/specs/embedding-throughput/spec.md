## MODIFIED Requirements

### Requirement: Configurable batch size and concurrency

The embedding configuration SHALL accept optional `batchSize` and `concurrency` keys with defaults of 64 and 1 respectively. Absent keys MUST fall back to the defaults. The batch size MUST be at least 1; a value below 1 MUST be rejected at load time.

#### Scenario: Defaults when keys absent
- **WHEN** `graphos.yaml` contains `embedding: {enabled: true}` with no `batchSize` or `concurrency` keys
- **THEN** the effective batch size is 64 and the effective concurrency is 1

#### Scenario: Invalid batch size rejected
- **WHEN** `graphos.yaml` contains `embedding: {batchSize: 0}`
- **THEN** configuration loading fails with an error naming the invalid key