# Spec: openspec-change-list

## ADDED Requirements

### Requirement: Change listing SHALL include all required fields

The change list SHALL display the following fields for each change:
- `name` — the change identifier (directory name under `openspec/changes/`)
- `status` — one of: `in-progress`, `complete`, `archived`, `no-tasks`
- `completedTasks` — number of completed tasks
- `totalTasks` — total number of tasks defined
- `lastModified` — ISO 8601 timestamp of last modification

#### Scenario: All changes listed with required fields
WHEN a user requests the change list
THEN the output SHALL include name, status, completedTasks, totalTasks, and lastModified for every change

### Requirement: Change listing SHALL be sorted by recency

The change list SHALL be sorted by `lastModified` in descending order (most recent first).

#### Scenario: Sorted output
WHEN a change list is generated
THEN changes SHALL appear ordered by lastModified descending

### Requirement: Change listing SHALL filter by status when requested

The change list SHALL support filtering by status when a status filter is provided.

#### Scenario: Filtered output
WHEN a status filter is provided
THEN only changes matching the filter SHALL appear in the output
