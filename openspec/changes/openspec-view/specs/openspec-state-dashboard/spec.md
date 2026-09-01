# Spec: openspec-state-dashboard

## ADDED Requirements

### Requirement: Dashboard SHALL show aggregate statistics

The dashboard SHALL display:
- Total number of changes (by status: in-progress, complete, archived, no-tasks)
- Total number of specs across all changes
- Total number of tasks across all changes (completed vs total)
- Number of changes with all artifacts complete vs pending

#### Scenario: Aggregate statistics
WHEN the dashboard is requested
THEN the output SHALL include counts for all categories listed above

### Requirement: Dashboard SHALL show per-change progress

For each change, the dashboard SHALL display:
- Change name and status
- Task progress (completed/total)
- Artifact completeness (which artifacts exist)
- Last modified timestamp

#### Scenario: Per-change progress
WHEN the dashboard is displayed
THEN each change SHALL show name, status, task progress, artifact status, and last modified

### Requirement: Dashboard SHALL highlight blockers and risks

The dashboard SHALL highlight:
- Changes with tasks but no artifacts created
- Changes with artifacts but no tasks defined
- Changes that have not been modified recently (stale changes)
- Changes with missing required artifacts per schema

#### Scenario: Risk highlighting
WHEN the dashboard is generated
THEN the output SHALL include a risk section listing any identified issues
