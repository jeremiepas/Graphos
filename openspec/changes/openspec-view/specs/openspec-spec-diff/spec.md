# Spec: openspec-spec-diff

## ADDED Requirements

### Requirement: Spec diff SHALL identify added, modified, and removed requirements

The spec diff SHALL compare each delta spec against the corresponding main spec and categorize changes as:
- `added` — requirements present in delta spec but not in main spec
- `modified` — requirements present in both but with different content
- `removed` — requirements present in main spec but not in delta spec

#### Scenario: Change categorization
WHEN a spec diff is generated
THEN each change SHALL be categorized as added, modified, or removed

### Requirement: Spec diff SHALL show requirement-level granularity

Each diff entry SHALL include:
- The requirement identifier (e.g., R1, R2)
- The change type (added/modified/removed)
- The relevant content (full requirement text for added, diff for modified, removed text for removed)

#### Scenario: Granular diff output
WHEN a spec diff is displayed
THEN each entry SHALL include requirement ID, change type, and relevant content

### Requirement: Spec diff SHALL validate delta spec fidelity

The spec diff SHALL verify that:
- All requirements in the delta spec have a corresponding main spec entry (for modifications)
- No requirements are silently dropped without explicit removal
- The delta spec structure matches the main spec structure

#### Scenario: Fidelity validation
WHEN a spec diff is generated
THEN the output SHALL include a validation section reporting structural mismatches
