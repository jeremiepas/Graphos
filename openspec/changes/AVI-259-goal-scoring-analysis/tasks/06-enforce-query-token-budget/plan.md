# Task: Enforce Query Token Budget

## Goal

Enforce token budgets on query responses to prevent context overflow in agent workflows.

## Score: 8.58 (P1) — Sixth highest priority

## Acceptance Criteria

- [ ] Token budget configurable (default 4000 tokens)
- [ ] Query responses truncated to budget
- [ ] Budget-aware formatting in FormatContext module
- [ ] Token counting accurate (not character counting)
- [ ] Tests for budget enforcement edge cases

## Dependencies

- fix-query-relevance-scoring (P0)

## Blocks

- All query features (2 features)
- MCP server tool responses
- Context selection

## Implementation Plan

1. Review current query response formatting
2. Implement token counter (handle Unicode, tabs, newlines)
3. Add budget enforcement to FormatContext
4. Wire into query CLI and MCP handlers
5. Add tests for budget enforcement

## Verification

- Run queries with small budgets, verify truncation
- Test Unicode handling in token counting
- Verify MCP tool responses respect budget
- Run existing query tests
