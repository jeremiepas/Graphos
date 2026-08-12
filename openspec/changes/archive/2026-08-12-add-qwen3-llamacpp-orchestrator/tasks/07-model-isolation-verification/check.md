# Task 7 — Model isolation verification — CHECK

**Task slug**: `07-model-isolation-verification`
**Attempt**: 1
**Status**: PASS

## Summary

Executed all Check Criteria from plan.md verbatim. All tests passed.

## Criterion Execution

### Criterion 1: No cloud API keys in provider config
- **Test**: `grep -ci 'OPENAI_API_KEY\|anthropic\|google\|azure' .opencode/providers/llamacpp.json` = 0
- **Result**: **PASS** — no cloud API keys found

### Criterion 2: All requests go to localhost only
- **Test**: `grep '"baseUrl"' .opencode/providers/llamacpp.json | grep -c 'localhost'`
- **Result**: **PASS** — baseUrl points to localhost:8080

### Criterion 3: Spec scenarios — llamacpp-model-provider
- **Spec requirement 7.1**: zero cloud API calls
  - **Result**: **PASS** — provider config uses only localhost
- **Spec requirement 7.2**: local inference via llama.cpp
  - **Result**: **PASS** — model=qwen3.6-35b-a3b configured

## Result

**PASS** — All criteria met. Proceed to Act.
