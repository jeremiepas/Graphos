# Task 1 — Create llamacpp model provider configuration — PLAN

**Task slug**: `01-create-llamacpp-model-provider`
**Attempt**: 1
**Status**: pending

## Summary

Create `.opencode/providers/llamacpp.json` with an OpenAI-compatible provider pointing to the local `llama.cpp` server on `http://localhost:8080/v1/chat/completions`, using model `qwen3.6-35b-a3b`. No cloud API keys.

## Detail

### Scope
- Create a new file `.opencode/providers/llamacpp.json`
- Provider name: `llamacpp`
- Model: `qwen3.6-35b-a3b`
- Base URL: `http://localhost:8080/v1/chat/completions`
- No `OPENAI_API_KEY` or any other cloud provider credentials

### Check Criteria

1. **Test**: File exists at `.opencode/providers/llamacpp.json`
   - **PASS**: `test -f .opencode/providers/llamacpp.json` returns 0
2. **Test**: File is valid JSON with required fields
   - **PASS**: `python3 -c "import json; d=json.load(open('.opencode/providers/llamacpp.json')); assert 'llamacpp' in str(d) and d['baseUrl']=='http://localhost:8080/v1/chat/completions' and d['model']=='qwen3.6-35b-a3b'"` returns 0
3. **Test**: No cloud keys in provider config
   - **PASS**: `grep -ci 'OPENAI_API_KEY\|anthropic\|google\|azure' .opencode/providers/llamacpp.json` returns 1 (no matches)
4. **Spec scenarios**: `llamacpp-model-provider` — "provider configuration points to localhost llama.cpp server with correct model"
   - **PASS**: provider file matches spec requirement 1.1, 1.2, 1.3

### Fail conditions
- File does not exist → FAIL
- Invalid JSON → FAIL
- baseUrl differs from `http://localhost:8080/v1/chat/completions` → FAIL
- Model differs from `qwen3.6-35b-a3b` → FAIL
- Cloud API keys present → FAIL

### Affected modules
- New file: `.opencode/providers/llamacpp.json`

### Prerequisites
- `.opencode/providers/` directory exists

### Risks
- Provider schema may differ from what opencode expects; adjust if `openspec validate` fails
