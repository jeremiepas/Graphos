# Task 7 — Model isolation verification — PLAN

**Task slug**: `07-model-isolation-verification`
**Attempt**: 1
**Status**: pending

## Summary

Run the orchestrator with `OPENAI_API_KEY` present in the environment to verify zero cloud API requests are made. All requests must target `http://localhost:8080` only. Verify `orchestrate.py` fails fast if llama.cpp is unreachable.

## Detail

### Scope
- Execute `openspec-orch start test-e2e-orch` with `OPENAI_API_KEY` set in environment
- Monitor network traffic to confirm no requests leave localhost:8080
- Verify `orchestrate.py` emits clear error when llama.cpp server is not available
- No firewall rules needed if configuration is correct

### Check Criteria

1. **Test**: Zero requests to non-localhost endpoints during run
   - **PASS**: Run `socat -t0 -t0 -T10 -v UDP4-CONNECT:443:127.0.0.1:443` in background; during orchestrator run, check no outbound traffic on port 443 except localhost
2. **Test**: `OPENAI_API_KEY` is ignored by orchestrator provider config
   - **PASS**: Set `export OPENAI_API_KEY=fake_key`, run `openspec-orch start test-e2e-orch` — process starts normally (not using cloud API)
3. **Test**: Orchestrator fails fast when llama.cpp is not available
   - **PASS**: Stop llama.cpp server, run `openspec-orch start test-e2e-orch` — exits with error code ≤ 2 within 60s and logs "llama.cpp not available" or similar
4. **Spec scenarios**: `openspec-orchestrator` — "all LLM requests go to local llama.cpp only"
   - **PASS**: spec requirement 4.1, 4.2

### Fail conditions
- Any request to non-localhost endpoint detected → FAIL
- `OPENAI_API_KEY` affects orchestrator behavior → FAIL
- Orchestrator hangs instead of failing fast when llama.cpp is down → FAIL

### Affected modules
- Existing: `orchestrate.py` (may need pre-flight check improvement)
- Network monitoring: external tools (socat, tcpdump) for verification

### Prerequisites
- Tasks 1–6 must complete: full orchestrator works with test change
- Network monitoring tools available (socat or tcpdump)

### Risks
- Network monitoring may require root/sudo privileges
- Test depends on llama.cpp server being available for the happy path
