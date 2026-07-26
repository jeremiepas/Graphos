# Plan: Honest NFData for LeidenState

## Goal
Make `deepseq` between Leiden iterations actually force the state.

## Approach
Replace `rnf LeidenState{} = ()` with a field-forcing instance; add a deepseq smoke spec.

## Check Criteria
- Smoke spec passes; suite green; build clean.
