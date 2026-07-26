# Plan: ST-based local moving pass

## Goal
O(1) per node move instead of a full assignment-vector copy per move.

## Approach
Thaw the assignment once per pass (`runST`), mutate in place, freeze at end. Same visit order and ΔQ scoring; sigma-tot IntMap updates unchanged.

## Check Criteria
- Golden specs pass unchanged; full suite green; build clean; module stays IO-free.
