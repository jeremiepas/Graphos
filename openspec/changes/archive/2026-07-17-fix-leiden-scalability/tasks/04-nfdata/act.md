# Act: Honest NFData for LeidenState

- Done. Convention: never ship `rnf _ = ()` instances — they silently disable deepseq call sites.
