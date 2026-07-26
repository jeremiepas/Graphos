# Do: ST-based local moving pass

- `localMovingPass` now runs in `runST`: safe `VU.thaw` once, `VUM.unsafeRead`/`unsafeWrite` per node, `unsafeFreeze` at end.
- New `localMovingLoop :: forall s. VUM.MVector s Int -> LeidenState -> ST s (Int, IntMap Double)` carries the pass.
- ΔQ scoring extracted to pure `bestCommunityFor` (takes m, gamma, sigma-tot, precomputed neighbor communities).
- Removed superseded `findBestCommunity` and `moveNode`.
- Added `ScopedTypeVariables`; imports: `Control.Monad.ST`, `Data.Vector.Unboxed.Mutable`.
