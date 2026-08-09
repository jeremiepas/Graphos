# Do: Lazy trace-directory creation

## Changes Made
- `src/Graphos/Infrastructure/Observability/SDK.hs`
  - Removed `createDirectoryIfMissing` from `newDebugTraceEnv`.
  - Added it inside `flushDebugTrace` only when `dtEnabled` is true and the event buffer is non-empty.
  - Documented the "folder ⇔ file" invariant in the `flushDebugTrace` haddock.
- `tests/Graphos/Infrastructure/Observability/SDKSpec.hs`
  - Added three Hspec cases covering disabled, empty-enabled, and events-enabled scenarios.
- `graphos.cabal`
  - Listed `Graphos.Infrastructure.Observability.SDKSpec` under `other-modules`.
- `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs`
  - Removed unused `System.IO` import.
  - Replaced partial `head` calls with `Data.List.NonEmpty.head` to unblock `-Werror`.
