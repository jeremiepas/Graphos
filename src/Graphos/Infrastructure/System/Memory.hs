-- | System memory reader (fix-oom-memory-budget-guard).
-- The only IO of the memory-budget guard: read @MemAvailable@ from
-- @/proc/meminfo@ (Linux). Every failure mode — missing file, unreadable
-- file, format drift — degrades to 'Nothing', which the policy layer treats
-- as "no derived budget" (uncapped run + warning, today's behavior).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.System.Memory
  ( readMemInfo
  , parseMemInfo
  ) where

import Control.Exception (SomeException, try, evaluate)
import Control.DeepSeq (force)

import Graphos.Domain.Config.Memory (MemInfo(..))

-- | Read @MemAvailable@ from @/proc/meminfo@. 'Nothing' on any read or parse
-- failure (notably on platforms without procfs).
readMemInfo :: IO (Maybe MemInfo)
readMemInfo = do
  r <- try (readFile "/proc/meminfo" >>= evaluate . force)
  pure $ case r of
    Left (_ :: SomeException) -> Nothing
    Right content             -> parseMemInfo content

-- | Parse @/proc/meminfo@ text: find the @MemAvailable:@ line (kB units,
-- stable since Linux 3.14) and convert to bytes. Unknown lines are ignored;
-- a missing or malformed @MemAvailable@ yields 'Nothing'.
parseMemInfo :: String -> Maybe MemInfo
parseMemInfo content =
  case [ rest | line <- lines content
              , ("MemAvailable:", rest) <- [splitAt 13 line] ] of
    (rest : _) -> case words rest of
      (num : _) -> case reads num :: [(Integer, String)] of
        [(kb, "")] | kb >= 0 -> Just (MemInfo (kb * 1024))
        _                    -> Nothing
      [] -> Nothing
    [] -> Nothing
