-- | Memory budget policy for the pipeline (fix-oom-memory-budget-guard).
-- Pure data types and arithmetic — no IO. Reading @/proc/meminfo@ lives in
-- 'Graphos.Infrastructure.System.Memory'; enforcement (re-exec with RTS
-- @-M@, pre-flight checks, stage guards) lives in @app/Main.hs@ and
-- @UseCase.Pipeline@.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.Config.Memory
  ( -- * Types
    Bytes
  , MemInfo(..)
  , Verdict(..)
  , MemoryBudgetSetting(..)
  , MemoryConfig(..)
  , defaultMemoryConfig

    -- * Policy constants
  , memorySafetyReserve
  , memoryBudgetFloor
  , memoryBudgetCeiling

    -- * Policy
  , deriveBudget
  , preFlightVerdict
  , projectedEmbeddingBytes

    -- * Parsing / rendering
  , parseByteSize
  , formatBytes
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withObject, (.:?))
import qualified Data.Aeson.Types
import Data.Char (toUpper)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | A byte count. Integer, not Int: budgets exceed 2^31 on any modern machine.
type Bytes = Integer

-- | Snapshot of the machine's available memory (from @MemAvailable@ in
-- @/proc/meminfo@ on Linux).
newtype MemInfo = MemInfo
  { miMemAvailable :: Bytes  -- ^ Estimated memory available for new work, in bytes
  } deriving (Eq, Show, Generic)

-- | Pre-flight comparison of available memory against the active budget.
data Verdict
  = Ok     -- ^ Comfortable headroom: available >= budget + safety reserve
  | Warn   -- ^ Thin headroom: budget fits, but with less than the safety reserve to spare
  | Abort  -- ^ Available memory is below the budget: the run cannot honor it
  deriving (Eq, Show)

-- | The @memory.budget@ graphos.yaml setting.
data MemoryBudgetSetting
  = BudgetAuto         -- ^ Derive from available memory (default)
  | BudgetOff          -- ^ Run uncapped (equivalent to --no-memory-budget)
  | BudgetFixed Bytes  -- ^ Fixed heap budget, e.g. "8G" / "512M" / plain MB
  deriving (Eq, Show)

-- | The @memory:@ section of graphos.yaml.
newtype MemoryConfig = MemoryConfig
  { memBudget :: MemoryBudgetSetting
  } deriving (Eq, Show)

defaultMemoryConfig :: MemoryConfig
defaultMemoryConfig = MemoryConfig { memBudget = BudgetAuto }

instance FromJSON MemoryConfig where
  parseJSON = withObject "MemoryConfig" $ \v ->
    MemoryConfig . maybe BudgetAuto id <$> v .:? "budget"

instance FromJSON MemoryBudgetSetting where
  parseJSON (String t) = case t of
    "auto" -> pure BudgetAuto
    "off"  -> pure BudgetOff
    other  -> case parseByteSize (T.unpack other) of
      Just b  -> pure (BudgetFixed b)
      Nothing -> fail $ "memory.budget: expected auto, off, or a size (e.g. 8G, 512M): "
                          ++ T.unpack other
  -- Bare numbers are MB, matching --max-heap.
  parseJSON v@(Number _) = do
    mb <- parseJSON v :: Data.Aeson.Types.Parser Int
    if mb > 0
      then pure (BudgetFixed (fromIntegral mb * 1024 * 1024))
      else fail "memory.budget: numeric value must be a positive whole number of MB"
  parseJSON _ = fail "memory.budget: expected auto, off, or a size"

instance ToJSON MemoryConfig where
  toJSON (MemoryConfig b) = toJSON (renderSetting b)
    where
      renderSetting BudgetAuto      = "auto" :: T.Text
      renderSetting BudgetOff       = "off"
      renderSetting (BudgetFixed n) = T.pack (show n)

-- | Memory kept out of the budget for the rest of the system (desktop, other
-- processes, page cache churn) when deriving a default.
memorySafetyReserve :: Bytes
memorySafetyReserve = 4 * 1024 * 1024 * 1024  -- 4 GB

-- | A derived budget is never smaller than this (a run below it would fail
-- immediately on any real corpus; better to warn at pre-flight instead).
memoryBudgetFloor :: Bytes
memoryBudgetFloor = 512 * 1024 * 1024  -- 512 MB

-- | A derived budget is never larger than this (beyond it a runaway run
-- stresses the whole machine long before the cap helps).
memoryBudgetCeiling :: Bytes
memoryBudgetCeiling = 32 * 1024 * 1024 * 1024  -- 32 GB

-- | Derive the active heap budget.
--
-- An explicit budget (CLI @--max-heap@ or a fixed graphos.yaml value) always
-- wins, unclamped. Otherwise the default is @available − reserve@, clamped to
-- ['memoryBudgetFloor', 'memoryBudgetCeiling']. Without memory info (and no
-- explicit value) there is no budget — the caller runs uncapped and warns.
deriveBudget :: Maybe MemInfo -> Maybe Bytes -> Bytes -> Maybe Bytes
deriveBudget _ (Just explicitBudget) _ = Just explicitBudget
deriveBudget Nothing Nothing _ = Nothing
deriveBudget (Just mi) Nothing reserve =
  Just (clamp (miMemAvailable mi - reserve))
  where
    clamp = max memoryBudgetFloor . min memoryBudgetCeiling

-- | Compare available memory against the active budget before any stage runs.
-- Without memory info there is nothing to compare: 'Ok'.
preFlightVerdict :: Bytes -> Maybe MemInfo -> Verdict
preFlightVerdict _ Nothing = Ok
preFlightVerdict budget (Just mi)
  | available < budget                        = Abort
  | available < budget + memorySafetyReserve  = Warn
  | otherwise                                 = Ok
  where available = miMemAvailable mi

-- | Unboxed-floor projection of an embedding assignment:
-- @nodes × dims × 8 B@ (Double). The boxed in-memory reality is larger; this
-- is a floor guard — the RTS @-M@ cap remains the hard stop.
projectedEmbeddingBytes :: Int -> Int -> Bytes
projectedEmbeddingBytes nodeCount dims =
  fromIntegral nodeCount * fromIntegral dims * 8

-- | Parse a human byte size: @8G@ / @512M@ (case-insensitive, decimals
-- allowed) or a plain number of MB (matching @--max-heap@).
parseByteSize :: String -> Maybe Bytes
parseByteSize s = case span (`notElem` ("GgMm" :: String)) s of
  (num, [suffix]) -> scale (toUpper suffix) <$> readDouble num
  (num, [])       -> mbToBytes <$> readDouble num
  _               -> Nothing
  where
    readDouble str = case reads str :: [(Double, String)] of
      [(d, "")] | d > 0 -> Just d
      _                 -> Nothing
    scale 'G' d = round (d * 1024 * 1024 * 1024)
    scale _   d = round (d * 1024 * 1024)
    mbToBytes d = round (d * 1024 * 1024)

-- | Render a byte count for log lines: exact bytes with a GiB/MiB gloss.
formatBytes :: Bytes -> String
formatBytes b
  | b >= gib  = render (fromIntegral b / fromIntegral gib) "GiB"
  | b >= mib  = render (fromIntegral b / fromIntegral mib) "MiB"
  | otherwise = show b ++ " B"
  where
    gib, mib :: Bytes
    gib = 1024 * 1024 * 1024
    mib = 1024 * 1024
    render :: Double -> String -> String
    render v unit = show (fromIntegral (round (v * 10) :: Integer) / 10 :: Double) ++ " " ++ unit
