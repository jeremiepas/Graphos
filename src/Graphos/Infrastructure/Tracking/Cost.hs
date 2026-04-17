-- | Token cost tracking - cumulative cost tracker across runs
module Graphos.Infrastructure.Tracking.Cost
  ( updateCostTracker
  , CostTracker(..)
  , CostRun(..)
  , printCostSummary
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- | A single run's cost entry
data CostRun = CostRun
  { crDate        :: Text
  , crInputTokens  :: Int
  , crOutputTokens :: Int
  , crFiles       :: Int
  } deriving (Eq, Show)

instance ToJSON CostRun where
  toJSON r = object
    [ "date"          .= crDate r
    , "input_tokens"  .= crInputTokens r
    , "output_tokens" .= crOutputTokens r
    , "files"         .= crFiles r
    ]

instance FromJSON CostRun where
  parseJSON = withObject "CostRun" $ \v -> CostRun
    <$> v .: "date"
    <*> v .: "input_tokens"
    <*> v .: "output_tokens"
    <*> v .: "files"

-- | Cumulative cost tracker
data CostTracker = CostTracker
  { ctRuns              :: [CostRun]
  , ctTotalInputTokens  :: Int
  , ctTotalOutputTokens :: Int
  } deriving (Eq, Show)

instance ToJSON CostTracker where
  toJSON t = object
    [ "runs"                .= ctRuns t
    , "total_input_tokens"  .= ctTotalInputTokens t
    , "total_output_tokens" .= ctTotalOutputTokens t
    ]

instance FromJSON CostTracker where
  parseJSON = withObject "CostTracker" $ \v -> CostTracker
    <$> v .: "runs"
    <*> v .: "total_input_tokens"
    <*> v .: "total_output_tokens"

-- | Update the cost tracker with a new run
updateCostTracker :: FilePath -> Int -> Int -> Int -> IO ()
updateCostTracker outputDir inputTok outputTok fileCount = do
  now <- getCurrentTime
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
      newRun = CostRun timestamp inputTok outputTok fileCount
      costPath = outputDir ++ "/cost.json"

  createDirectoryIfMissing True outputDir

  existing <- doesFileExist costPath
  tracker <- if not existing
    then pure CostTracker { ctRuns = [], ctTotalInputTokens = 0, ctTotalOutputTokens = 0 }
    else do
      bs <- BSL.readFile costPath
      case eitherDecode bs of
        Left _  -> pure CostTracker { ctRuns = [], ctTotalInputTokens = 0, ctTotalOutputTokens = 0 }
        Right t -> pure t

  let updated = tracker
        { ctRuns              = ctRuns tracker ++ [newRun]
        , ctTotalInputTokens  = ctTotalInputTokens tracker + inputTok
        , ctTotalOutputTokens = ctTotalOutputTokens tracker + outputTok
        }

  BSL.writeFile costPath (encode updated)
  printCostSummary newRun updated

-- | Print cost summary to stdout
printCostSummary :: CostRun -> CostTracker -> IO ()
printCostSummary run tracker = do
  putStrLn $ "This run: " ++ show (crInputTokens run) ++ " input tokens, " ++ show (crOutputTokens run) ++ " output tokens"
  putStrLn $ "All time: " ++ show (ctTotalInputTokens tracker) ++ " input, " ++ show (ctTotalOutputTokens tracker) ++ " output (" ++ show (length (ctRuns tracker)) ++ " runs)"