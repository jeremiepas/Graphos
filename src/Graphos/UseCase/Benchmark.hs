-- | Benchmark - token reduction comparison (corpus vs subgraph)
module Graphos.UseCase.Benchmark
  ( runBenchmark
  , BenchmarkResult(..)
  , printBenchmark
  ) where

-- | Benchmark result
data BenchmarkResult = BenchmarkResult
  { brCorpusWords    :: Int
  , brSubgraphTokens :: Int
  , brSavingsPercent :: Double
  } deriving (Eq, Show)

-- | Run a benchmark comparing corpus size to typical subgraph size
runBenchmark :: Int    -- ^ Corpus word count
             -> Int    -- ^ Typical subgraph token estimate
             -> BenchmarkResult
runBenchmark corpusWords subgraphTokens =
  let savings = if corpusWords > 0
                then 100.0 * (1.0 - fromIntegral subgraphTokens / (fromIntegral corpusWords * 4.0 / 1000.0))
                else 0.0
  in BenchmarkResult
    { brCorpusWords    = corpusWords
    , brSubgraphTokens = subgraphTokens
    , brSavingsPercent = max 0 savings
    }

-- | Print benchmark results
printBenchmark :: BenchmarkResult -> IO ()
printBenchmark result = do
  putStrLn "\n=== Token Reduction Benchmark ==="
  putStrLn $ "Full corpus:     " ++ show (brCorpusWords result) ++ " words (~" ++ show (brCorpusWords result `div` 250) ++ " tokens)"
  putStrLn $ "Typical subgraph: ~" ++ show (brSubgraphTokens result) ++ " tokens"
  putStrLn $ "Token savings:    " ++ show (round (brSavingsPercent result) :: Int) ++ "%"