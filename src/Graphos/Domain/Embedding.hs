-- | Pure embedding input preparation (change: lfm-embedding-optimization,
-- design D1/D3/D8).
--
-- Before any text goes on the wire it is /prepared/: the configured document
-- prefix is prepended and the text is truncated to fit the effective token
-- limit. This is a pure Domain function — the Infrastructure client calls it
-- just before building the JSON payload, and the UseCase pipelines stay
-- preparation-agnostic (they dedup over raw texts; prepared-text convergence
-- happens beneath, via the cache keys over the prepared text).
module Graphos.Domain.Embedding
  ( prepare
  , effectiveTokenLimit
  , estimateTokens
  , modelTokenLimit
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Config (EmbeddingConfig(..))

-- | Static model-limit table (design D3): documented context windows for the
-- models Graphos is known to target. An unknown model falls back to
-- 'embMaxTokens' (or no truncation when that is 0 too). Matching is a
-- substring check so quantization suffixes (@:Q4_K_M@) and repo paths don't
-- defeat the table.
modelTokenLimit :: String -> Maybe Int
modelTokenLimit model
  | "nomic-embed" `T.isInfixOf` t = Just 8192
  | "LFM2.5"      `T.isInfixOf` t = Just 512
  | "all-minilm"  `T.isInfixOf` t = Just 256
  | otherwise                     = Nothing
  where
    t = T.pack model

-- | The effective token limit for a configuration: an explicit @maxTokens@
-- override wins over the model table; @0@ means "model default" (falling
-- back to the static table, or no limit for unknown models).
effectiveTokenLimit :: EmbeddingConfig -> Int
effectiveTokenLimit cfg
  | embMaxTokens cfg > 0 = embMaxTokens cfg
  | otherwise = maybe 0 id (modelTokenLimit (embModel cfg))

-- | Conservative token estimate (design D3): characters ÷ 4 — the
-- English/code heuristic ≈ 4 chars/token. This is a budget guard, not a
-- real subword tokenizer; residual overflow is contained per-input by the
-- transport's failure isolation.
estimateTokens :: Text -> Int
estimateTokens t = (T.length t + 3) `div` 4

-- | Prepare one embed input for the API: prepend the configured document
-- prefix, then truncate (conservative char÷4 estimate) so the prepared text
-- MUST NOT exceed 'effectiveTokenLimit' tokens.
--
-- @prepare cfg t == t@ when the prefix is empty and @t@ fits under the
-- effective limit (the passthrough contract: normal texts are byte-identical).
prepare :: EmbeddingConfig -> Text -> Text
prepare cfg t0 =
  let limit = effectiveTokenLimit cfg
      t = embDocPrefix cfg <> t0
  in if limit <= 0 || estimateTokens t <= limit
       then t
       else truncateToCharBudget (4 * limit) t

-- | Keep the leading characters within the byte/char budget. The prefix is
-- never cut: truncation eats into the raw text only (the informative head —
-- the label — stays).
truncateToCharBudget :: Int -> Text -> Text
truncateToCharBudget budget t = T.take (max 0 (budget - 0)) t