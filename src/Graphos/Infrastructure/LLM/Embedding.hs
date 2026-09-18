-- | Ollama embedding client — calls local Ollama /embeddings endpoint.
-- Native http-client transport over a shared connection manager (keep-alive):
-- no external process is forked and no payload is written to a temp file.
-- Only used when --embed flag is passed; no API key required for local Ollama.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LLM.Embedding
  ( generateEmbedding
  , generateEmbeddings
  , embeddingManager
  , parseEmbeddingsResponse
  , cosineSimilarity
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Ord (comparing)
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody(..)
  , defaultManagerSettings
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import System.IO.Unsafe (unsafePerformIO)

import Graphos.Domain.Config (EmbeddingConfig(..))
import Graphos.Infrastructure.LLM.OpenAI (resolveEnvVars)

-- | Shared connection manager (design D9): created lazily once per process
-- and reused by every embedding call, so connections are kept alive across
-- calls instead of one TCP handshake per request.
embeddingManager :: Manager
embeddingManager = unsafePerformIO (newManager defaultManagerSettings)
{-# NOINLINE embeddingManager #-}

-- | Per-request timeout in microseconds: the historical 30s one-text budget
-- scaled by the number of texts in the request (one text ⇒ 30s, as before).
requestTimeoutMicros :: Int -> Int
requestTimeoutMicros nTexts = 30 * 1000 * 1000 * max 1 nTexts

-- | Build the POST request to the @/embeddings@ endpoint.
-- Custom headers from 'embHeaders' are applied to every request; references
-- like @$VAR@ in header values are resolved against the environment.
embeddingsRequest :: EmbeddingConfig -> Int -> BSL.ByteString -> IO Request
embeddingsRequest cfg nTexts body = do
  base <- parseRequest (embBaseUrl cfg ++ "/embeddings")
  let customHeaders =
        [ (fromString k :: HeaderName, encodeUtf8 (T.pack (resolveEnvVars v)))
        | (k, v) <- Map.toList (embHeaders cfg)
        ]
  pure base
    { method          = "POST"
    , requestHeaders  = ("Content-Type", "application/json") : customHeaders
    , requestBody     = RequestBodyLBS body
    , responseTimeout = responseTimeoutMicro (requestTimeoutMicros nTexts)
    }

-- | POST a JSON payload to the embeddings endpoint.
-- Returns the decoded JSON body on 2xx, otherwise a 'Left' error carrying
-- the HTTP status or transport failure.
postEmbeddings :: EmbeddingConfig -> Int -> Aeson.Value -> IO (Either Text Aeson.Value)
postEmbeddings cfg nTexts payload = catch (do
  req <- embeddingsRequest cfg nTexts (Aeson.encode payload)
  res <- httpLbs req embeddingManager
  let st   = responseStatus res
      body = responseBody res
  if statusCode st >= 200 && statusCode st < 300
    then pure $ case Aeson.eitherDecode body of
           Left err -> Left $ T.pack $
             "Failed to parse embedding JSON: " ++ err ++ ": " ++ take 200 (BSL8.unpack body)
           Right v  -> Right v
    else pure $ Left $ T.pack $
      "Embedding API call failed (HTTP " ++ show (statusCode st) ++ "): " ++ take 200 (BSL8.unpack body)
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Embedding API error: " ++ show e

-- | Generate an embedding vector for a single text input using Ollama.
-- Equivalent to a batch of one ('generateEmbeddings' with a singleton list,
-- projected to its only vector); kept for tests and external callers.
generateEmbedding :: EmbeddingConfig -> Text -> IO (Either Text [Double])
generateEmbedding cfg inputText = do
  r <- generateEmbeddings cfg [inputText]
  pure $ case r of
    Left err  -> Left err
    Right vec -> case vec of
      [v] -> Right v
      _   -> Left $ T.pack ("Embedding API returned " ++ show (length vec) ++ " vectors for 1 input")

-- | Generate embedding vectors for a list of texts in one API call.
-- Payload: @{"model": …, "input": [t1, …, tn]}@ (OpenAI-compatible batch
-- endpoint); returns one vector per input text, in input order.
generateEmbeddings :: EmbeddingConfig -> [Text] -> IO (Either Text [[Double]])
generateEmbeddings cfg inputTexts = do
  r <- postEmbeddings cfg (length inputTexts) $ Aeson.object
        [ "model" Aeson..= embModel cfg
        , "input" Aeson..= inputTexts
        ]
  pure $ case r of
    Left err -> Left err
    Right v  -> parseEmbeddingsResponse (length inputTexts) v

-- | Parse an OpenAI-compatible batch embeddings response, validating the
-- OpenAI @index@ contract before trusting element order:
--
--   * every item carries an integer @index@;
--   * the indices form a permutation of @[0 .. n-1]@;
--   * @data@ has exactly @n@ items (@n@ = number of submitted texts).
--
-- Items are sorted by @index@ so the returned vectors are in input order
-- regardless of the order the server chose to send them.
parseEmbeddingsResponse :: Int -> Aeson.Value -> Either Text [[Double]]
parseEmbeddingsResponse n v = do
  obj     <- asObject v "response"
  dataVal <- maybe (Left (missingDataMsg v)) Right (KeyMap.lookup "data" obj)
  items   <- asArray dataVal "data"
  let itemList = V.toList items
  if length itemList /= n
    then Left (arityMsg (length itemList))
    else do
      indexed <- mapM parseEmbeddingItem itemList
      let sorted = sortByIndex indexed
      if map fst sorted /= [0 .. n - 1]
        then Left "Embedding response 'data' indices are not a permutation of [0..n-1]"
        else Right (map snd sorted)
  where
    missingDataMsg val = T.pack ("No 'data' in response: " ++ take 200 (show val))
    arityMsg got = T.pack
      ("Embedding response arity mismatch: expected " ++ show n ++ " items, got " ++ show got)
    sortByIndex = sortBy (comparing fst)

-- | Parse one @data@ item into its @(index, vector)@ pair.
parseEmbeddingItem :: Aeson.Value -> Either Text (Int, [Double])
parseEmbeddingItem item = do
  iobj   <- asObject item "data item"
  idxVal <- maybe (Left "Missing 'index' in embedding data item") Right
                  (KeyMap.lookup "index" iobj)
  idx    <- indexToInt idxVal
  embVal <- maybe (Left "No 'embedding' array in data item") Right
                  (KeyMap.lookup "embedding" iobj)
  vec    <- asArray embVal "embedding"
  ds     <- mapM numToDouble (V.toList vec)
  Right (idx, ds)

-- | Extract the integer @index@ field from a JSON number.
indexToInt :: Aeson.Value -> Either Text Int
indexToInt (Aeson.Number x) = Right (round x)
indexToInt other = Left (T.pack ("Non-integer 'index' in embedding data item: " ++ show other))

numToDouble :: Aeson.Value -> Either Text Double
numToDouble (Aeson.Number x) = Right (realToFrac x)
numToDouble other = Left (T.pack ("Non-numeric value in embedding array: " ++ show other))

asObject :: Aeson.Value -> String -> Either Text Aeson.Object
asObject (Aeson.Object o) _    = Right o
asObject other what = Left (T.pack ("Embedding " ++ what ++ " is not a JSON object: " ++ show other))

asArray :: Aeson.Value -> String -> Either Text (V.Vector Aeson.Value)
asArray (Aeson.Array a) _      = Right a
asArray other what = Left (T.pack ("Embedding " ++ what ++ " is not a JSON array: " ++ show other))

-- | Compute cosine similarity between two embedding vectors.
-- Returns 0.0 if vectors have different lengths or are empty.
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity a b
  | length a /= length b = 0.0
  | null a = 0.0
  | otherwise =
      let dot = sum (zipWith (*) a b)
          normA = sqrt (sum (map (\x -> x * x) a))
          normB = sqrt (sum (map (\x -> x * x) b))
      in if normA == 0.0 || normB == 0.0
         then 0.0
         else dot / (normA * normB)