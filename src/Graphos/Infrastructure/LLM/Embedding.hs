-- | Ollama embedding client — calls local Ollama /embeddings endpoint.
-- Reuses the same OpenAI-compatible pattern as Infra/LLM/OpenAI.hs.
-- Only used when --embed flag is passed; no API key required for local Ollama.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LLM.Embedding
  ( generateEmbedding
  , cosineSimilarity
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Graphos.Domain.Config (EmbeddingConfig(..))

-- | Generate an embedding vector for a text input using Ollama.
-- Calls the OpenAI-compatible /embeddings endpoint.
-- Returns Left on error, Right with the embedding vector on success.
generateEmbedding :: EmbeddingConfig -> Text -> IO (Either Text [Double])
generateEmbedding cfg inputText = catch (do
  let apiBase = embBaseUrl cfg
      model = embModel cfg

  let payload = Aeson.encode $ Aeson.object
        [ "model" Aeson..= model
        , "input" Aeson..= inputText
        ]

      payloadPath = "/tmp/graphos-embed-payload.json"

  BSL8.writeFile payloadPath payload

  let curlArgs = [ "-s", "--max-time", "30"
                 , "-X", "POST"
                 , "-H", "Content-Type: application/json"
                 , "--data-binary", "@" ++ payloadPath
                 , apiBase ++ "/embeddings"
                 ]

  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl" curlArgs ""

  case exitCode of
    ExitSuccess -> pure $ parseEmbeddingResponse (T.pack stdout)
    ExitFailure code -> pure $ Left $ T.pack $ "Embedding API call failed (curl exit " ++ show code ++ "): " ++ take 200 stderr
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Embedding API error: " ++ show e

-- | Parse the OpenAI-compatible embeddings response.
-- Expects: {"data": [{"embedding": [0.1, 0.2, ...]}]}
parseEmbeddingResponse :: Text -> Either Text [Double]
parseEmbeddingResponse response =
  case Aeson.decode (BSL8.fromStrict (encodeUtf8 response)) of
    Just (Aeson.Object obj) ->
      case KeyMap.lookup "data" obj of
        Just (Aeson.Array arr)
          | not (V.null arr) ->
            case V.toList arr of
              (Aeson.Object item: _) ->
                case KeyMap.lookup "embedding" item of
                  Just (Aeson.Array vec) ->
                    Right [ d | Aeson.Number n <- V.toList vec
                              , let Just d = Just (realToFrac n :: Double)
                      ]
                  _ -> Left "No 'embedding' array in data item"
              _ -> Left "First data item is not an object"
          | otherwise -> Left "Empty data array"
        _ -> Left $ "No 'data' in response: " <> T.take 200 response
    _ -> Left $ "Failed to parse embedding JSON: " <> T.take 200 response

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