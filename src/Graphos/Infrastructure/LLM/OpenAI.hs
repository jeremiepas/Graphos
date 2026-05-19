-- | OpenAI-compatible LLM API client.
-- Uses curl (like Neo4j push) to call chat completion endpoints.
-- Supports OpenAI, Ollama, LiteLLM, and any OpenAI-compatible API.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.LLM.OpenAI
  ( callLLM
  , parseLabelsFromResponse
  , resolveEnvVars
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

import Graphos.Domain.Config (LabelingConfig(..))
import Graphos.Domain.Types (CommunityId)

-- | Call an OpenAI-compatible chat completion API via curl.
callLLM :: LabelingConfig -> Text -> IO (Either Text Text)
callLLM cfg prompt = catch (do
  let apiBase = labelingBaseUrl cfg
      model = labelingModel cfg
      apiKey = resolveEnvVars (labelingApiKey cfg)

  -- Build JSON payload  
  let systemMsg :: Aeson.Object
      systemMsg = KeyMap.fromList
        [ (AesonKey.fromText "role", Aeson.String "system")
        , (AesonKey.fromText "content", Aeson.String "You are a code architecture analyst. Respond only with valid JSON.")
        ]
      userMsg :: Aeson.Object
      userMsg = KeyMap.fromList
        [ (AesonKey.fromText "role", Aeson.String "user")
        , (AesonKey.fromText "content", Aeson.String prompt)
        ]
      payload = Aeson.encode $ Aeson.object
        [ "model" Aeson..= model
        , "messages" Aeson..= [Aeson.Object systemMsg, Aeson.Object userMsg]
        , "temperature" Aeson..= (0.3 :: Double)
        , "max_tokens" Aeson..= (500 :: Int)
        ]
      payloadPath = "/tmp/graphos-llm-payload.json"

  BSL8.writeFile payloadPath payload

  let authHeaders = if not (null apiKey) && labelingProvider cfg /= "ollama"
                      then ["-H", "Authorization: Bearer " ++ apiKey]
                      else []
      curlArgs = [ "-s", "--max-time", "60"
                  , "-X", "POST"
                  , "-H", "Content-Type: application/json"
                  ]
                  ++ authHeaders
                  ++ [ "--data-binary", "@" ++ payloadPath
                     , apiBase ++ "/chat/completions"
                     ]

  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl" curlArgs ""

  -- Cleanup temp file (best effort)
  _ <- catch (BSL8.readFile payloadPath >> return ()) (\(_ :: SomeException) -> pure ())

  case exitCode of
    ExitSuccess -> pure $ parseResponse (T.pack stdout)
    ExitFailure code -> pure $ Left $ T.pack $ "LLM API call failed (curl exit " ++ show code ++ "): " ++ take 200 stderr
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "LLM API call error: " ++ show e

-- | Parse the OpenAI chat completion response to extract the assistant message content.
parseResponse :: Text -> Either Text Text
parseResponse response =
  case Aeson.decode (BSL8.fromStrict (encodeUtf8 response)) of
    Just (Aeson.Object obj) ->
      case KeyMap.lookup "choices" obj of
        Just (Aeson.Array arr)
          | not (V.null arr) ->
            case V.toList arr of
              (Aeson.Object choice:_) ->
                case KeyMap.lookup "message" choice of
                  Just (Aeson.Object msg) ->
                    case KeyMap.lookup "content" msg of
                      Just (Aeson.String content) -> Right content
                      Just other -> Right $ T.pack $ show other
                      Nothing -> Left "No 'content' in message"
                  _ -> Left "No 'message' object in choice"
              _ -> Left "First choice is not an object"
          | otherwise -> Left "Empty choices array"
        _ -> Left $ "No 'choices' in response: " <> T.take 200 response
    _ -> Left $ "Failed to parse JSON response: " <> T.take 200 response

-- | Parse community labels from LLM response text.
-- Expects JSON like: {"483": "Export Module", "484": "Config Parsing"}
parseLabelsFromResponse :: Text -> Map CommunityId Text
parseLabelsFromResponse response =
  let clean = stripCodeBlocks response
  in case Aeson.decode (BSL8.fromStrict (encodeUtf8 clean)) of
       Just (Aeson.Object obj) -> Map.fromList
         [ (read (T.unpack (AesonKey.toText k)) :: CommunityId, v)
         | (k, Aeson.String v) <- KeyMap.toList obj
         ]
       _ -> Map.empty

-- | Strip markdown code blocks from LLM response.
stripCodeBlocks :: Text -> Text
stripCodeBlocks t =
  T.strip $ T.replace "```" "" $ T.replace "```json" "" t

-- | Resolve environment variable references like ${VAR} in a string.
-- Uses unsafePerformIO for env var lookup in pure context.
resolveEnvVars :: String -> String
resolveEnvVars s = go s
  where
    go [] = []
    go ('$':'{':rest) = let (varName, after) = break (== '}') rest
                        in unsafeGetEnv varName ++ go (drop 1 after)
    go (c:cs) = c : go cs

    unsafeGetEnv :: String -> String
    unsafeGetEnv var = unsafePerformIO (catch (getEnv var) (\(_ :: SomeException) -> pure []))
    {-# NOINLINE unsafeGetEnv #-}