-- | OpenAI Vision API client for image analysis.
-- Sends images as base64-encoded data URLs to multimodal LLMs
-- (qwen3.6-moe, gpt-4o, etc.) and parses structured responses.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LLM.Vision
  ( analyzeImage
  , ImageAnalysis(..)
  , Entity(..)
  , ImageKind(..)
  , encodeImageBase64
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.IO (withFile, IOMode(ReadMode), hFileSize)

import Graphos.Infrastructure.Http (runCurl, CurlResult(..), withSecureTempFile)
import Graphos.Domain.Config (VisionConfig(..), LabelingConfig(..))
import Graphos.Infrastructure.LLM.OpenAI (resolveEnvVars)

-- | Image kind classification from vision model
data ImageKind
  = Photo
  | Screenshot
  | Diagram
  | Resume
  | Chart
  | OtherKind
  deriving (Eq, Show)

instance Aeson.ToJSON ImageKind where
  toJSON Photo      = "photo"
  toJSON Screenshot = "screenshot"
  toJSON Diagram    = "diagram"
  toJSON Resume     = "resume"
  toJSON Chart      = "chart"
  toJSON OtherKind  = "other"

instance Aeson.FromJSON ImageKind where
  parseJSON = Aeson.withText "ImageKind" $ \t -> case T.toLower t of
    "photo"      -> pure Photo
    "screenshot" -> pure Screenshot
    "diagram"    -> pure Diagram
    "resume"     -> pure Resume
    "chart"      -> pure Chart
    _            -> pure OtherKind

-- | Structured entity extracted from an image
data Entity = Entity
  { entityLabel       :: Text
  , entityType        :: Text
  , entityConfidence  :: Double
  } deriving (Eq, Show)

instance Aeson.FromJSON Entity where
  parseJSON = Aeson.withObject "Entity" $ \v -> Entity
    <$> v Aeson..: "label"
    <*> v Aeson..: "type"
    <*> v Aeson..:? "confidence" Aeson..!= 0.5

instance Aeson.ToJSON Entity where
  toJSON e = Aeson.object
    [ "label"      Aeson..= entityLabel e
    , "type"       Aeson..= entityType e
    , "confidence" Aeson..= entityConfidence e
    ]

-- | Image analysis result from vision LLM
data ImageAnalysis = ImageAnalysis
  { iaDescription :: Text
  , iaEntities    :: [Entity]
  , iaKind         :: ImageKind
  } deriving (Eq, Show)

instance Aeson.FromJSON ImageAnalysis where
  parseJSON = Aeson.withObject "ImageAnalysis" $ \v -> ImageAnalysis
    <$> v Aeson..: "description"
    <*> v Aeson..:? "entities" Aeson..!= []
    <*> v Aeson..:? "kind" Aeson..!= OtherKind

instance Aeson.ToJSON ImageAnalysis where
  toJSON a = Aeson.object
    [ "description" Aeson..= iaDescription a
    , "entities"    Aeson..= iaEntities a
    , "kind"        Aeson..= iaKind a
    ]

-- | Encode an image file as base64 data URL.
-- Returns the MIME type and base64-encoded content.
encodeImageBase64 :: FilePath -> IO (Either Text (Text, Text))
encodeImageBase64 filePath = catch (do
  content <- BS.readFile filePath
  let mimeType = case takeExtension filePath of
        ".png"  -> "image/png"
        ".jpg"  -> "image/jpeg"
        ".jpeg" -> "image/jpeg"
        ".webp" -> "image/webp"
        ".gif"  -> "image/gif"
        _       -> "image/png"
      b64 = decodeUtf8 (B64.encode content)
      dataUrl = "data:" <> mimeType <> ";base64," <> b64
  pure (Right (mimeType, dataUrl))
  ) $ \(e :: SomeException) -> pure (Left $ T.pack $ "Error encoding image: " ++ show e)
  where
    takeExtension path = case dropWhile (/= '.') path of
      '.':ext -> '.':map toLower ext
      _       -> ""
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Analyze an image using a multimodal LLM.
-- Sends the image as a base64 data URL in the OpenAI Vision API format.
analyzeImage :: VisionConfig -> LabelingConfig -> FilePath -> IO (Either Text ImageAnalysis)
analyzeImage vCfg lCfg filePath = catch (do
  let fileSizeLimit = 15 * 1024 * 1024  -- 15MB original file size limit

  -- Check file size
  fileSize <- getFileSize filePath
  if fileSize > fileSizeLimit
    then pure (Left $ "Image too large (" <> T.pack (show fileSize) <> " bytes > " <> T.pack (show fileSizeLimit) <> " limit): " <> T.pack filePath)
    else do
      -- Encode image
      encResult <- encodeImageBase64 filePath
      case encResult of
        Left err -> pure (Left err)
        Right (_mimeType, dataUrl) -> do
          let apiBase = case vcBaseUrl vCfg of
                url | null url   -> labelingBaseUrl lCfg
                   | otherwise  -> url
              apiKey = let vk = vcApiKey vCfg
                       in if null vk || vk == "${OPENAI_API_KEY}"
                          then resolveEnvVars (labelingApiKey lCfg)
                          else resolveEnvVars vk
              model = vcModel vCfg
              maxTokens = vcMaxTokens vCfg

          -- Build Vision API payload
          let imageUrlObj = Aeson.object
                [ "url" Aeson..= dataUrl
                ]
              userContent = Aeson.object
                [ "type" Aeson..= ("image_url" :: Text)
                , "image_url" Aeson..= imageUrlObj
                ]
              systemMsg = Aeson.object
                [ "role" Aeson..= ("system" :: Text)
                , "content" Aeson..= systemPrompt
                ]
              userMsg = Aeson.object
                [ "role" Aeson..= ("user" :: Text)
                , "content" Aeson..= [userContent]
                ]
              payload = Aeson.encode $ Aeson.object
                [ "model" Aeson..= model
                , "messages" Aeson..= [systemMsg, userMsg]
                , "max_tokens" Aeson..= maxTokens
                , "temperature" Aeson..= (0.3 :: Double)
                ]
          withSecureTempFile "payload" payload $ \payloadPath -> do
            let authOpts = [("--header", "Authorization: Bearer " ++ apiKey) | not (null apiKey)]
                customHeaderOpts = Map.toList (vcHeaders vCfg) >>= \(k, v) -> [("--header", k ++ ": " ++ resolveEnvVars v)]
                secretOpts = authOpts ++ customHeaderOpts
                firstArgs = [ "-s", "--max-time", "120", "-X", "POST", "-H", "Content-Type: application/json" ]
                lastArgs = [ "--data-binary", "@" ++ payloadPath, apiBase ++ "/chat/completions" ]
            res <- runCurl firstArgs secretOpts lastArgs
            case res of
              CurlSuccess stdout -> pure $ parseVisionResponse stdout
              CurlFailed n err   -> pure $ Left (T.pack ("Vision API call failed (curl exit " ++ show n ++ "): " ++ T.unpack (T.take 200 err)))
  ) $ \(e :: SomeException) -> pure (Left $ T.pack $ "Vision API call error: " ++ show e)
  where
    systemPrompt :: Text
    systemPrompt = "You are an image analysis assistant. Analyze the image and respond with a JSON object containing: \"description\" (a detailed description), \"entities\" (an array of {\"label\": string, \"type\": string, \"confidence\": number} for each notable entity), and \"kind\" (one of: photo, screenshot, diagram, resume, chart, other). Respond ONLY with valid JSON."

-- | Parse the Vision API response into an ImageAnalysis.
parseVisionResponse :: Text -> Either Text ImageAnalysis
parseVisionResponse response =
  case Aeson.decode (BSL8.fromStrict (encodeUtf8 response)) of
    Just (Aeson.Object obj) ->
      let choices = case KeyMap.lookup "choices" obj of
            Just (Aeson.Array arr) -> V.toList arr
            _ -> []
      in case choices of
           (Aeson.Object choice:_) ->
             case KeyMap.lookup "message" choice of
               Just (Aeson.Object msg) ->
                 case KeyMap.lookup "content" msg of
                   Just (Aeson.String content) -> parseAnalysisContent content
                   Just other -> Left $ "Unexpected content type: " <> T.pack (show other)
                   Nothing -> Left "No 'content' in message"
               _ -> Left "No 'message' object in choice"
           _ -> Left $ "No valid choices in response: " <> T.take 500 response
    _ -> Left $ "Failed to parse JSON response: " <> T.take 500 response

-- | Parse the content of a vision response into ImageAnalysis.
-- The LLM may wrap JSON in markdown code blocks.
parseAnalysisContent :: Text -> Either Text ImageAnalysis
parseAnalysisContent content =
  let cleaned = stripCodeBlocks content
  in case Aeson.decode (BSL8.fromStrict (encodeUtf8 cleaned)) of
       Just analysis -> Right analysis
       Nothing -> Left $ "Failed to parse ImageAnalysis JSON: " <> T.take 500 cleaned

-- | Strip markdown code blocks from LLM response.
stripCodeBlocks :: Text -> Text
stripCodeBlocks t = T.strip $ T.replace "```json" "" $ T.replace "```" "" t

-- | Get file size in bytes.
getFileSize :: FilePath -> IO Int
getFileSize path = catch (do
  size <- withFile path ReadMode hFileSize
  pure (fromIntegral size :: Int)
  ) $ \(_ :: SomeException) -> pure 0