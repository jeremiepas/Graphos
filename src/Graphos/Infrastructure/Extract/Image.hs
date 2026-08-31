-- | Image file extraction - converts ImageAnalysis to graph nodes.
-- Creates an ImageFile node with description/kind/entities metadata,
-- plus typed entity nodes linked via Contains edges.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Graphos.Infrastructure.Extract.Image
  ( extractImageFile
  , extractImageFromBytes
  , imageAnalysisToExtraction
  , imageStubNode
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.FilePath (takeFileName)

import Graphos.Domain.Types
import Graphos.Infrastructure.LLM.Vision
  ( analyzeImage
  , ImageAnalysis(..)
  , Entity(..)
  , ImageKind(..)
  )
import Graphos.Infrastructure.Logging (LogEnv, logInfo, logDebug, logWarn)

-- | Extract nodes from an image file using vision LLM analysis.
-- Creates an ImageFile node with metadata, plus entity nodes linked
-- via Contains edges. Returns a stub node if vision is disabled or fails.
extractImageFile :: PipelineConfig -> LogEnv -> FilePath -> IO Extraction
extractImageFile config env filePath = do
  let vCfg = gcVision (cfgGraphosConfig config)
      lCfg = gcLabeling (cfgGraphosConfig config)
  if not (vcEnabled vCfg)
    then do
      logDebug env $ T.pack $ "  [vision] Skipping (disabled): " ++ filePath
      pure (extractionFromLists [imageStubNode filePath] [])
    else do
      result <- analyzeImage vCfg lCfg filePath
      case result of
        Left err -> do
          logWarn env $ T.pack $ "  [vision] Error analyzing " ++ filePath ++ ": " ++ T.unpack err
          pure (extractionFromLists [imageStubNode filePath] [])
        Right analysis -> do
          logInfo env $ T.pack $ "  [vision] " ++ filePath ++ " → " ++ show (length (iaEntities analysis)) ++ " entities"
          pure (imageAnalysisToExtraction filePath analysis)

-- | Extract nodes from an image provided as ByteString (for embedded images).
extractImageFromBytes :: PipelineConfig -> LogEnv -> FilePath -> BS.ByteString -> IO Extraction
extractImageFromBytes config env filePath _bytes = do
  let vCfg = gcVision (cfgGraphosConfig config)
  if not (vcEnabled vCfg)
    then pure (extractionFromLists [imageStubNode filePath] [])
    else do
      -- For embedded images, write to temp file then analyze
      -- (the vision API requires a file path for base64 encoding)
      -- For now, return a stub - full implementation requires temp file management
      logDebug env $ T.pack $ "  [vision] Embedded image (stub): " ++ filePath
      pure (extractionFromLists [imageStubNode filePath] [])

-- | Convert an ImageAnalysis to an Extraction with nodes and edges.
imageAnalysisToExtraction :: FilePath -> ImageAnalysis -> Extraction
imageAnalysisToExtraction filePath analysis =
  let imageNode = makeImageNode filePath analysis
      entityNodes = map (makeEntityNode filePath) (iaEntities analysis)
      entityEdges = map (makeContainsEdge filePath) entityNodes
  in extractionFromLists (imageNode : entityNodes) entityEdges

-- | Create an ImageFile node from analysis results.
makeImageNode :: FilePath -> ImageAnalysis -> Node
makeImageNode filePath analysis = Node
  { nodeId = T.pack filePath
  , nodeLabel = fromText (T.pack (takeFileName filePath))
  , nodeFileType = ImageFile
  , nodeSourceFile = fromText (T.pack filePath)
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText "Image")
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Just $ Aeson.object
      [ "description" Aeson..= iaDescription analysis
      , "kind" Aeson..= showKind (iaKind analysis)
      , "entities" Aeson..= map entityToJSON (iaEntities analysis)
      ]
  , nodePresentBits = bitNodeKind
  }

-- | Create an entity node from a vision-extracted entity.
makeEntityNode :: FilePath -> Entity -> Node
makeEntityNode filePath entity = Node
  { nodeId = T.pack (filePath ++ "#" ++ T.unpack (entityLabel entity))
  , nodeLabel = fromText (entityLabel entity)
  , nodeFileType = ImageFile
  , nodeSourceFile = fromText (T.pack filePath)
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText (entityType entity))
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Just $ Aeson.object
      [ "confidence" Aeson..= entityConfidence entity
      ]
  , nodePresentBits = bitNodeKind
  }

-- | Create a Contains edge from image node to entity node.
makeContainsEdge :: FilePath -> Node -> Edge
makeContainsEdge filePath entityNode = Edge
  { edgeId = EdgeId (T.pack (filePath ++ "->" ++ T.unpack (nodeId entityNode)))
  , edgeSource = T.pack filePath
  , edgeTarget = nodeId entityNode
  , edgeRelation = Contains
  , edgeWeight = 0.8
  , edgeConfidence = Confidence 0.8
  , edgeExtra = Nothing
  }

-- | Create a stub node for an image file when vision is disabled or fails.
imageStubNode :: FilePath -> Node
imageStubNode fp = Node
  { nodeId = T.pack fp
  , nodeLabel = fromText (T.pack (takeFileName fp))
  , nodeFileType = ImageFile
  , nodeSourceFile = fromText (T.pack fp)
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText "Image")
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = bitNodeKind
  }

-- | Convert ImageKind to Text for JSON serialization.
showKind :: ImageKind -> Text
showKind Photo      = "photo"
showKind Screenshot = "screenshot"
showKind Diagram    = "diagram"
showKind Resume     = "resume"
showKind Chart      = "chart"
showKind OtherKind  = "other"

-- | Convert Entity to JSON value.
entityToJSON :: Entity -> Aeson.Value
entityToJSON e = Aeson.object
  [ "label" Aeson..= entityLabel e
  , "type" Aeson..= entityType e
  , "confidence" Aeson..= entityConfidence e
  ]
