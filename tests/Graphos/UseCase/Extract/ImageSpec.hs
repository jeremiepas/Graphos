-- | Tests for Image extraction module — node creation from ImageAnalysis.
module Graphos.UseCase.Extract.ImageSpec where

import Test.Hspec
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Infrastructure.LLM.Vision (ImageAnalysis(..), Entity(..), ImageKind(..))
import Graphos.Infrastructure.Extract.Image (imageAnalysisToExtraction, imageStubNode)

spec :: Spec
spec = do
  describe "imageStubNode" $ do
    it "creates a stub node with ImageFile type and Image kind" $ do
      let node = imageStubNode "/path/to/photo.png"
      nodeId node `shouldBe` "/path/to/photo.png"
      nodeLabel node `shouldBe` "photo.png"
      nodeFileType node `shouldBe` ImageFile
      nodeKind node `shouldBe` Just "Image"
      nodeExtra node `shouldBe` Nothing

  describe "imageAnalysisToExtraction" $ do
    let analysis = ImageAnalysis
          { iaDescription = "A landscape photo with mountains"
          , iaEntities =
              [ Entity { entityLabel = "Mountain", entityType = "GeographicFeature", entityConfidence = 0.9 }
              , Entity { entityLabel = "Sky", entityType = "VisualElement", entityConfidence = 0.7 }
              ]
          , iaKind = Photo
          }
    let extraction = imageAnalysisToExtraction "/path/to/photo.png" analysis
        nodes = Map.elems (extractionNodes extraction)
        edges = Map.elems (extractionEdges extraction)

    it "creates an ImageFile node as the primary node" $ do
      let imageNodes = filter (\n -> nodeKind n == Just "Image") nodes
      length imageNodes `shouldBe` 1
      nodeLabel (fromJust (listToMaybe imageNodes)) `shouldBe` "photo.png"

    it "creates entity nodes linked to the image" $ do
      let entityNodes = filter (\n -> nodeKind n /= Just "Image") nodes
      length entityNodes `shouldBe` 2

    it "stores description and kind in nodeExtra of the image node" $ do
      let imageNode = fromJust (listToMaybe (filter (\n -> nodeKind n == Just "Image") nodes))
      case nodeExtra imageNode of
        Just (Aeson.Object obj) -> do
          case KeyMap.lookup "description" obj of
            Just (Aeson.String desc) -> desc `shouldBe` "A landscape photo with mountains"
            _ -> expectationFailure "description not found in nodeExtra"
          case KeyMap.lookup "kind" obj of
            Just (Aeson.String kind) -> kind `shouldBe` "photo"
            _ -> expectationFailure "kind not found in nodeExtra"
        _ -> expectationFailure "nodeExtra is not an object"

    it "creates Contains edges from image to entities" $ do
      let containsEdges = filter (\e -> edgeRelation e == Contains) edges
      length containsEdges `shouldBe` 2

    it "uses filePath#entityLabel as entity node ID" $ do
      let entityNodes = filter (\n -> nodeKind n /= Just "Image") nodes
      map nodeId entityNodes `shouldSatisfy` (\ids -> all (\nid -> "/path/to/photo.png#" `T.isPrefixOf` nid) ids)