-- | Extraction schema and validation.
module Graphos.Domain.Extraction
  ( -- * Type classes for extraction
    Extractor(..)
  , ExtractionResult(..)

    -- * Validation
  , validateExtraction

    -- * LSP extraction types
  , LSPSymbol(..)
  , LSPDocumentSymbol(..)
  , LSPReference(..)
  , LSPCallHierarchyItem(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Graphos.Domain.Types

class Extractor a where
  extract :: a -> FilePath -> IO Extraction

data ExtractionResult a = ExtractionResult
  { resultData    :: a
  , resultTokens  :: (Int, Int)
  } deriving (Eq, Show)

validateExtraction :: Extraction -> Either [Text] Extraction
validateExtraction ext =
  let nodes = Map.elems (extractionNodes ext)
      edges = Map.elems (extractionEdges ext)
      errors = []
        <> validateNodeIds nodes
        <> validateEdges edges
  in if null errors
     then Right ext
     else Left errors
  where
    validateNodeIds nodes =
      let duplicateIds = [nid | nid <- nodeLabel <$> nodes
                              , length (filter (== nid) (nodeLabel <$> nodes)) > 1]
      in ["Duplicate node labels: " <> T.intercalate ", " (take 5 duplicateIds) | not (null duplicateIds)]
        ++ ["Empty node ID in extraction" | any (T.null . nodeId) nodes]
    validateEdges edges =
      ["Empty source in edge" | any (T.null . edgeSource) edges]
      ++ ["Empty target in edge" | any (T.null . edgeTarget) edges]
      ++ ["Invalid confidence score" | any (\e -> let Confidence c = edgeConfidence e in c < 0 || c > 1) edges]

-- ───────────────────────────────────────────────
-- LSP extraction types
-- ───────────────────────────────────────────────

-- | A symbol extracted via LSP documentSymbol
data LSPSymbol = LSPSymbol
  { lspSymName           :: Text
  , lspSymKind           :: Int  -- LSP SymbolKind
  , lspSymRange          :: (Int, Int)  -- (startLine, endLine)
  , lspSymChildren       :: [LSPSymbol]
  } deriving (Eq, Show)

-- | Document symbols response from LSP
data LSPDocumentSymbol = LSPDocumentSymbol
  { lspDocSymName      :: Text
  , lspDocSymKind      :: Int
  , lspDocSymDetail    :: Maybe Text
  , lspDocSymStartLine :: Int
  , lspDocSymEndLine   :: Int
  } deriving (Eq, Show)

-- | A reference location from LSP
data LSPReference = LSPReference
  { lspRefUri        :: Text
  , lspRefStartLine  :: Int
  , lspRefStartChar  :: Int
  } deriving (Eq, Show)

-- | Call hierarchy item from LSP
data LSPCallHierarchyItem = LSPCallHierarchyItem
  { lspCallName :: Text
  , lspCallKind :: Int
  , lspCallUri  :: Text
  , lspCallRange :: (Int, Int)  -- (startLine, endLine)
  } deriving (Eq, Show)