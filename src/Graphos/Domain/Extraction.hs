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

import Data.Text (Text)
import qualified Data.Text as T
import Graphos.Domain.Types

-- | An extractor produces extraction results from a source
class Extractor a where
  extract :: a -> FilePath -> IO Extraction

-- | Result from an extraction pass
data ExtractionResult a = ExtractionResult
  { resultData    :: a
  , resultTokens  :: (Int, Int)  -- (input, output)
  } deriving (Eq, Show)

-- | Validate that an extraction has the required fields
validateExtraction :: Extraction -> Either [Text] Extraction
validateExtraction ext =
  let errors = []
        <> validateNodeIds (extractionNodes ext)
        <> validateEdges (extractionEdges ext)
        <> validateHyperedges (extractionHyperedges ext)
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
      ++ ["Invalid confidence score" | any (\e -> edgeConfidenceScore e < 0 || edgeConfidenceScore e > 1) edges]
    validateHyperedges hes =
      ["Hyperedge with < 3 nodes: " <> hyperedgeId h | h <- hes, length (hyperedgeNodes h) < 3]

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