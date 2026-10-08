-- | Multi-source configuration: named filesystem roots that together form a
-- single graph. Leaf module: depends on no other Graphos modules.
module Graphos.Domain.Config.Source
  ( SourceConfig(..)
  , mkSourceConfig
  , validSources
  , parseShorthand'
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), withObject, (.:?), (.:))
import qualified Data.Text as T
import Data.Text (Text)

-- | One named filesystem root contributing files to the graph.
data SourceConfig
  = SourceConfig
    { scName   :: !Text       -- ^ unique source name (prefixed onto paths)
    , scPath   :: !FilePath   -- ^ root directory of this source
    , scIgnore :: ![Text]     -- ^ extra ignore patterns scoped to this source
    } deriving (Eq, Show)

-- | Smart constructor: rejects an empty source name.
mkSourceConfig :: Text -> FilePath -> [Text] -> Either String SourceConfig
mkSourceConfig name path ignores
  | T.null name = Left "source name must not be empty"
  | otherwise   = Right (SourceConfig name path ignores)

-- | Validate a source list: names must be non-empty and unique.
-- The error names the offending entry (name when present, else its path).
validSources :: [SourceConfig] -> Either String [SourceConfig]
validSources srcs = case filter (T.null . scName) srcs of
  (s : _) -> Left $ "source name must not be empty (at path " ++ scPath s ++ ")"
  [] -> case firstDuplicateName srcs of
    Just name -> Left $ "duplicate source name: " ++ T.unpack name
    Nothing   -> Right srcs

firstDuplicateName :: [SourceConfig] -> Maybe Text
firstDuplicateName = go []
  where
    go _ [] = Nothing
    go seen (s : ss)
      | scName s `elem` seen = Just (scName s)
      | otherwise            = go (scName s : seen) ss

instance ToJSON SourceConfig where
  toJSON s = object
    [ "name"   .= scName s
    , "path"   .= scPath s
    , "ignore" .= scIgnore s
    ]

-- | Accepts either the full mapping form
-- (@{name: repoA, path: ~/code/repoA, ignore: [...]}@) or the shorthand
-- string form (a bare path string; the directory base name becomes the
-- source name).
instance FromJSON SourceConfig where
  parseJSON (Data.Aeson.String t) = pure (parseShorthand' t)
  parseJSON v = withObject "SourceConfig" objFn v
    where
      objFn obj = do
        mname <- obj .:? "name"
        path  <- obj .:  "path"
        mign  <- obj .:? "ignore"
        let name = maybe (T.pack (basePath path)) id mname
        pure SourceConfig { scName = name, scPath = path, scIgnore = maybe [] id mign }

-- | Directory base name of a path (trailing slashes tolerated).
basePath :: FilePath -> FilePath
basePath p = reverse (takeWhile (/= '/') (dropWhile (== '/') (reverse p)))

-- | Parse a string-form source (shorthand: a bare path; the directory base
-- name becomes the source name). Used by the 'FromJSON' instance and exposed
-- for testing.
parseShorthand' :: Text -> SourceConfig
parseShorthand' t =
  let p = T.unpack t
  in SourceConfig { scName = T.pack (basePath p), scPath = p, scIgnore = [] }