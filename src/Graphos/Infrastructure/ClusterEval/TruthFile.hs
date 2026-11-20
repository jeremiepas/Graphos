{-# LANGUAGE OverloadedStrings #-}
-- | Explicit ground truth for @graphos clustereval --truth FILE@
-- (clustering-evaluation-and-selection, design D2): a JSON object mapping a
-- file path to the name of its group, e.g. a hand-made reference
-- architecture: @{ "src/a/x.ts": "core", "src/b/y.ts": "api" }@.
module Graphos.Infrastructure.ClusterEval.TruthFile
  ( parseTruth
  , loadTruthFile
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Value(..), eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Community.Partition (Partition, fromAssignment)

-- | Parse a truth file. Every value must be a string (the group name).
parseTruth :: BS.ByteString -> Either Text (Partition Text)
parseTruth bytes = case eitherDecodeStrict' bytes of
  Left err -> Left ("truth file is not JSON: " <> T.pack err)
  Right (Object o) ->
    let entries = [ (Key.toText k, v) | (k, v) <- KM.toList o ]
        bad = [ k | (k, v) <- entries, not (isString v) ]
    in case bad of
         [] -> Right (fromAssignment (Map.fromList [ (k, g) | (k, String g) <- entries ]))
         (k : _) -> Left ("truth file: the group of " <> k <> " is not a string")
  Right _ -> Left "truth file must be a JSON object mapping a file to its group"
  where
    isString (String _) = True
    isString _ = False

loadTruthFile :: FilePath -> IO (Either Text (Partition Text))
loadTruthFile path = do
  r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure $ case r of
    Left e -> Left ("cannot read truth file " <> T.pack path <> ": " <> T.pack (show e))
    Right bs -> parseTruth bs
