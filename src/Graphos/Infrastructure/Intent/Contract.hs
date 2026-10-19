-- | Intent-graph projection serialisation: strict, well-formed export/load
-- (intent-graph-verification 3.2).
--
-- 'exportIntentProjection' emits the canonical JSON of a projection;
-- 'importIntentProjection' decodes it and then enforces well-formedness: an
-- edge whose source or target is not a declared node is rejected, naming the
-- dangling endpoint. This is the loader half of the "every edge endpoint
-- declared" contract.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Intent.Contract
  ( exportIntentProjection
  , importIntentProjection
   ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Intent.Projection
  ( IntentProjection, checkWellFormed )

-- | Serialize a projection to canonical JSON bytes (byte-stable: the projection
-- already orders its edges canonically).
exportIntentProjection :: IntentProjection -> BS.ByteString
exportIntentProjection = Aeson.encode

-- | Decode a projection and reject ill-formed input. Returns 'Left' naming the
-- dangling endpoint when an edge references a node that is not declared.
importIntentProjection :: BS.ByteString -> Either Text IntentProjection
importIntentProjection bs =
  case Aeson.decode bs of
    Left err ->
      Left $ "intent projection: could not parse intent contract: "
             <> T.pack (show err)
    Right p -> case checkWellFormed p of
      Right _ -> Right p
      Left msg -> Left msg
