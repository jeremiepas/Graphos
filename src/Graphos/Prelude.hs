-- | Graphos Prelude - common imports and utilities
-- Re-exports commonly used types and functions to reduce import noise
{-# LANGUAGE LambdaCase #-}
module Graphos.Prelude
  ( module X
  ) where

import Control.Monad            as X
import Data.Aeson               as X (ToJSON(..), FromJSON(..), Value, object, (.=), (.:))
import Data.ByteString           as X (ByteString)
import Data.Foldable             as X (foldl', traverse_)
import Data.HashMap.Strict       as X (HashMap)
import Data.Int                  as X
import Data.List                 as X (sort, sortOn, groupBy, intercalate, nub)
import Data.Map.Strict           as X (Map)
import Data.Maybe                as X (fromMaybe, catMaybes, mapMaybe, isJust, isNothing)
import Data.Set                  as X (Set)
import Data.Text                 as X (Text)
import Data.Traversable          as X
import GHC.Generics             as X (Generic)
