{-# LANGUAGE StrictData #-}
-- | Hex color representation and palette utilities for term visualization.
module Graphos.Domain.HexColor
  ( HexColor
  , d3Palette
  , assignTermColors
  ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | A CSS hex color string (e.g. "#441")
newtype HexColor = HexColor Text
  deriving (Eq, Show)

-- | D3's 20-color qualitative palette (Tableau-20 adapted for web).
d3Palette :: [HexColor]
d3Palette = map HexColor
  [ "#1b9e77"
  , "#d95f02"
  , "#7570b3"
  , "#e7298a"
  , "#66a103"
  , "#e6ab82"
  , "#a6761d"
  , "#666666"
  , "#e0e0e0"
  , "#377eb8"
  , "#ff7f00"
  , "#4daf4a"
  , "#f2c5dc"
  , "#b3deeb"
  , "#f3b9ac"
  , "#bf4040"
  , "#77b053"
  , "#e8a600"
  , "#c531ae"
  , "#768890"
  ]

-- | Assign a deterministic color from the D3 palette to each term in a list.
-- Terms are sorted lexicographically, then colors are cycled through the palette.
assignTermColors :: [Text] -> Map Text HexColor
assignTermColors terms =
  let sorted = sort terms
      colors = cycle d3Palette
      mapped = zipWith (\c t -> (t, c)) colors sorted
  in Map.fromList mapped