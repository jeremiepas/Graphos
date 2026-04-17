-- | Diacritic-insensitive node search - Unicode normalization for queries
module Graphos.UseCase.Query.Normalize
  ( normalizeForSearch
  , stripDiacritics
  ) where

import Data.Char (isAlpha, toLower, ord)
import Data.Text (Text)
import qualified Data.Text as T

-- | Normalize text for search: strip diacritics + lowercase
normalizeForSearch :: Text -> Text
normalizeForSearch = T.toLower . stripDiacritics

-- | Strip diacritics from text using a character mapping approach.
-- This covers common Latin diacritics (accent, grave, umlaut, etc.).
-- For full Unicode normalization, use the text-icu library.
stripDiacritics :: Text -> Text
stripDiacritics = T.concatMap stripChar

-- | Strip diacritics from a single character
stripChar :: Char -> Text
stripChar c = case lookup c diacriticMap of
  Just base -> T.singleton base
  Nothing   -> T.singleton c

-- | Map of common diacritic characters to their base form
-- Covers: À-ÿ range plus common ligatures
diacriticMap :: [(Char, Char)]
diacriticMap =
  [ ('À','A'), ('Á','A'), ('Â','A'), ('Ã','A'), ('Ä','A'), ('Å','A'), ('Æ','A')
  , ('à','a'), ('á','a'), ('â','a'), ('ã','a'), ('ä','a'), ('å','a'), ('æ','a')
  , ('Ç','C'), ('ç','c')
  , ('È','E'), ('É','E'), ('Ê','E'), ('Ë','E')
  , ('è','e'), ('é','e'), ('ê','e'), ('ë','e')
  , ('Ì','I'), ('Í','I'), ('Î','I'), ('Ï','I')
  , ('ì','i'), ('í','i'), ('î','i'), ('ï','i')
  , ('Ð','D'), ('ð','d')
  , ('Ñ','N'), ('ñ','n')
  , ('Ò','O'), ('Ó','O'), ('Ô','O'), ('Õ','O'), ('Ö','O'), ('Ø','O')
  , ('ò','o'), ('ó','o'), ('ô','o'), ('õ','o'), ('ö','o'), ('ø','o')
  , ('Ù','U'), ('Ú','U'), ('Û','U'), ('Ü','U')
  , ('ù','u'), ('ú','u'), ('û','u'), ('ü','u')
  , ('Ý','Y'), ('ý','y'), ('ÿ','y')
  , ('Þ','T'), ('þ','t')
  , ('ß','s')
  ]