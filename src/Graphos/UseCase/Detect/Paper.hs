-- | Paper detection heuristic - detect academic papers among docs
module Graphos.UseCase.Detect.Paper
  ( looksLikePaper
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Heuristic to detect academic papers among .md/.txt files.
-- Checks for: arXiv IDs, DOIs, abstract sections, citation patterns.
looksLikePaper :: Text -> Bool
looksLikePaper content =
  let lower = T.toLower content
      hasArxiv = any (`T.isInfixOf` lower) arxivPatterns
      hasDoi = "10." `T.isInfixOf` lower && ("/" `T.isInfixOf` T.drop 3 (T.dropWhile (/= '1') lower))
      hasAbstract = any (`T.isInfixOf` lower) abstractPatterns
      hasCitations = any (`T.isInfixOf` lower) citationPatterns
      hasReferences = any (`T.isInfixOf` lower) referencePatterns
      score = sum [1 | hasArxiv] + sum [1 | hasDoi] + sum [1 | hasAbstract] + sum [1 | hasCitations] + sum [1 | hasReferences]
  in score >= 2

-- | ArXiv ID patterns
arxivPatterns :: [Text]
arxivPatterns =
  [ "arxiv.org"
  , "arxiv:"
  , "arxiv."
  ]

-- | Abstract section patterns
abstractPatterns :: [Text]
abstractPatterns =
  [ "abstract"
  , "summary"
  , "résumé"
  ]

-- | Citation patterns
citationPatterns :: [Text]
citationPatterns =
  [ "et al."
  , "et al.,"
  , "ibid."
  , "op. cit."
  , "cf."
  , "proceedings of"
  , "journal of"
  , "ieee"
  , "acm "
  ]

-- | Reference section patterns
referencePatterns :: [Text]
referencePatterns =
  [ "references"
  , "bibliography"
  , "works cited"
  , "références"
  ]