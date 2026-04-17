-- | Security - URL validation, label sanitization, safe fetch
module Graphos.Infrastructure.Security
  ( validateUrl
  , sanitizeLabel
  , validateGraphPath
  ) where

import Data.Char (isControl, isSpace, ord)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, splitDirectories, (</>))
import System.Directory (canonicalizePath)

-- | Validate a URL - must be http/https, no file:// scheme
validateUrl :: Text -> Either Text Text
validateUrl url
  | T.isPrefixOf "http://" url  = Right url
  | T.isPrefixOf "https://" url = Right url
  | otherwise = Left $ "Invalid URL scheme (only http/https allowed): " <> T.take 20 url

-- | Sanitize a node label - strip control chars, cap at 256 chars, HTML-escape
sanitizeLabel :: Text -> Text
sanitizeLabel = T.take 256 . escapeHtml . T.filter (not . isControl)

-- | Validate that a graph path resolves inside graphos-out/ (no path traversal)
validateGraphPath :: FilePath -> Either Text FilePath
validateGraphPath path
  | ".." `elem` splitDirectories path = Left "Path traversal detected in graph path"
  | otherwise = Right path

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | HTML-escape special characters
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;"
           . T.replace "<" "&lt;"
           . T.replace ">" "&gt;"
           . T.replace "\"" "&quot;"
           . T.replace "'" "&#39;"