{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.Resolver
  ( resolveImport
  ) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, (</>), normalise, takeExtension, replaceExtension, takeBaseName)

-- | Purely resolves an import specifier to a canonical target path and name.
-- Returns Nothing if the specifier is invalid or cannot be resolved.
resolveImport :: FilePath -> String -> Maybe (FilePath, Text)
resolveImport _ "" = Nothing
resolveImport base specifier
  | "node:" `isPrefixOf` specifier =
      let pkg = drop 5 specifier
      in Just ("external:node:" <> pkg, T.pack pkg)
  | "@" `isPrefixOf` specifier =
      Just ("external:" <> specifier, T.pack specifier)
  | not ("." `isPrefixOf` specifier) && not ("/" `isPrefixOf` specifier) =
      Just ("external:" <> specifier, T.pack specifier)
  | otherwise =
      let target = normalise (takeDirectory base </> specifier)
          ext = takeExtension target
          newTarget = if ext `elem` [".js", ".mjs", ".cjs"]
                      then replaceExtension target ".ts"
                      else target
          finalTarget = if isSuffixOf "/" specifier || (not (null specifier) && last specifier == '/')
                      then newTarget </> "index.ts"
                      else newTarget
          targetName = T.pack $ takeBaseName finalTarget
      in Just (finalTarget, targetName)
