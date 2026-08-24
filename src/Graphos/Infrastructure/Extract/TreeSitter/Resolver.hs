{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.Resolver
  ( resolveImport
  ) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, (</>), takeExtension, replaceExtension, takeBaseName)

-- | Purely resolves an import specifier to a canonical target path and name.
--
-- Resolution rules:
--   * `node:` builtins → `("external:node:<pkg>", <pkg>)`
--   * scoped packages (`@scope/name`) → `("external:<specifier>", <specifier>)`
--   * bare packages (no leading `.` or `/`) → `("external:<specifier>", <specifier>)`
--   * relative/absolute paths → resolved against `base`, with `.js`/`.mjs`/`.cjs`
--     rewritten to `.ts` and a trailing `/` expanded to `index.ts`
--
-- Returns `Nothing` for an empty specifier.
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
      let target = resolveDots (takeDirectory base </> specifier)
          ext = takeExtension target
          newTarget = if ext `elem` [".js", ".mjs", ".cjs"]
                      then replaceExtension target ".ts"
                      else target
          finalTarget = if isSuffixOf "/" specifier || (not (null specifier) && last specifier == '/')
                      then newTarget </> "index.ts"
                      else newTarget
          targetName = T.pack $ takeBaseName finalTarget
      in Just (finalTarget, targetName)

-- | Resolve '.' and '..' path components in a FilePath.
resolveDots :: FilePath -> FilePath
resolveDots p = T.unpack (T.intercalate "/" (go (T.splitOn "/" (T.pack p)) []))
  where
    go :: [Text] -> [Text] -> [Text]
    go [] acc = reverse acc
    go (x : rest) acc
      | x == "." = go rest acc
      | x == ".." = case acc of
          [] -> go rest acc
          ("" : _) -> go rest acc
          (_ : _) -> go rest (tail acc)
      | otherwise = go rest (x : acc)
