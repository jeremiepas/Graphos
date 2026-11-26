-- | Schema validation for the user-declared parts of @graphos.yaml@
-- (custom-extractors 2.1 / 2.2).
--
-- The loader parses a config file into a raw JSON 'Value' first, then asks
-- 'configSectionErrors' whether the @extractors:@ and @resolve:@ entries make
-- sense. Any error aborts the load with a message naming the offending
-- extension and field, instead of silently falling back to the compiled-in
-- defaults. Pure — no IO.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Config.Validation
  ( -- * Whole-file validation
    configSectionErrors
  , extractorSectionErrors
  , resolveSectionErrors

    -- * Single-entry validation
  , validateExtractorEntry
  , validateResolveEntry
  ) where

import Data.Aeson (FromJSON(..), Object, Value(..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T

import Graphos.Domain.Config.Extraction
import Graphos.Domain.Config.Resolve

-- ───────────────────────────────────────────────
-- Raw section validation
-- ───────────────────────────────────────────────

-- | Validate both user-declared schema sections of a parsed config document.
-- Returns every problem found (empty means the document is acceptable).
configSectionErrors :: Value -> [String]
configSectionErrors (Object o) = extractorSectionErrors o ++ resolveSectionErrors o
configSectionErrors _          = []

-- | Validate the @extractors:@ mapping: every entry must decode and satisfy
-- its mode's required/incompatible-field rules.
extractorSectionErrors :: Object -> [String]
extractorSectionErrors o = case KeyMap.lookup "extractors" o of
  Nothing -> []
  Just (Object entries) ->
    concatMap (\(k, v) -> withEntry "extractors" (keyText k)
                                (validateExtractorEntry (keyText k)) v)
              (KeyMap.toList entries)
  Just _ -> ["\"extractors\" must be a mapping of file extensions to entries"]

-- | Validate the @resolve:@ mapping: every entry must decode and satisfy the
-- requirements of its resolver kind.
resolveSectionErrors :: Object -> [String]
resolveSectionErrors o = case KeyMap.lookup "resolve" o of
  Nothing -> []
  Just (Object entries) ->
    concatMap (\(k, v) -> withEntry "resolve" (keyText k)
                                (validateResolveEntry (keyText k)) v)
              (KeyMap.toList entries)
  Just _ -> ["\"resolve\" must be a mapping of file extensions to resolver entries"]

-- | Decode one section entry and run its typed validator, prefixing decode
-- failures with @section["extension"]:@ so the message names the entry.
withEntry :: forall a. FromJSON a
          => String                 -- ^ section name
          -> String                 -- ^ extension key
          -> (a -> Either String ()) -- ^ typed validator
          -> Value
          -> [String]
withEntry section ext check value =
  case (parseEither parseJSON value :: Either String a) of
    Left err  -> [prefix ++ oneLine err]
    Right cfg -> case check cfg of
      Left err -> [err]
      Right () -> []
  where
    prefix = section ++ "[\"" ++ ext ++ "\"]: "

-- | Collapse a multi-line YAML/Aeson error into one log line.
oneLine :: String -> String
oneLine = T.unpack . T.replace "\n" "; " . T.pack

keyText :: Key -> String
keyText = T.unpack . Key.toText

-- ───────────────────────────────────────────────
-- Extractor entries
-- ───────────────────────────────────────────────

-- | Validate one per-extension extractor entry.
--
--   * @command@: requires a non-empty @command@ argv list; forbids the
--     tree-sitter @grammar@ / @grammar_path@ / @queries@ fields.
--   * @tree-sitter@: requires @grammar@ or @grammar_path@; forbids @command@,
--     @batch@ and @timeout@.
--   * @lsp@ / @stub@: forbid the command and runtime-grammar fields.
--
-- A non-positive @timeout@ is rejected for every mode.
validateExtractorEntry :: String -> ExtractorConfig -> Either String ()
validateExtractorEntry ext ec = firstLeft $ case ecMode ec of
  ExtractCommand    -> commandRules
  ExtractTreeSitter -> treeSitterRules
  ExtractLSP        -> languageServerRules
  ExtractStub       -> stubRules
  where
    prefix = "extractors[\"" ++ ext ++ "\"]: "

    fieldNotAllowed :: String -> String -> Either String ()
    fieldNotAllowed mode field = Left $ prefix ++ "field \"" ++ field
      ++ "\" is not valid with mode \"" ++ mode ++ "\""

    forbid :: String -> String -> Maybe a -> Either String ()
    forbid mode field = maybe (Right ()) (const (fieldNotAllowed mode field))

    timeoutRule :: Either String ()
    timeoutRule = case ecTimeoutSeconds ec of
      Just t | t <= 0 -> Left $ prefix ++ "\"timeout\" must be a positive number of seconds"
      _               -> Right ()

    commandRules :: [Either String ()]
    commandRules =
      [ case ecCommand ec of
          Nothing                  -> Left $ prefix ++ "mode \"command\" requires \"command\" (argv list)"
          Just []                  -> Left $ prefix ++ "mode \"command\" requires a non-empty \"command\" argv list"
          Just argv | any null argv -> Left $ prefix ++ "\"command\" argv entries must not be empty"
          Just _                   -> Right ()
      , forbid "command" "grammar" (ecGrammar ec)
      , forbid "command" "grammar_path" (ecGrammarPath ec)
      , forbid "command" "queries" (ecQueries ec)
      , timeoutRule
      ]

    treeSitterRules :: [Either String ()]
    treeSitterRules =
      [ case (ecGrammar ec, ecGrammarPath ec) of
          (Nothing, Nothing)        -> Left $ prefix ++ "mode \"tree-sitter\" requires \"grammar\" or \"grammar_path\""
          (Just g, _) | null g      -> Left $ prefix ++ "\"grammar\" must not be empty"
          (_, Just p) | null p      -> Left $ prefix ++ "\"grammar_path\" must not be empty"
          _                         -> Right ()
      , forbid "tree-sitter" "command" (ecCommand ec)
      , forbid "tree-sitter" "batch" (ecBatch ec)
      , forbid "tree-sitter" "timeout" (ecTimeoutSeconds ec)
      ]

    languageServerRules :: [Either String ()]
    languageServerRules =
      [ forbid "lsp" "command" (ecCommand ec)
      , forbid "lsp" "batch" (ecBatch ec)
      , forbid "lsp" "timeout" (ecTimeoutSeconds ec)
      , forbid "lsp" "grammar_path" (ecGrammarPath ec)
      , forbid "lsp" "queries" (ecQueries ec)
      ]

    stubRules :: [Either String ()]
    stubRules =
      [ forbid "stub" "command" (ecCommand ec)
      , forbid "stub" "batch" (ecBatch ec)
      , forbid "stub" "timeout" (ecTimeoutSeconds ec)
      , forbid "stub" "grammar_path" (ecGrammarPath ec)
      , forbid "stub" "queries" (ecQueries ec)
      ]

-- ───────────────────────────────────────────────
-- Resolve entries
-- ───────────────────────────────────────────────

-- | Validate one @resolve:@ entry against its resolver kind.
--
--   * @package@ / @psr4@ require a non-empty @roots@ list.
--   * @module-path@ requires a @source_of_truth@ index file.
--   * @path-prefix@ has no required fields.
--
-- Empty path strings are rejected in every kind. Unknown kinds never reach
-- this function: 'ResolveKind'\'s 'FromJSON' instance already fails, and the
-- loader turns that decode failure into a fatal config error.
validateResolveEntry :: String -> ResolveConfig -> Either String ()
validateResolveEntry ext rc = firstLeft
  [ case rcKind rc of
      ResolvePackage    -> requireRoots "package"
      ResolvePSR4       -> requireRoots "psr4"
      ResolveModulePath -> case rcSourceOfTruth rc of
        Just f | not (null f) -> Right ()
        _ -> Left $ prefix ++ "kind \"module-path\" requires \"source_of_truth\""
      ResolvePathPrefix -> Right ()
  , if any null (rcRoots rc)
      then Left $ prefix ++ "\"roots\" must not contain empty paths"
      else Right ()
  ]
  where
    prefix = "resolve[\"" ++ ext ++ "\"]: "

    requireRoots :: String -> Either String ()
    requireRoots name
      | null (rcRoots rc) = Left $ prefix ++ "kind \"" ++ name
          ++ "\" requires a non-empty \"roots\" list"
      | otherwise         = Right ()

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | First failure in a list of checks, or 'Right' when all pass.
firstLeft :: [Either String ()] -> Either String ()
firstLeft []               = Right ()
firstLeft (Right () : rest) = firstLeft rest
firstLeft (Left err : _)    = Left err
