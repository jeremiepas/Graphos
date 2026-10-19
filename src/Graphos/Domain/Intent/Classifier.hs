{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pure, config-driven deterministic intent-kind classifier.
--
-- The intent kinds (@Handler@, @Validator@, @Test@, @SchemaField@,
-- @ContractField@, @Doc@, @Module@) are assigned by ordered, configuration
-- declared rules keyed on the node's source-file path glob, its LSP symbol
-- kind, its label/name pattern, and its file type. The first rule that matches
-- wins; when no rule matches the kind is @Other@.
--
-- This module is pure and total: it performs no I/O and never invokes a model,
-- so classification is stable across runs and findings are reproducible
-- (intent-graph-verification decision 5).
module Graphos.Domain.Intent.Classifier
  ( -- * Intent kinds
    IntentKind(..)
    -- * Rules + config
  , KindRule(..)
  , ClassifierConfig(..)
  , defaultClassifierConfig
    -- * Classification
  , classify
  , classifyNode
  , matchGlob
   ) where

import Data.Aeson
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)

import Graphos.Domain.Types.Node
  ( Node, nodeLabel, nodeKind, nodeFileType, nodeSourceFile, FileType(..) )

-- | A deterministic intent kind. 'Other' is the total fallback when no rule
-- matches, so classification is always defined.
data IntentKind
  = Handler
  | Validator
  | Test
  | SchemaField
  | ContractField
  | Doc
  | Module
  | Other
  deriving (Eq, Show, Ord, Enum, Bounded)

instance ToJSON IntentKind where
  toJSON Handler       = "handler"
  toJSON Validator     = "validator"
  toJSON Test          = "test"
  toJSON SchemaField   = "schema-field"
  toJSON ContractField = "contract-field"
  toJSON Doc           = "doc"
  toJSON Module        = "module"
  toJSON Other         = "other"

instance FromJSON IntentKind where
  parseJSON = withText "IntentKind" $ \t -> case t of
    "handler"        -> pure Handler
    "validator"      -> pure Validator
    "test"           -> pure Test
    "schema-field"   -> pure SchemaField
    "contract-field" -> pure ContractField
    "doc"            -> pure Doc
    "module"         -> pure Module
    "other"          -> pure Other
    other            -> fail $ "Unknown intent kind: " ++ T.unpack other

-- | One ordered classification rule. Every matcher is optional; a rule matches
-- iff every present matcher holds (an AND-of-ORs: each field is an internal
-- OR across its list). 'ruleIntent' is the assigned kind when it matches.
data KindRule = KindRule
  { ruleName         :: !Text          -- human label, for config readability
  , rulePathGlobs    :: !(Maybe [Text]) -- source-file path globs (OR)
  , ruleSymbolKinds  :: !(Maybe [Text]) -- LSP symbol-kind text (OR)
  , ruleNamePatterns :: !(Maybe [Text]) -- label/name patterns (OR)
  , ruleFileTypes    :: !(Maybe [Text]) -- file-type text (OR)
  , ruleIntent       :: !IntentKind
  }

instance Show KindRule where
  show r = "KindRule " ++ ruleName r

instance ToJSON KindRule where
  toJSON r = object
    [ "name"        .= ruleName r
    , "pathGlobs"   .= rulePathGlobs r
    , "symbolKinds" .= ruleSymbolKinds r
    , "namePatterns" .= ruleNamePatterns r
    , "fileTypes"   .= ruleFileTypes r
    , "intent"      .= ruleIntent r
    ]

instance FromJSON KindRule where
  parseJSON = withObject "KindRule" $ \v -> KindRule
    <$> v .:  "name"
    <*> v .:? "pathGlobs"
    <*> v .:? "symbolKinds"
    <*> v .:? "namePatterns"
    <*> v .:? "fileTypes"
    <*> v .:  "intent"

-- | The ordered rule set. Configuration-driven: decode from a JSON/YAML file to
-- adapt the classifier to a project without code changes.
data ClassifierConfig = ClassifierConfig
  { classifierRules :: ![KindRule]
  } deriving (Eq, Show)

instance ToJSON ClassifierConfig where
  toJSON (ClassifierConfig rs) = object [ "rules" .= rs ]

instance FromJSON ClassifierConfig where
  parseJSON = withObject "ClassifierConfig" $ \v -> ClassifierConfig <$> v .: "rules"

-- | A sensible default rule set covering every intent kind. Ordered specific
-- first; 'Other' is the implicit fallback (no rule matches).
defaultClassifierConfig :: ClassifierConfig
defaultClassifierConfig = ClassifierConfig
  [ -- Modules / namespaces / packages (LSP symbol kinds)
    KindRule "module-symbol" (Just ["Module","Namespace","Package"]) Nothing Nothing Nothing Module
  , -- Handlers: name or handler directory
    KindRule "handler-name" Nothing Nothing ["*Handler*","*handle*"] Nothing Handler
  , KindRule "handler-path" (Just ["**/handlers/**","**/handler/**"]) Nothing Nothing Nothing Handler
  , -- Validators: name or validator directory
    KindRule "validator-name" Nothing Nothing ["*Validator*","*validate*","*validation*"] Nothing Validator
  , KindRule "validator-path" (Just ["**/validators/**","**/validation/**"]) Nothing Nothing Nothing Validator
  , -- Tests: spec/test naming or test directory
    KindRule "test-name" Nothing Nothing ["*Spec*","*spec*","*Test*","*test*"] Nothing Test
  , KindRule "test-path" (Just ["**/tests/**","**/test/**","**/testing/**"]) Nothing Nothing Nothing Test
  , -- Schema fields: a field inside a schema / models directory
    KindRule "schema-field" (Just ["**/schema/**","**/schemas/**","**/models/**"]) (Just ["Field"]) Nothing Nothing SchemaField
  , -- Contract fields: a field inside a contract directory
    KindRule "contract-field" (Just ["**/contract/**","**/contracts/**"]) (Just ["Field"]) Nothing Nothing ContractField
  , -- Documentation: doc-typed files or a docs directory
    KindRule "doc-file" Nothing Nothing Nothing (Just ["doc"]) Doc
  , KindRule "doc-path" (Just ["**/docs/**","**/documentation/**","*.md","*.markdown"]) Nothing Nothing Nothing Doc
  ]

-- | Case-insensitive glob match (@*@ and @**@ any sequence, @?@ one char).
matchGlob :: Text -> Text -> Bool
matchGlob pattern subject =
  mp (T.toLower (T.unpack pattern)) (T.toLower (T.unpack subject))
  where
    mp [] [] = True
    mp [] _  = False
    mp (c:cs) [] | c == '*' = mp cs []
                 | otherwise = False
    mp (c:cs) (d:ds)
      | c == '*'  = mp cs (d:ds) || mp (c:cs) ds
      | c == '?'  = mp cs ds
      | otherwise = c == d && mp cs ds
    mp _ [] = False

fileTypeText :: FileType -> Text
fileTypeText = \case
  CodeFile   -> "code"
  DocFile    -> "doc"
  PaperFile  -> "paper"
  ImageFile  -> "image"
  VideoFile  -> "video"
  AudioFile  -> "audio"
  OfficeFile -> "office"

-- | Classify by raw inputs. @mSymbolKind@ is the node's LSP symbol-kind text
-- (e.g. @"Function"@); 'Nothing' matches a 'ruleSymbolKinds' list only when the
-- list is empty.
classify
  :: ClassifierConfig
  -> Text          -- source-file path
  -> Maybe Text    -- LSP symbol-kind text
  -> Text          -- label / name
  -> Text          -- file-type text
  -> IntentKind
classify (ClassifierConfig rules) path mSymbolKind name fileType =
  case find (\r -> matchRule r path mSymbolKind name fileType) rules of
    Just r -> ruleIntent r
    Nothing -> Other

matchRule :: KindRule -> Text -> Maybe Text -> Text -> Text -> Bool
matchRule r path mSymbolKind name fileType =
  pathOk && symbolOk && nameOk && fileTypeOk
  where
    pathOk     = maybe True (\gs -> any (`matchGlob` path)   gs) (rulePathGlobs r)
    symbolOk   = maybe True (\ks -> mSymbolKind `elem` ks)     (ruleSymbolKinds r)
    nameOk     = maybe True (\ps -> any (`matchGlob` name)     ps) (ruleNamePatterns r)
    fileTypeOk = maybe True (\fs -> fileType `elem` fs)        (ruleFileTypes r)

-- | Classify a node, pulling the match inputs from its fields. Pure and total.
classifyNode :: ClassifierConfig -> Node -> IntentKind
classifyNode cfg n =
  classify cfg
    (toText (nodeSourceFile n))
    (fmap toText (nodeKind n))
    (toText (nodeLabel n))
    (fileTypeText (nodeFileType n))