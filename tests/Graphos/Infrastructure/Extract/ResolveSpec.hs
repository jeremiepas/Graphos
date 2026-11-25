{-# LANGUAGE OverloadedStrings #-}
-- | Declarative import resolution (custom-extractors 6.1–6.5): one case per
-- scenario of the @import-resolution@ delta, on hand-built extractions.
module Graphos.Infrastructure.Extract.ResolveSpec (spec) where

import Test.Hspec
import Data.Bits ((.|.))
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)

import Graphos.Domain.Config (ResolveConfig(..), ResolveKind(..))
import Graphos.Domain.Types
  ( Confidence(..), Edge(..), EdgeId(..), Extraction(..), FileType(..)
  , Node(..), Relation(..), bitNodeKind, bitNodeLineEnd, bitNodeLineStart
  , computePresentBits )
import Graphos.Infrastructure.Extract.Resolve

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

moduleNode :: FilePath -> Node
moduleNode fp =
  let n = Node
        { nodeId           = T.pack fp
        , nodeLabel        = fromText (T.pack fp)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack fp)
        , nodeSource       = Nothing
        , nodeLineStart    = Just 1
        , nodeLineEnd      = Just 1
        , nodeSignature    = Nothing
        , nodeCommunityId  = Nothing
        , nodeKind         = Just (fromText "Module")
        , nodeDegree       = Nothing
        , nodeIsBridge     = Nothing
        , nodeExtra        = Nothing
        , nodePresentBits  = 0
        }
  in n { nodePresentBits = computePresentBits n .|. bitNodeKind .|. bitNodeLineStart .|. bitNodeLineEnd }

-- | The placeholder node an unresolved import leaves behind.
externalNode :: String -> Node
externalNode sp =
  let fp = "external:" ++ sp
      n = (moduleNode fp)
        { nodeKind      = Just (fromText "External")
        , nodeLineStart = Nothing
        , nodeLineEnd   = Nothing
        , nodePresentBits = 0
        }
  in n { nodePresentBits = computePresentBits n .|. bitNodeKind }

importEdge :: FilePath -> String -> Edge
importEdge src sp =
  Edge
    { edgeId         = EdgeId (T.pack (src ++ "->external:" ++ sp ++ ":imports"))
    , edgeSource     = T.pack src
    , edgeTarget     = T.pack ("external:" ++ sp)
    , edgeRelation   = Imports
    , edgeWeight     = 1
    , edgeConfidence = Confidence 1
    , edgeExtra      = Nothing
    }

mkExtraction :: [Node] -> [Edge] -> Extraction
mkExtraction ns es = Extraction
  { extractionNodes = Map.fromList [ (nodeId n, n) | n <- ns ]
  , extractionEdges = Map.fromList [ (edgeId e, e) | e <- es ]
  , extractionErrors = []
  }

-- | Source files (relative) an importer's @imports@ edges point at.
targetsOf :: FilePath -> Extraction -> [Text]
targetsOf src ex = sort
  [ toText (nodeSourceFile n)
  | e <- Map.elems (extractionEdges ex)
  , edgeRelation e == Imports
  , Just s <- [Map.lookup (edgeSource e) (extractionNodes ex)]
  , toText (nodeSourceFile s) == T.pack src
  , Just n <- [Map.lookup (edgeTarget e) (extractionNodes ex)] ]

run :: Map.Map String ResolveConfig -> Map.Map String ModuleIndex -> Extraction -> (Extraction, ResolveStats)
run cfg indexes = resolveDeclarativeImports cfg indexes

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────

spec :: Spec
spec = describe "Declarative import resolution (custom-extractors 6)" $ do

  describe "package resolver" $ do
    it "a config roots entry resolves a dotted import to its source file" $ do
      let ex = mkExtraction
            [ moduleNode "src/main/java/com/example/App.java"
            , moduleNode "src/main/java/com/example/Foo.java"
            , externalNode "com.example.Foo" ]
            [ importEdge "src/main/java/com/example/App.java" "com.example.Foo" ]
          cfg = Map.fromList
            [ (".java", ResolveConfig ResolvePackage ["src/main/java"] Nothing) ]
          (ex', stats) = run cfg Map.empty ex
      targetsOf "src/main/java/com/example/App.java" ex'
        `shouldBe` ["src/main/java/com/example/Foo.java"]
      rsPackage stats `shouldBe` 1
      rsDropped stats `shouldBe` 1

    it "the built-in conventional roots resolve without a config entry" $ do
      let ex = mkExtraction
            [ moduleNode "src/main/kotlin/org/example/App.kt"
            , moduleNode "src/main/kotlin/org/example/model/Cart.kt"
            , externalNode "org.example.model.Cart" ]
            [ importEdge "src/main/kotlin/org/example/App.kt" "org.example.model.Cart" ]
          (ex', stats) = run Map.empty Map.empty ex
      targetsOf "src/main/kotlin/org/example/App.kt" ex'
        `shouldBe` ["src/main/kotlin/org/example/model/Cart.kt"]
      rsPackage stats `shouldBe` 1

    it "a static import member retries without the last segment" $ do
      let ex = mkExtraction
            [ moduleNode "src/main/java/org/example/App.java"
            , moduleNode "src/main/java/org/example/util/Money.java"
            , externalNode "org.example.util.Money.zero" ]
            [ importEdge "src/main/java/org/example/App.java" "org.example.util.Money.zero" ]
          cfg = Map.fromList
            [ (".java", ResolveConfig ResolvePackage ["src/main/java"] Nothing) ]
          (ex', _) = run cfg Map.empty ex
      targetsOf "src/main/java/org/example/App.java" ex'
        `shouldBe` ["src/main/java/org/example/util/Money.java"]

    it "leaves an unresolved package external" $ do
      let ex = mkExtraction
            [ moduleNode "src/main/java/org/example/App.java"
            , externalNode "com.example.Missing" ]
            [ importEdge "src/main/java/org/example/App.java" "com.example.Missing" ]
          cfg = Map.fromList
            [ (".java", ResolveConfig ResolvePackage ["src/main/java"] Nothing) ]
          (ex', stats) = run cfg Map.empty ex
      targetsOf "src/main/java/org/example/App.java" ex' `shouldBe` ["external:com.example.Missing"]
      rsPackage stats `shouldBe` 0
      -- an untouched placeholder is never dropped
      Map.member "external:com.example.Missing" (extractionNodes ex') `shouldBe` True

  describe "psr4 resolver" $ do
    it "maps a namespace to its file under the configured root" $ do
      let ex = mkExtraction
            [ moduleNode "src/App/Http/Controller.php"
            , moduleNode "src/App/Models/User.php"
            , externalNode "App\\Models\\User" ]
            [ importEdge "src/App/Http/Controller.php" "App\\Models\\User" ]
          cfg = Map.fromList
            [ (".php", ResolveConfig ResolvePSR4 ["src"] Nothing) ]
          (ex', stats) = run cfg Map.empty ex
      targetsOf "src/App/Http/Controller.php" ex'
        `shouldBe` ["src/App/Models/User.php"]
      rsPSR4 stats `shouldBe` 1

  describe "module-path resolver" $ do
    it "selects the module directory from the source-of-truth index" $ do
      let ex = mkExtraction
            [ moduleNode "app/ui/Screen.java"
            , moduleNode "app/src/main/java/app/ui/Home.java"
            , externalNode "app.ui.Screen" ]
            [ importEdge "app/src/main/java/app/ui/Home.java" "app.ui.Screen" ]
          cfg = Map.fromList
            [ (".java", ResolveConfig ResolveModulePath [] (Just "settings.gradle")) ]
          indexes = Map.fromList [ (".java", Map.fromList [("app", "app")]) ]
          (ex', stats) = run cfg indexes ex
      targetsOf "app/src/main/java/app/ui/Home.java" ex' `shouldBe` ["app/ui/Screen.java"]
      rsModulePath stats `shouldBe` 1

    it "parses Gradle and Maven module declarations" $
      parseModuleIndex
        "include ':app', ':core'\n// comment\n<module>web</module>\nplain lib\n"
        `shouldBe` [ ("app", "app"), ("core", "core"), ("web", "web"), ("plain", "lib") ]

  describe "path-prefix resolver" $ do
    it "concatenates the import path with the importer extension" $ do
      let ex = mkExtraction
            [ moduleNode "src/app.py"
            , moduleNode "src/lib/util.py"
            , externalNode "./lib/util" ]
            [ importEdge "src/app.py" "./lib/util" ]
          cfg = Map.fromList
            [ (".py", ResolveConfig ResolvePathPrefix ["src"] Nothing) ]
          (ex', stats) = run cfg Map.empty ex
      targetsOf "src/app.py" ex' `shouldBe` ["src/lib/util.py"]
      rsPathPrefix stats `shouldBe` 1

  describe "precedence" $ do
    it "warns when a config entry overrides a built-in resolver, naming extension and kind" $
      resolveOverrideWarnings
        (Map.fromList [ (".java", ResolveConfig ResolvePathPrefix [] Nothing) ])
        `shouldBe` ["resolve[\".java\"]: config overrides the built-in path-prefix resolver"]

    it "does not warn for an extension with no built-in resolver" $
      resolveOverrideWarnings
        (Map.fromList [ (".php", ResolveConfig ResolvePSR4 ["src"] Nothing) ])
        `shouldBe` []

    it "the config entry's kind wins over the built-in one" $ do
      let cfg = Map.fromList [ (".java", ResolveConfig ResolvePathPrefix ["src"] Nothing) ]
      resolveKindName (rcKind (effectiveResolveConfigs cfg Map.! ".java")) `shouldBe` "path-prefix"
      resolveKindName (rcKind (effectiveResolveConfigs Map.empty Map.! ".java")) `shouldBe` "package"

  describe "pass invariants" $ do
    it "keeps the EdgeId of a rewritten edge" $ do
      let ex = mkExtraction
            [ moduleNode "src/main/java/com/example/App.java"
            , moduleNode "src/main/java/com/example/Foo.java"
            , externalNode "com.example.Foo" ]
            [ importEdge "src/main/java/com/example/App.java" "com.example.Foo" ]
          cfg = Map.fromList
            [ (".java", ResolveConfig ResolvePackage ["src/main/java"] Nothing) ]
          (ex', _) = run cfg Map.empty ex
          importIds x = sort [ k | (k, e) <- Map.toList (extractionEdges x), edgeRelation e == Imports ]
      importIds ex' `shouldBe` importIds ex

    it "is the identity without an external placeholder" $ do
      let ex = mkExtraction [ moduleNode "src/a.py" ] []
      run Map.empty Map.empty ex `shouldBe` (ex, ResolveStats 0 0 0 0 0)
