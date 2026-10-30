{-# LANGUAGE OverloadedStrings #-}
-- | @imports@ edges for Go, Rust and Ruby: real tree-sitter parse of small
-- on-disk projects (written to a temp directory), then
-- 'withImportEdges' — the same path the production extraction port takes.
module Graphos.Infrastructure.Extract.TreeSitter.ImportsSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (toText)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)

import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types (Node(..), Edge(..), Relation(..), Extraction(..))
import Graphos.Infrastructure.Extract.TreeSitter.Core (parseWithGrammar)
import Graphos.Infrastructure.Extract.TreeSitter.Convert (tsNodesToExtraction)
import Graphos.Infrastructure.Extract.TreeSitter.Imports
import Graphos.Infrastructure.Wiring (getGrammarPtr)

-- | Write @files@ (relative path, content) under @root@.
writeTree :: FilePath -> [(FilePath, Text)] -> IO ()
writeTree root = mapM_ $ \(rel, body) -> do
  let p = root </> rel
  createDirectoryIfMissing True (takeDirectory p)
  BS.writeFile p (TE.encodeUtf8 body)

-- | Extraction of one file, import edges included.
extractFile :: String -> FilePath -> IO Extraction
extractFile grammar path = do
  src <- BS.readFile path
  case getGrammarPtr grammar of
    Nothing -> fail ("no grammar " ++ grammar)
    Just lang -> do
      parsed <- parseWithGrammar lang src
      case parsed of
        Nothing -> fail ("parse failed: " ++ path)
        Just ts -> withImportEdges grammar path ts (tsNodesToExtraction GranularityFunction path ts)

-- | (target source_file, weight, extra) of every imports edge.
importTargets :: Extraction -> [(Text, Double, Value)]
importTargets ex =
  sort [ (toText (nodeSourceFile n), edgeWeight e, maybe Null id (edgeExtra e))
       | e <- Map.elems (extractionEdges ex), edgeRelation e == Imports
       , Just n <- [Map.lookup (edgeTarget e) (extractionNodes ex)] ]

targetFiles :: Extraction -> [Text]
targetFiles ex = sort [f | (f, _, _) <- importTargets ex]

extraField :: Text -> Value -> Maybe Value
extraField k (Object o) = KM.lookup (Key.fromText k) o
extraField _ _ = Nothing

-- | Every imports edge leaves the file's module node.
sourcesAreModule :: Extraction -> Expectation
sourcesAreModule ex =
  [ fmap toText (nodeKind =<< Map.lookup (edgeSource e) (extractionNodes ex))
  | e <- Map.elems (extractionEdges ex), edgeRelation e == Imports ]
    `shouldSatisfy` all (== Just "Module")

spec :: Spec
spec = describe "tree-sitter imports edges (Go, Rust, Ruby)" $ do
  describe "parseUseTree" $ do
    it "expands groups, self, globs and aliases" $
      map (\u -> (upSegs u, upGlob u, upAlias u)) (parseUseTree "crate::a::{self, b::C, d::*, e as f}")
        `shouldBe` [ (["crate", "a"], False, Nothing)
                   , (["crate", "a", "b", "C"], False, Nothing)
                   , (["crate", "a", "d"], True, Nothing)
                   , (["crate", "a", "e"], False, Just "f") ]
    it "keeps a leading :: and nests groups" $
      map (\u -> (upAbsolute u, upSegs u)) (parseUseTree "::x::{y::{z, w}, /* c */ v}")
        `shouldBe` [(True, ["x", "y", "z"]), (True, ["x", "y", "w"]), (True, ["x", "v"])]
    it "rejects text that is not a use tree" $
      parseUseTree "a::b; fn" `shouldBe` []

  describe "manifests" $ do
    it "reads the go.mod module path" $
      parseGoModule "// c\nmodule example.com/shop // x\n\ngo 1.22\n" `shouldBe` Just "example.com/shop"
    it "reads Cargo.toml package, lib and path/workspace deps" $ do
      let cm = parseCargoManifest $ T.unlines
            [ "[package]", "name = \"my-app\"", "[lib]", "name = \"mylib\""
            , "[dependencies]", "core-lib = { workspace = true }", "serde = \"1\""
            , "[target.'cfg(unix)'.dependencies.local-dep]", "path = \"../local\"" ]
      cmPackageName cm `shouldBe` Just "my-app"
      cmLibName cm `shouldBe` Just "mylib"
      Map.keys (cmDeps cm) `shouldBe` ["core_lib", "local_dep", "serde"]

  around (withSystemTempDirectory "graphos-imports") $ do
    it "Go: internal package → one edge per non-test file (weight 1/n), alias kept, stdlib external" $ \tmp -> do
      writeTree tmp
        [ ("go.mod", "module example.com/shop\n\ngo 1.22\n")
        , ("cmd/main.go", T.unlines
            [ "package main", "", "import (", "\t\"fmt\""
            , "\tm \"example.com/shop/internal/model\""
            , "\t\"example.com/shop/internal/store\"", "\t_ \"embed\"", ")"
            , "", "func main() { fmt.Println(m.Cart{}, store.New()) }" ])
        , ("internal/model/cart.go", "package model\n\ntype Cart struct{}\n")
        , ("internal/model/item.go", "package model\n\ntype Item struct{}\n")
        , ("internal/model/cart_test.go", "package model\n")
        , ("internal/store/store.go", "package store\n\nimport \"example.com/shop/internal/model\"\n\nfunc New() model.Cart { return model.Cart{} }\n")
        ]
      ex <- extractFile "go" (tmp </> "cmd/main.go")
      targetFiles ex `shouldBe`
        sort [ "external:embed", "external:fmt"
             , T.pack (tmp </> "internal/model/cart.go"), T.pack (tmp </> "internal/model/item.go")
             , T.pack (tmp </> "internal/store/store.go") ]
      let model = [ (w, x) | (f, w, x) <- importTargets ex, "internal/model/" `T.isInfixOf` f ]
      map fst model `shouldBe` [0.5, 0.5]
      map (extraField "alias" . snd) model `shouldBe` replicate 2 (Just (String "m"))
      map (extraField "package_files" . snd) model `shouldBe` replicate 2 (Just (Number 2))
      [ extraField "alias" x | (f, _, x) <- importTargets ex, f == "external:embed" ] `shouldBe` [Just (String "_")]
      sourcesAreModule ex
      store <- extractFile "go" (tmp </> "internal/store/store.go")
      targetFiles store `shouldBe` sort (map (T.pack . (tmp </>)) ["internal/model/cart.go", "internal/model/item.go"])

    it "Rust: crate/super/self/group/glob, workspace crate, fn-body use, repeated use" $ \tmp -> do
      writeTree tmp
        [ ("Cargo.toml", T.unlines
            [ "[workspace]", "members = [\"crates/*\"]", "", "[workspace.dependencies]"
            , "core-lib = { path = \"crates/core-lib\", version = \"0.1\" }", "serde = \"1\"" ])
        , ("crates/core-lib/Cargo.toml", "[package]\nname = \"core-lib\"\n\n[dependencies]\nserde = { workspace = true }\n")
        , ("crates/core-lib/src/lib.rs", "pub mod model;\npub mod util;\n")
        , ("crates/core-lib/src/util.rs", "pub mod fmt;\npub fn helper() {}\n")
        , ("crates/core-lib/src/util/fmt.rs", "use serde::Serialize;\npub fn render() {}\n")
        , ("crates/core-lib/src/model/mod.rs", "pub mod item;\nuse crate::util::{self, fmt::*};\npub enum Kind { A }\n")
        , ("crates/core-lib/src/model/item.rs", T.unlines
            [ "use std::fmt;", "use super::Kind;", "use crate::util::helper;", "pub struct Item;"
            , "fn build() {", "    use crate::util::fmt::render;", "    render();", "}"
            , "mod tests {", "    use super::*;", "    use crate::util::helper;", "}" ])
        , ("crates/app/Cargo.toml", "[package]\nname = \"app\"\n\n[dependencies]\ncore-lib = { workspace = true }\nanyhow = \"1\"\n")
        , ("crates/app/src/main.rs", T.unlines
            [ "mod cli;", "use core_lib::model::{item::Item, Kind};", "use anyhow::Result;"
            , "use cli::run;", "enum Color { Red }", "use Color::*;", "fn main() {}" ])
        , ("crates/app/src/cli.rs", "use ::core_lib::util;\npub fn run() {}\n")
        ]
      let f = T.pack . (tmp </>)
      item <- extractFile "rust" (tmp </> "crates/core-lib/src/model/item.rs")
      targetFiles item `shouldBe` sort
        [ "external:std", f "crates/core-lib/src/model/mod.rs", f "crates/core-lib/src/util.rs"
        , f "crates/core-lib/src/util/fmt.rs" ]
      sourcesAreModule item
      -- one Import node per occurrence: the repeated `use crate::util::helper;`
      -- (module level + mod tests) and the `use` inside fn build
      let imports = [ (toText (nodeLabel n), nodeLineStart n)
                    | n <- Map.elems (extractionNodes item), fmap toText (nodeKind n) == Just "Import" ]
      [l | (t, l) <- imports, t == "use crate::util::helper;"] `shouldMatchList` [Just 3, Just 11]
      [l | (t, l) <- imports, t == "use crate::util::fmt::render;"] `shouldBe` [Just 6]

      modrs <- extractFile "rust" (tmp </> "crates/core-lib/src/model/mod.rs")
      targetFiles modrs `shouldBe` sort [f "crates/core-lib/src/util.rs", f "crates/core-lib/src/util/fmt.rs"]

      mainrs <- extractFile "rust" (tmp </> "crates/app/src/main.rs")
      targetFiles mainrs `shouldBe` sort
        [ "external:anyhow", f "crates/app/src/cli.rs"
        , f "crates/core-lib/src/model/item.rs", f "crates/core-lib/src/model/mod.rs" ]
      cli <- extractFile "rust" (tmp </> "crates/app/src/cli.rs")
      targetFiles cli `shouldBe` [f "crates/core-lib/src/util.rs"]

    it "Ruby: require_relative, require of a lib/ file, gem; no junk targets" $ \tmp -> do
      writeTree tmp
        [ ("Gemfile", "source \"https://rubygems.org\"\n")
        , ("lib/shop/pricing.rb", "module Shop; module Pricing; end; end\n")
        , ("app/helpers/money.rb", "module Money; end\n")
        , ("app/models/cart.rb", T.unlines
            [ "require_relative \"../helpers/money\"", "require 'shop/pricing'", "require \"json\""
            , "require \"x/#{name}\"", "require File.join(\"a\", \"b\")", "require_relative \"missing\""
            , "class Cart", "  def total", "    require 'bigdecimal'", "  end", "end" ])
        ]
      ex <- extractFile "ruby" (tmp </> "app/models/cart.rb")
      targetFiles ex `shouldBe` sort
        [ "external:bigdecimal", "external:json"
        , T.pack (tmp </> "app/helpers/money.rb"), T.pack (tmp </> "lib/shop/pricing.rb") ]
      sourcesAreModule ex
      [ extraField "via" x | (t, _, x) <- importTargets ex, "money.rb" `T.isSuffixOf` t ]
        `shouldBe` [Just (String "require_relative")]

    it "leaves other grammars untouched" $ \tmp -> do
      writeTree tmp [("a.py", "import os\n")]
      ex <- extractFile "python" (tmp </> "a.py")
      importTargets ex `shouldBe` []
