module Graphos.UseCase.Extract.HaskellSpec where

import Test.Hspec
import Data.List (find, nub)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Text.Short (fromText, toText)
import Graphos.UseCase.Extract.Haskell
import Graphos.Domain.Types

spec :: Spec
spec = do
  describe "Haskell stub extraction" $ do
    it "parses the module name" $ do
      parseHaskellModule "module Foo.Bar\n" `shouldBe` Just "Foo.Bar"

    it "parses qualified import names" $ do
      parseHaskellImports "import qualified Data.Map as M\n" `shouldBe` ["Data.Map"]

    it "emits no junk nodes for guard/brace/indented lines" $ do
      let src = unlines
            [ "module Foo where"
            , "f x | x > 0 = 1"
            , "    | otherwise = 0"
            , "g = let y = 1 in y"
            , "  where"
            , "    helper = 2"
            , "h = case x of { 1 -> True }"
            , "  "
            ]
          nodes = haskellStubNodes "src/Foo.hs" src
          decls = filter (\n -> nodeKind n `notElem` [Just (fromText "Module")]) nodes
      decls `shouldSatisfy` all (\n -> not (T.null (toText (nodeLabel n))))
      any (\n -> nodeLabel n == "|" || nodeLabel n == "}" || nodeLabel n == "where") decls `shouldBe` False

    it "does not emit 20-char truncated fragment labels" $ do
      let src = unlines
            [ "module Foo where"
            , "\"  - from: \" ++ T.unpack x"
            , "( NodeId, CommunityId )"
            ]
          nodes = haskellStubNodes "src/Foo.hs" src
          labels = map nodeLabel nodes
      any (`elem` labels) [fromText "\"  - from: \" ++ T.un", fromText "( NodeId, CommunityI"] `shouldBe` False

    it "assigns kinds by declaration form" $ do
      let src = unlines
            [ "module Foo where"
            , "data FooType = FooCons"
            , "newtype Bar = Bar Int"
            , "type Baz = Int"
            , "class Quux a where"
            , "instance Quux Int"
            , "func :: Int -> Int"
            , "func x = x"
            ]
          nodes = haskellStubNodes "src/Foo.hs" src
          kindOf lbl = nodeKind <$> find (\n -> nodeLabel n == fromText lbl) nodes
      kindOf "FooType" `shouldBe` Just (Just (fromText "Type"))
      kindOf "Bar" `shouldBe` Just (Just (fromText "Type"))
      kindOf "Baz" `shouldBe` Just (Just (fromText "Type"))
      kindOf "Quux" `shouldBe` Just (Just (fromText "Class"))
      kindOf "Int" `shouldSatisfy` (/= Just (Just (fromText "Type")))
      kindOf "func" `shouldBe` Just (Just (fromText "Function"))

    it "emits only imports edges from imports and contains edges from declarations" $ do
      let src = unlines
            [ "module Foo where"
            , "import Data.Map"
            , "f :: Int -> Int"
            , "f x = x"
            ]
          nodes = haskellStubNodes "src/Foo.hs" src
          ex = extractionFromLists nodes (haskellStubEdges "src/Foo.hs" nodes)
          rels = map edgeRelation (Map.elems (extractionEdges ex))
      length (filter (== Imports) rels) `shouldBe` 1
      length (filter (== Contains) rels) `shouldBe` 1

    it "shares module IDs across files for cross-file imports" $ do
      let srcA = unlines [ "module Graphos.Config where", "value = 1" ]
          srcB = unlines [ "module Graphos.App where", "import Graphos.Config", "main = pure ()" ]
          nodesA = haskellStubNodes "src/Graphos/Config.hs" srcA
          nodesB = haskellStubNodes "src/Graphos/App.hs" srcB
          exA = extractionFromLists nodesA (haskellStubEdges "src/Graphos/Config.hs" nodesA)
          exB = extractionFromLists nodesB (haskellStubEdges "src/Graphos/App.hs" nodesB)
          ex = Extraction
            { extractionNodes = extractionNodes exA `Map.union` extractionNodes exB
            , extractionEdges = extractionEdges exA `Map.union` extractionEdges exB
            }
          configNodeId = nodeId $ NE.head $ NE.fromList $ filter (\n -> nodeLabel n == fromText "Graphos.Config") (Map.elems $ extractionNodes ex)
          importEdges = filter (\e -> edgeRelation e == Imports) (Map.elems $ extractionEdges ex)
      any (\e -> edgeTarget e == configNodeId) importEdges `shouldBe` True

    it "keeps two Main module nodes distinct" $ do
      let srcA = unlines [ "module Main where", "main = pure ()" ]
          srcB = unlines [ "module Main where", "main = pure ()" ]
          exA = extractionFromLists (haskellStubNodes "app/A/Main.hs" srcA) (haskellStubEdges "app/A/Main.hs" (haskellStubNodes "app/A/Main.hs" srcA))
          exB = extractionFromLists (haskellStubNodes "app/B/Main.hs" srcB) (haskellStubEdges "app/B/Main.hs" (haskellStubNodes "app/B/Main.hs" srcB))
          ex = Extraction
            { extractionNodes = extractionNodes exA `Map.union` extractionNodes exB
            , extractionEdges = extractionEdges exA `Map.union` extractionEdges exB
            }
          mainNodes = filter (\n -> nodeLabel n == fromText "Main") (Map.elems $ extractionNodes ex)
      length mainNodes `shouldBe` 2
      length (nub $ map nodeId mainNodes) `shouldBe` 2
