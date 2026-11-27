module Graphos.UseCase.Extract.HaskellSpec where

import Test.Hspec
import Data.List (find, isInfixOf, nub, sortOn)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Text.Short (fromText, toText)
import Graphos.UseCase.Extract.Haskell
import Graphos.Domain.Types

-- | Expected declarations of @test-corpus/haskell-decls/Decls.hs@
-- (function-source-access 1.1): @(name, first line, last line, signature)@,
-- one row per top-level declaration of the fixture.
fixtureRows :: [(String, Int, Int, String)]
fixtureRows =
  [ ("verdict",   3,  4, "verdict :: Int -> Int")       -- signature + definition, blank line after
  , ("threshold", 6,  6, "threshold = 10")              -- the declaration after verdict
  , ("helper",    7,  7, "helper x = x * 2")            -- no type signature
  , ("connect",  10, 20, "connect :: String -> IO Int") -- multi-line signature, body with comments
  , ("Mode",     30, 33, "data Mode")                   -- multi-line data type
  , ("size",     37, 38, "size :: Mode -> Int")         -- after a doc comment and a pragma
  , ("classify", 40, 43, "classify :: Int -> Mode")     -- several clauses
  , ("Show",     45, 46, "instance Show Mode where")    -- instance with a body
  , ("render",   48, 50, "render :: Mode -> String")    -- clause whose body starts on the next line
  ]

-- | Names in the fixture that keep a node without a range: an indented
-- binding and a declaration inside a block comment.
fixtureUnranged :: [String]
fixtureUnranged = ["show", "commented"]

fixturePath :: FilePath
fixturePath = "test-corpus/haskell-decls/Decls.hs"

-- | Sanity check of declaration ranges (function-source-access 1.3): start <=
-- end, ranges of one file do not overlap, the first line contains the name.
-- Returns the names that fail.
rangeSanityFailures :: [String] -> [(String, Int, Int)] -> [String]
rangeSanityFailures srcLines ranges =
  [ name
  | (i, (name, s, e)) <- zip [0 :: Int ..] sorted
  , let previousEnd = if i == 0 then 0 else (\(_, _, pe) -> pe) (sorted !! (i - 1))
  , not (s <= e && s > previousEnd && s >= 1 && s <= length srcLines
         && name `isInfixOf` (srcLines !! (s - 1)))
  ]
  where sorted = sortOn (\(_, s, _) -> s) ranges

-- | Ranges carried by the declaration nodes of a file.
nodeRanges :: [Node] -> [(String, Int, Int)]
nodeRanges nodes =
  nub [ (T.unpack (toText (nodeLabel n)), s, e)
      | n <- nodes, nodeKind n /= Just "Module", Just s <- [nodeLineStart n], Just e <- [nodeLineEnd n] ]

spec :: Spec
spec = do
  describe "Haskell declaration ranges (function-source-access)" $ do
    it "the fixture yields one declaration per expected row" $ do
      src <- readFile fixturePath
      [ (hdName d, hdLineStart d, hdLineEnd d, hdSignature d) | d <- parseHaskellDecls src ]
        `shouldBe` fixtureRows

    it "the fixture's nodes carry the range and signature of their declaration" $ do
      src <- readFile fixturePath
      let nodes = haskellStubNodes "test-corpus/haskell-decls/Decls.hs" src
          byLabel l = find (\n -> nodeLabel n == fromText (T.pack l)) nodes
      mapM_ (\(name, s, e, sig) -> do
               let n = byLabel name
               (nodeLineStart =<< n) `shouldBe` Just s
               (nodeLineEnd =<< n) `shouldBe` Just e
               (fmap toText . nodeSignature =<< n) `shouldBe` Just (T.pack sig)
               fmap (isFieldPresent bitNodeSignature . nodePresentBits) n `shouldBe` Just True)
            fixtureRows
      mapM_ (\name -> (nodeLineStart =<< byLabel name) `shouldBe` Nothing) fixtureUnranged

    it "the fixture's ranges pass the sanity check" $ do
      src <- readFile fixturePath
      rangeSanityFailures (lines src) (nodeRanges (haskellStubNodes fixturePath src)) `shouldBe` []

    it "function with a type signature: lines 3-4, signature is the type" $ do
      let src = unlines
            [ "module A where", ""
            , "verdict :: Int -> Int"
            , "verdict x = helper x + 42"
            , ""
            , "other = 1" ]
      take 1 (parseHaskellDecls src) `shouldBe`
        [HaskellDecl "verdict" "Function" 3 4 "verdict :: Int -> Int"]

    it "multi-line type signature: joined by single spaces, range covers the definition" $ do
      let src = unlines $
            replicate 9 "" ++
            [ "connect :: String"        -- 10
            , "        -> Int"           -- 11
            , "        -> IO Int"        -- 12
            , "connect host port = do"   -- 13
            ] ++ replicate 6 "  pure port" ++  -- 14-19
            [ "  pure 0" ]               -- 20
      parseHaskellDecls src `shouldBe`
        [HaskellDecl "connect" "Function" 10 20 "connect :: String -> Int -> IO Int"]

    it "declaration without a type signature: one line, signature is the line" $ do
      let src = unlines (replicate 6 "" ++ ["helper x = x * 2", "", "-- trailing"])
      parseHaskellDecls src `shouldBe` [HaskellDecl "helper" "Function" 7 7 "helper x = x * 2"]

    it "data type declaration spans its constructors" $ do
      let src = unlines (replicate 29 "" ++ ["data Mode = Auto", "  | On", "  | Off", "  | Unknown", "", "f = 1"])
      take 1 (parseHaskellDecls src) `shouldBe` [HaskellDecl "Mode" "Type" 30 33 "data Mode = Auto"]

    it "a comment or pragma documenting the next declaration does not extend the previous one" $ do
      let src = unlines
            [ "f = 1", "", "-- | Doc of g.", "{-# INLINE g #-}", "g :: Int", "g = 2" ]
      map (\d -> (hdName d, hdLineStart d, hdLineEnd d)) (parseHaskellDecls src)
        `shouldBe` [("f", 1, 1), ("g", 5, 6)]

    it "ids are stable: node ids and edges are those of the per-line declaration names" $ do
      src <- readFile fixturePath
      let fp = "src/Decls.hs"
          nodes = haskellStubNodes fp src
          ex = extractionFromLists nodes (haskellStubEdges fp nodes)
          prefix = concat (take 1 [ takeWhile (/= '_') (T.unpack (nodeId n)) | n <- nodes, nodeLabel n == "verdict" ])
          -- The rule used before declarations carried positions: one name per
          -- matching line, id = directory hash + name.
          legacyNames = [ nm | l <- lines src, isTopLevelDecl l, Just nm <- [extractDeclName l] ]
          legacyDeclIds = Set.fromList [ T.pack (prefix ++ "_" ++ nm) | nm <- legacyNames ]
          declIds = Set.fromList [ nodeId n | n <- Map.elems (extractionNodes ex), nodeKind n /= Just "Module" ]
          containsTargets = Set.fromList [ edgeTarget e | e <- Map.elems (extractionEdges ex), edgeRelation e == Contains ]
      declIds `shouldBe` legacyDeclIds
      containsTargets `shouldBe` legacyDeclIds
      map fst (parseHaskellDeclNames src) `shouldBe` legacyNames

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
            , extractionErrors = []
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
            , extractionErrors = []
            }
          mainNodes = filter (\n -> nodeLabel n == fromText "Main") (Map.elems $ extractionNodes ex)
      length mainNodes `shouldBe` 2
      length (nub $ map nodeId mainNodes) `shouldBe` 2
