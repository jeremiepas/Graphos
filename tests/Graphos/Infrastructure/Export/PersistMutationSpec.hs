-- | Persistence round-trip for Cypher mutations
-- (opencypher-write-mutations): persist a mutated graph to graph.json,
-- verify the backup exists, schema_version is preserved, and the
-- mutations are visible after a reload through the standard reader.
module Graphos.Infrastructure.Export.PersistMutationSpec where

import Test.Hspec
import System.Directory (doesFileExist)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.Aeson (ToJSON(..), encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Short (fromText)
import Data.List (isInfixOf)

import Graphos.Domain.Types
  ( Node(..), Edge(..), EdgeId(..), FileType(CodeFile)
  , Relation(Calls), Confidence(..)
  )
import Graphos.Domain.Graph (gNodes)
import Graphos.Domain.Graph.Mutation (MutationSummary(..))
import Graphos.Domain.Query.Cypher.Parser (parseStatement)
import Graphos.Domain.Query.Cypher.AST (CypherStatement(..))
import Graphos.Domain.Query.Cypher.Eval (evaluateStatement, MutationResult(..))
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import Graphos.Infrastructure.Export.PersistMutation (persistMutatedGraph)

mkNode :: Text -> Text -> Text -> Node
mkNode nid kind srcFile = Node
  { nodeId          = nid
  , nodeLabel       = fromText nid
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText srcFile
  , nodeLineStart   = Just 1
  , nodeLineEnd     = Just 10
  , nodeSignature   = Nothing
  , nodeCommunityId = Nothing
  , nodeKind        = Just (fromText kind)
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodePresentBits = 0
  }

mkEdge :: Text -> Text -> Text -> Relation -> Edge
mkEdge eid src tgt rel = Edge
  { edgeId         = EdgeId eid
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = rel
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

-- | A schema-versioned baseline document matching the fixture graph.
baselineJson :: String
baselineJson =
  "{\"schema_version\":\"1\",\"nodes\":"
  <> jsonList [toJSON (mkNode "n1" "Function" "src/a.hs"), toJSON (mkNode "n2" "Function" "src/b.hs")]
  <> ",\"edges\":"
  <> jsonList [toJSON (mkEdge "e1" "n1" "n2" Calls)]
  <> "}"
  where
    jsonList xs = T.unpack (decodeUtf8 (BSL.toStrict (BSL.concat (lbrack : go xs ++ [rbrack]))))
    lbrack = BSL.fromStrict "["
    rbrack = BSL.fromStrict "]"
    go []     = []
    go (x:xs) = encode x : concatMap (\v -> [BSL.fromStrict ",", encode v]) xs

spec :: Spec
spec = describe "persistMutatedGraph" $ do
  it "persists the mutated graph with a backup and reloads with mutations" $ do
    withSystemTempDirectory "graphos-persistmut" $ \dir -> do
      let path = dir </> "graph.json"
      writeFile path baselineJson
      lr0 <- loadGraphFromFile path
      case lr0 of
        Left e -> fail (T.unpack e)
        Right loaded -> do
          let st = case parseStatement "CREATE (m:Module {id: 'm9'})" of
                Right (MutStatement m) -> MutStatement m
                _ -> error "fixture parse failure"
          case evaluateStatement 100 st (lrGraph loaded) (lrIndex loaded) of
            Left e -> fail (T.unpack e)
            Right mr -> do
              msNodesCreated (mrSummary mr) `shouldBe` 1
              res <- persistMutatedGraph path loaded (mrGraph mr)
              case res of
                Left e -> fail (T.unpack e)
                Right backup -> do
                  backupExists <- doesFileExist backup
                  backupExists `shouldBe` True
                  backupContent <- readFile backup
                  backupContent `shouldBe` baselineJson
                  persisted <- readFile path
                  persisted `shouldSatisfy` isInfixOf "m9"
                  persisted `shouldSatisfy` isInfixOf "\"schema_version\""
                  lr1 <- loadGraphFromFile path
                  case lr1 of
                    Left e -> fail (T.unpack e)
                    Right reloaded ->
                      Map.member "m9" (gNodes (lrGraph reloaded)) `shouldBe` True