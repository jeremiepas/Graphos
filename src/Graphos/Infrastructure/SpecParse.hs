-- | Deterministic structural parser for openspec artifacts
-- (spec-artifact-schema capability, requirement "Hybrid extraction").
--
-- This is the no-model layer: node identity, spans and structural edges come
-- from the document format alone — requirement headers, scenario blocks, and
-- backtick cross-references to known capability names. The small-model
-- semantic pass (satisfies/constrains/refines/conflicts_with) is a separate,
-- later stage; with the provider disabled this parser still yields the full
-- structure (spec scenario "Structure needs no model").
module Graphos.Infrastructure.SpecParse
  ( parseSpecDir
  , parseSpecFile
  ) where

import Control.Monad (filterM, forM)
import Data.Char (isAlphaNum, toLower)
import Data.List (isPrefixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText, fromText)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeDirectory, takeFileName)

import Graphos.Domain.Types

-- | Walk an openspec root, parse every spec.md, and link cross-references
-- between capabilities. Returns spec-artifact nodes and structural edges.
parseSpecDir :: FilePath -> IO (Either Text ([Node], [Edge]))
parseSpecDir root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure (Left ("no such directory: " <> T.pack root))
    else do
      files <- findSpecFiles root
      if null files
        then pure (Left ("no spec.md files under " <> T.pack root))
        else do
          parsed <- forM files $ \fp -> do
            content <- T.pack <$> readFile fp
            let (cap, ns, es) = parseSpecFile fp content
            pure (cap, content, ns, es)
          let caps = nub [ c | (c, _, _, _) <- parsed ]
              nodes = concat [ ns | (_, _, ns, _) <- parsed ]
              edges = concat [ es | (_, _, _, es) <- parsed ]
              refs = crossReferences caps [ (c, body) | (c, body, _, _) <- parsed ]
          pure (Right (nodes, edges ++ refs))

-- | Recursively collect **/spec.md (skipping the archive).
findSpecFiles :: FilePath -> IO [FilePath]
findSpecFiles dir = do
  entries <- listDirectory dir
  let paths = [ dir </> e | e <- entries, e /= "archive", not ("." `isPrefixOf` e) ]
  dirs <- filterM doesDirectoryExist paths
  let files = [ p | p <- paths, takeFileName p == "spec.md" ]
  rest <- concat <$> mapM findSpecFiles dirs
  pure (files ++ rest)

-- | Parse one spec.md into (capability name, nodes, structural edges).
parseSpecFile :: FilePath -> Text -> (Text, [Node], [Edge])
parseSpecFile fp content =
  let capability = T.pack (takeFileName (takeDirectory fp))
      capId = "cap:" <> capability
      ls = zip [1 :: Int ..] (T.lines content)
      go _ acc [] = acc
      go current acc ((i, line) : rest)
        | Just title <- T.stripPrefix "### Requirement: " line =
            let rid = "req:" <> capability <> "/" <> slug title
                node = mkNode rid title "Requirement" fp i
                edge = mkEdge capId Contains rid
            in go (Just rid) (addNode node (addEdge edge acc)) rest
        | Just title <- T.stripPrefix "#### Scenario: " line =
            case current of
              Just rid ->
                let sid = rid <> "/scn:" <> slug title
                    node = mkNode sid title "Scenario" fp i
                    edge = mkEdge rid Contains sid
                in go current (addNode node (addEdge edge acc)) rest
              Nothing -> go current acc rest
        | otherwise = go current acc rest
      capNode = mkNode capId capability "Capability" fp 1
      (nodes, edges) = go Nothing ([], []) ls
  in (capability, capNode : reverse nodes, reverse edges)
  where
    addNode n (ns, es) = (n : ns, es)
    addEdge e (ns, es) = (ns, e : es)

-- | Backtick mentions of other capability names become references edges
-- between capabilities — deterministic cross-linking, no model.
crossReferences :: [Text] -> [(Text, Text)] -> [Edge]
crossReferences caps files = nub
  [ mkEdge ("cap:" <> src) References ("cap:" <> cap)
  | (src, body) <- files
  , cap <- caps
  , cap /= src
  , ("`" <> cap <> "`") `T.isInfixOf` body
  ]

slug :: Text -> Text
slug = T.pack . map (\c -> if isAlphaNum c then toLower c else '-') . T.unpack

mkNode :: Text -> Text -> Text -> FilePath -> Int -> Node
mkNode nid label kind fp line =
  let n = Node
        { nodeId = nid
        , nodeLabel = fromText label
        , nodeFileType = DocFile
        , nodeSourceFile = fromText (T.pack fp)
        , nodeLineStart = Just line
        , nodeLineEnd = Nothing
        , nodeSignature = Nothing
        , nodeCommunityId = Nothing
        , nodeKind = Just (fromText kind :: ShortText)
        , nodeDegree = Nothing
        , nodeIsBridge = Nothing
        , nodeExtra = Nothing
        , nodePresentBits = 0
        }
  in n { nodePresentBits = computePresentBits n }

mkEdge :: Text -> Relation -> Text -> Edge
mkEdge src rel tgt = Edge
  { edgeId = EdgeId (src <> "|" <> relationToText rel <> "|" <> tgt)
  , edgeSource = src
  , edgeTarget = tgt
  , edgeRelation = rel
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }
