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
  , parseAdrDir
  , parseAdrFile
  , parseSpecCorpus
  ) where

import Control.Monad (filterM, forM)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
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

-- ── ADR files (decision records) ─────────────────────────────────────────────

-- | Parse every @*.md@ ADR under @dir@ (non-recursive). ADR identity comes
-- from the filename (@adr-007-….md@ → @adr-007@); status and supersession
-- come from the @**Status**: …@ line, so node existence is never modelled.
parseAdrDir :: FilePath -> IO (Either Text ([Node], [Edge]))
parseAdrDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Left ("no such directory: " <> T.pack dir))
    else do
      entries <- listDirectory dir
      let adrs = [ dir </> e
                 | e <- entries
                 , ".md" `T.isSuffixOf` T.pack e
                 , isAdrName (T.pack e)
                 ]
      if null adrs
        then pure (Left ("no ADR files under " <> T.pack dir))
        else do
          (nodes, edgess) <- unzip <$> forM adrs (\fp -> do
            content <- T.pack <$> readFile fp
            pure (parseAdrFile fp content))
          pure (Right (concat nodes, concat edgess))

-- | A filename is an ADR when it starts with @adr-@ or has a >=3-digit
-- numeric lead (classic MADR numbering, e.g. @0007-foo.md@).
isAdrName :: Text -> Bool
isAdrName name =
  ("adr-" `T.isPrefixOf` base) || (T.length (T.takeWhile isDigit base) >= 3)
  where
    base = T.toLower (T.pack (takeFileName (T.unpack name)))
    isDigit c = c >= '0' && c <= '9'

-- | Parse one ADR markdown file into (Decision node, structural edges).
--
-- * The file itself becomes a @Decision@ node (id @adr-NNN@), with an
--   @active@ flag in @extra@: @false@ only when the status line reads
--   @Superseded@ (case-insensitive).
-- * A @**Supersedes**: ADR-NNN@ line yields @supersedes@ edges from this
--   decision to the named predecessors (Decision nodes referenced by id).
parseAdrFile :: FilePath -> Text -> ([Node], [Edge])
parseAdrFile fp content =
  let ls = zip [1 :: Int ..] (T.lines content)
      title = case [ T.stripPrefix "# " l | (_, l) <- ls ] of
        (Just t : _) -> T.strip t
        _            -> T.pack (takeFileName fp)
      baseName = T.toLower (T.pack (takeFileName (takeBaseName' fp)))
      adrId = case T.stripPrefix "adr-" baseName of
        Just rest | not (T.null (T.takeWhile (/= '-') rest)) ->
          T.takeWhile (/= '-') rest
        _ -> baseName
      statusLine = firstMatch [ T.stripPrefix "**Status**:" l >>= pure . T.strip | (_, l) <- ls ]
      supersedesLine = firstMatch [ T.stripPrefix "**Supersedes**:" l >>= pure . T.strip | (_, l) <- ls ]
      supersededTargets =
        [ "adr:" <> normalizeAdr (takeWord (T.stripStart t))
        | Just body <- [supersedesLine]
        , t <- T.splitOn "," body
        , not (T.null (takeWord (T.stripStart t)))
        ]
      node = mkDecision adrId title fp statusLine
      edges = [ mkEdge ("adr:" <> adrId) Supersedes tgt | tgt <- supersededTargets ]
  in ([node], edges)
  where
    firstMatch xs = case [ x | Just x <- xs ] of
      (x : _) -> Just x
      []      -> Nothing
    takeWord t = T.takeWhile (\c -> c /= ' ' && c /= '*' && c /= '`' && c /= '.') t
    -- "ADR-004" → "004" (same id derivation as the file's own name)
    normalizeAdr t = case T.stripPrefix "adr-" (T.toLower t) of
      Just rest | not (T.null (T.takeWhile (/= '-') rest)) ->
        T.takeWhile (/= '-') rest
      _ -> T.toLower t
    takeBaseName' p = T.unpack . T.pack . fst . break (== '.') $ takeFileName p

-- | Full spec corpus: openspec @spec.md@ files plus ADRs under
-- @\<root\>/../docs/proposals@ (or any explicit ADR directory), cross-linked.
-- Falls back to specs-only when no ADR directory exists.
parseSpecCorpus :: FilePath -> IO (Either Text ([Node], [Edge]))
parseSpecCorpus openspecRoot = do
  specs <- parseSpecDir openspecRoot
  case specs of
    Left err -> pure (Left err)
    Right (specNodes, specEdges) -> do
      let repoRoot = takeDirectory openspecRoot
          adrDirs = [ repoRoot </> "docs" </> "proposals"
                    , repoRoot </> "docs" </> "adr"
                    , openspecRoot </> "adrs"
                    ]
      adrResults <- mapM parseAdrDirIfExists adrDirs
      let (adrNodes, adrEdges) = concatPairs [ r | Just r <- adrResults ]
      pure (Right (specNodes ++ adrNodes, specEdges ++ adrEdges))
  where
    parseAdrDirIfExists d = do
      exists <- doesDirectoryExist d
      if not exists then pure Nothing else either (const Nothing) Just <$> parseAdrDir d
    concatPairs :: [([a], [b])] -> ([a], [b])
    concatPairs xs = (concat [ a | (a, _) <- xs ], concat [ b | (_, b) <- xs ])

-- | ADR Decision node: @extra@ carries the raw status text plus the
-- @active@ boolean (false iff status is Superseded).
mkDecision :: Text -> Text -> FilePath -> Maybe Text -> Node
mkDecision nid title fp mstatus =
  let active = maybe True (\s -> not ("superseded" `T.isPrefixOf` T.toLower s)) mstatus
      extraVal = A.toJSON (KM.fromList ([ ("active", A.toJSON active) ]
        ++ [ ("status", A.toJSON s) | Just s <- [mstatus] ] :: [(A.Key, A.Value)]))
      n = Node
        { nodeId = "adr:" <> nid
        , nodeLabel = fromText title
        , nodeFileType = DocFile
        , nodeSourceFile = fromText (T.pack fp)
        , nodeLineStart = Just 1
        , nodeLineEnd = Nothing
        , nodeSignature = Nothing
        , nodeCommunityId = Nothing
        , nodeKind = Just (fromText "Decision" :: ShortText)
        , nodeDegree = Nothing
        , nodeIsBridge = Nothing
        , nodeExtra = Just extraVal
        , nodePresentBits = 0
        }
  in n { nodePresentBits = computePresentBits n }

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
