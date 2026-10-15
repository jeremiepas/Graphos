{-# LANGUAGE StrictData #-}
-- | Deterministic @documents@ linking passes.
--
-- These three passes connect documentation nodes to code nodes using signals
-- that are reproducible by construction (no embeddings, no LLM):
--
--  * 'inferCoLocationEdges' links a doc to code in the same directory subtree.
--  * 'inferSymbolMentionEdges' links a doc to the unique definition of an
--    identifier mentioned in its text.
--  * 'inferPathReferenceEdges' links a doc to the code node whose source file
--    matches a repository-relative path cited in its text.
--
-- Every edge produced here uses the 'Documents' relation with 'documentsConfidence',
-- which is distinct from similarity-based 'Inferred' edges and (per the semantic
-- filter) survives @edges = semantic@ regardless of confidence.
module Graphos.UseCase.Infer.Document
  ( inferCoLocationEdges
  , inferSymbolMentionEdges
  , inferPathReferenceEdges
  , inferSymbolMentionEdgesWith
  , inferPathReferenceEdgesWith
  , documentsConfidence
  , minIdentLength
  , pathExtensions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (nub, sort, sortOn, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (toText)
import Data.Char (isAlphaNum)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Analysis (dedupOn)

-- | High confidence for deterministic documents edges. The semantic filter
-- (FormatContext) drops @confidence < 0.7@; 'documentsConfidence' sits well
-- above that so these edges are never filtered as ambiguous.
documentsConfidence :: Double
documentsConfidence = 0.9

-- | Minimum whole-word identifier length for the symbol-mention pass. Identifiers
-- shorter than this ("the", "set") are common noise and are skipped.
minIdentLength :: Int
minIdentLength = 4

-- | File extensions treated as known source paths for the path-reference pass.
-- A cited token must end with one of these to be considered a file reference.
pathExtensions :: [Text]
pathExtensions =
  [ ".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"
  , ".hs", ".lhs"
  , ".rs", ".py", ".pyx"
  , ".go", ".java", ".kt", ".scala", ".sc", ".kts"
  , ".rb", ".php"
  , ".cpp", ".cc", ".cxx", ".c++", ".hpp", ".hh", ".h"
  , ".cs", ".swift", ".m", ".mm"
  , ".ml", ".mli", ".ex", ".exs", ".erl", ".clj"
  , ".sh", ".bash", ".yaml", ".yml", ".toml", ".json", ".md", ".markdown"
  ]

-- | Build a documents edge from a documentation node to a code node.
documentEdge :: NodeId -> NodeId -> Edge
documentEdge docNid codeNid =
  Edge
    { edgeId        = EdgeId (docNid <> "->" <> codeNid <> ":" <> relationToText Documents)
    , edgeSource    = docNid
    , edgeTarget    = codeNid
    , edgeRelation  = Documents
    , edgeWeight    = documentsConfidence
    , edgeConfidence = Confidence documentsConfidence
    , edgeExtra     = Nothing
    }

-- | A documentation node paired with its id, for iterating over docs.
type DocNode = (NodeId, Node)

-- | A code node paired with its id.
type CodeNode = (NodeId, Node)

isDoc :: Node -> Bool
isDoc n = nodeFileType n == DocFile

isCode :: Node -> Bool
isCode n = nodeFileType n == CodeFile

-- ────────────────────────────────────────────────────────────────────────────
-- Path / directory helpers
-- ────────────────────────────────────────────────────────────────────────────

-- | Directory components of a repo-relative path (the filename dropped).
--
-- > pathDir "a/b/src/lib.rs" == ["a", "b", "src"]
-- > pathDir "README.md"      == []
pathDir :: Text -> [Text]
pathDir p =
  let comps = T.split (== '/') p
  in filter (not . T.null) (init comps)

-- | Normalise a cited path so it compares equal to a node's 'nodeSourceFile':
-- drop a leading @./@ and any trailing slash.
normalizePath :: Text -> Text
normalizePath p =
  let noTrailing = T.dropWhile (== '/') p
      noDotSlash = if T.isPrefixOf "./" noTrailing then T.drop 2 noTrailing else noTrailing
  in noDotSlash

-- | True when @codeDir@ lies in @docDir@'s directory subtree (same directory or
-- a descendant). A root-level doc (@docDir@ empty) never co-locates across
-- subtrees, which keeps a top-level README from linking to arbitrary code.
sameSubtree :: [Text] -> [Text] -> Bool
sameSubtree docDir codeDir = not (null docDir) && docDir `isPrefixOf` codeDir

-- ────────────────────────────────────────────────────────────────────────────
-- Index builders
-- ────────────────────────────────────────────────────────────────────────────

-- | Map of a code-node label to the ids of nodes defining it. Labels are kept
-- case-sensitive: identifiers in code are case-sensitive, so @Config@ and
-- @config@ are distinct definitions.
buildDefIndex :: Graph -> Map Text [NodeId]
buildDefIndex g =
  Map.fromListWith (++)
    [ (toText (nodeLabel n), [nid])
    | (nid, n) <- Map.toList (gNodes g)
    , isCode n
    , let lab = toText (nodeLabel n)
    , not (T.null lab)
    ]

-- | Map of a normalised source file path to the ids of nodes for that file.
buildPathIndex :: Graph -> Map Text [NodeId]
buildPathIndex g =
  Map.fromListWith (++)
    [ (normalizePath (toText (nodeSourceFile n)), [nid])
    | (nid, n) <- Map.toList (gNodes g)
    , isCode n
    , let sf = toText (nodeSourceFile n)
    , not (T.null sf)
    ]

-- | Documentation nodes of the graph, filtered by file type.
docNodes :: Graph -> [DocNode]
docNodes g = [ (nid, n) | (nid, n) <- Map.toList (gNodes g), isDoc n ]

codeNodes :: Graph -> [CodeNode]
codeNodes g = [ (nid, n) | (nid, n) <- Map.toList (gNodes g), isCode n ]

-- | Doc-body text index extracted from @nodeExtra.doc_body@, which the
-- markdown extractor stores on every doc file node. The explicit @Map NodeId
-- Text@ argument (tests, ingest) overrides the node-borne text.
docBodyIndex :: Graph -> Map NodeId Text
docBodyIndex g =
  Map.fromList
    [ (nid, body)
    | (nid, n) <- Map.toList (gNodes g)
    , isDoc n
    , Just (Object km) <- [nodeExtra n]
    , Just (String body) <- [KM.lookup (Key.fromText "doc_body") km]
    ]

-- ────────────────────────────────────────────────────────────────────────────
-- Tokenisers
-- ────────────────────────────────────────────────────────────────────────────

-- | Split text into maximal runs of characters matching predicate @p@,
-- dropping empty runs. Implemented over 'Data.List.span' (well-defined argument
-- order) rather than 'Data.Text.span' (whose order varies across package
-- versions).
tokenise :: (Char -> Bool) -> Text -> [Text]
tokenise p = go . T.unpack
  where
    go [] = []
    go s =
      let (run, rest) = span p s
      in if null run
           then go (drop 1 s)
           else T.pack run : go rest

-- | Whole-word identifiers in a text: maximal runs of @A-Za-z0-9_@ that are at
-- least the given minimum length, de-duplicated and stable-sorted.
identifiersWith :: Int -> Text -> [Text]
identifiersWith minLen text = sort . nub $
  [ run | run <- tokenise isIdentChar text, T.length run >= minLen ]
  where
    isIdentChar c = isAlphaNum c || c == '_'

-- | Repository-relative file paths cited in a text: maximal runs of path-safe
-- characters that contain a @/@ and end with a known extension, de-duplicated
-- and stable-sorted. Bare filenames (@index.ts@, no @/@) are rejected here.
-- Trailing sentence punctuation on a cited path (e.g. @src/x.ts.@) is stripped,
-- mirroring the Lean model's @dropTrailingPunct@.
citedPathsWith :: [Text] -> Text -> [Text]
citedPathsWith exts text = sort . nub $
  [ run | raw <- tokenise isPathChar text, run <- [stripTrailingPunct raw], isValid run ]
  where
    isValid run = T.elem '/' run && any (\ext -> T.isSuffixOf ext run) exts
    isPathChar c =
      c == '/' || c == '.' || c == '-' || c == '_' || isAlphaNum c
    stripTrailingPunct t =
      if T.null t then t
      else case T.last t of
        c | c `elem` (". , ) ; : ' \"" :: String) -> stripTrailingPunct (T.dropEnd 1 t)
        _ -> t

-- ────────────────────────────────────────────────────────────────────────────
-- Passes
-- ────────────────────────────────────────────────────────────────────────────

-- | Co-location pass: a doc links to every code node in the same directory
-- subtree (same directory or a descendant). Root-level docs do not participate.
inferCoLocationEdges :: Graph -> [Edge]
inferCoLocationEdges g =
  let docCodeEdges =
        [ documentEdge docNid codeNid
        | (docNid, dn) <- docNodes g
        , docDir <- [pathDir (toText (nodeSourceFile dn))]
        , (codeNid, cn) <- codeNodes g
        , codeDir <- [pathDir (toText (nodeSourceFile cn))]
        , sameSubtree docDir codeDir
        ]
  in sortOn edgeSortKey docCodeEdges

-- | Symbol-mention pass: a doc links to the unique node defining an identifier
-- mentioned in its text. Identifiers defined by more than one node are ambiguous
-- and skipped; identifiers defined by no node are ignored.
--
-- Text sources are merged: node-borne @nodeExtra.doc_body@ (pipeline path)
-- plus the explicit index argument (tests/ingest); the argument wins per doc.
inferSymbolMentionEdges :: Map NodeId Text -> Graph -> [Edge]
inferSymbolMentionEdges = inferSymbolMentionEdgesWith minIdentLength

-- | Symbol-mention pass with a configurable minimum identifier length
-- (doc-code-linking threshold, @semantic_edges.min_ident_length@).
inferSymbolMentionEdgesWith :: Int -> Map NodeId Text -> Graph -> [Edge]
inferSymbolMentionEdgesWith minLen docText g =
  let defIdx = buildDefIndex g
      texts = Map.union docText (docBodyIndex g)
      edges =
         [ documentEdge docNid defNid
         | (docNid, _) <- docNodes g
         , Just text <- [Map.lookup docNid texts]
         , ident <- identifiersWith minLen text
         , defNids <- [Map.findWithDefault [] ident defIdx]
         , length defNids == 1
         , defNid <- defNids
         ]
  in dedupOn (\e -> (edgeSource e, edgeTarget e)) edges

-- | Path-reference pass: a doc links to the code node whose normalised source
-- file matches a repository-relative path cited in its text. Dangling paths
-- (no matching code node) and bare filenames are skipped. Cross-subtree links
-- are allowed.
--
-- Text sources are merged as in 'inferSymbolMentionEdges'.
inferPathReferenceEdges :: Map NodeId Text -> Graph -> [Edge]
inferPathReferenceEdges = inferPathReferenceEdgesWith pathExtensions

-- | Path-reference pass with configurable accepted source-path extensions
-- (doc-code-linking threshold, @semantic_edges.path_extensions@).
inferPathReferenceEdgesWith :: [Text] -> Map NodeId Text -> Graph -> [Edge]
inferPathReferenceEdgesWith exts docText g =
  let pathIdx = buildPathIndex g
      texts = Map.union docText (docBodyIndex g)
      edges =
         [ documentEdge docNid codeNid
         | (docNid, _) <- docNodes g
         , Just text <- [Map.lookup docNid texts]
         , path <- citedPathsWith exts text
         , codeNids <- [Map.findWithDefault [] (normalizePath path) pathIdx]
         , not (null codeNids)
         , codeNid <- codeNids
         ]
  in dedupOn (\e -> (edgeSource e, edgeTarget e)) edges

-- | Canonical ordering key for deterministic edge output.
edgeSortKey :: Edge -> (NodeId, NodeId, Relation)
edgeSortKey e = (edgeSource e, edgeTarget e, edgeRelation e)
