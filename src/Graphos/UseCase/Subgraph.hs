{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Pure subgraph extraction for the path/taxonomy-driven @graphos subgraph@
-- command. Complements @research-view@ (query-term-driven) by selecting *core*
-- files from path patterns grouped into named subsystems, expanding a *boundary*
-- tier of files that import a core file or are imported by one, and an *external*
-- tier of package dependencies.
--
-- The module is pure (no IO): every node in the result carries its tier,
-- subsystem and architectural layer; every edge carries a provenance marker
-- distinguishing edges taken from the source graph from edges derived from
-- @Import@-kind nodes when the source graph lacks real @imports@ edges.
module Graphos.UseCase.Subgraph
  ( extractSubgraph
  , SubgraphConfig(..)
  , SubsystemConfig(..)
  , SubgraphTier(..)
  , EdgeProvenance(..)
  , architecturalLayer
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:), (.:?), (.!=), withObject, withText)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Control.Monad (msum)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import GHC.Generics (Generic)
import System.FilePath (takeDirectory, takeExtension, dropExtension, normalise, (</>))

import Graphos.Domain.Graph.Index (matchGlob)
import Graphos.Domain.Types
import Graphos.Domain.Types.Graph (LabeledGraph(..))

-- | Tier classification for nodes in a subgraph.
data SubgraphTier = CoreTier | BoundaryTier | ExternalTier
  deriving (Eq, Show, Generic, Ord)

instance ToJSON SubgraphTier where
  toJSON = \case
    CoreTier     -> "core"
    BoundaryTier -> "boundary"
    ExternalTier -> "external"

instance FromJSON SubgraphTier where
  parseJSON = withText "SubgraphTier" $ \t -> case t of
    "core"     -> pure CoreTier
    "boundary" -> pure BoundaryTier
    "external" -> pure ExternalTier
    _          -> fail $ "Unknown SubgraphTier: " ++ T.unpack t

instance NFData SubgraphTier

-- | Provenance for edges.
data EdgeProvenance = SourceGraph | Derived
  deriving (Eq, Show, Generic)

instance ToJSON EdgeProvenance where
  toJSON SourceGraph = "source"
  toJSON Derived     = "derived"

instance FromJSON EdgeProvenance where
  parseJSON = withText "EdgeProvenance" $ \t -> case t of
    "source" -> pure SourceGraph
    "derived" -> pure Derived
    _         -> fail $ "Unknown EdgeProvenance: " ++ T.unpack t

instance NFData EdgeProvenance

-- | Configuration for a named subsystem.
data SubsystemConfig = SubsystemConfig
  { scSubsystemName     :: !Text
  , scSubsystemPatterns :: ![Text]
  } deriving (Eq, Show, Generic)

instance ToJSON SubsystemConfig where
  toJSON s = object
    [ "name"     .= scSubsystemName s
    , "patterns" .= scSubsystemPatterns s
    ]

instance FromJSON SubsystemConfig where
  parseJSON = withObject "SubsystemConfig" $ \v -> SubsystemConfig
    <$> v .: "name"
    <*> v .:? "patterns" .!= []

-- | Configuration for subgraph extraction.
data SubgraphConfig = SubgraphConfig
  { scSubsystems      :: ![SubsystemConfig]
  , scMaxHops         :: !Int
  , scIncludeDerived  :: !Bool
  } deriving (Eq, Show, Generic)

instance ToJSON SubgraphConfig where
  toJSON c = object
    [ "subsystems"      .= scSubsystems c
    , "max_hops"        .= scMaxHops c
    , "include_derived" .= scIncludeDerived c
    ]

instance FromJSON SubgraphConfig where
  parseJSON = withObject "SubgraphConfig" $ \v -> SubgraphConfig
    <$> v .:? "subsystems"      .!= []
    <*> v .:? "max_hops"        .!= 1
    <*> v .:? "include_derived" .!= False

-- | Infer an architectural layer from a source path. Heuristic, path-based:
-- the highest-priority layer keyword found among the path segments.
architecturalLayer :: Text -> Text
architecturalLayer src =
  let segs = map T.toLower (T.splitOn "/" src)
  in fromMaybe "unknown"
       (findLayer [ ("domain",        ["domain"])
                  , ("usecase",       ["usecase"])
                  , ("infrastructure",["infrastructure"])
                  , ("interface",     ["app", "cli", "interface", "ui", "web", "frontend"])
                  , ("tests",         ["tests", "test", "spec", "specs"])
                  , ("data",          ["data", "database", "persistence", "repository"])
                  , ("external",      ["external", "vendor", "third_party"])
                  ] segs)
  where
    findLayer pairs segs =
      fmap fst (find (\(_, names) -> any (`elem` segs) names) pairs)

-- | Normalize a path for identity comparisons: drop a leading @.\/@ and
-- collapse @.\/@ and @..\/@ segments so @.\/src\/..\/x.ts@ equals @x.ts@.
normalizePath :: Text -> Text
normalizePath = T.intercalate "/" . collapseDots . T.splitOn "/" . stripDotSlash
  where
    stripDotSlash t = fromMaybe t (T.stripPrefix "./" t)
    collapseDots = reverse . go []
    go acc [] = acc
    go acc (".":rest) = go acc rest
    go acc ("..":rest) = case acc of
      (_:as) -> go as rest
      []     -> go ("..":acc) rest
    go acc (x:rest) = go (x:acc) rest

-- | Match a subsystem pattern against a node's normalized source file
-- (leading @.\/@ stripped, segments collapsed).
matchPath :: Text -> Node -> Bool
matchPath pat n =
  matchGlob (T.toLower pat) (T.toLower (normalizePath (toText (nodeSourceFile n))))

-- | Extract the module specifier out of an @Import@-kind node label:
-- the last @from@ clause followed by a quoted string, or — for grammars whose
-- import syntax uses a bare module name (e.g. Python) — the bare token.
parseSpecifier :: Text -> Maybe Text
parseSpecifier label =
  let matches = T.breakOnAll "from" label
  in msum [ specifierAfter (T.drop 4 after) | (_, after) <- reverse matches ]
  where
    specifierAfter t =
      let trimmed = T.dropWhile (== ' ') t
      in case T.uncons trimmed of
           Just ('\'', rest) -> nonEmptyStr (T.takeWhile (/= '\'') rest)
           Just ('"', rest)  -> nonEmptyStr (T.takeWhile (/= '"') rest)
           _                 -> bareToken trimmed
    bareToken t =
      let token = T.takeWhile (\c -> not (c `elem` [' ', '\t', '(', ','])) t
      in nonEmptyStr token
    nonEmptyStr s = if T.null s then Nothing else Just s

-- | The node that represents the file containing an @Import@ node: prefer the
-- @File@-kind node with the same source file, else any node on that file.
containingFileNode :: Map NodeId Node -> Text -> Maybe Node
containingFileNode nodeMap srcFile =
  let sameFile = [ n | n <- Map.elems nodeMap
                 , normalizePath (toText (nodeSourceFile n)) == normalizePath srcFile ]
      fileNodes = [ n | n <- sameFile, nodeKind n == Just "File" ]
  in case fileNodes of
       (n:_) -> Just n
       []    -> case sameFile of
                  (n:_) -> Just n
                  []    -> Nothing

-- | Canonical key for a package/builtin specifier: scoped packages collapse
-- to the scope root, subpaths are folded away.
packageKey :: Text -> Text
packageKey spec
  | "node:" `T.isPrefixOf` spec = spec
  | "@" `T.isPrefixOf` spec =
      case T.splitOn "/" spec of
        (a:b:_) -> a <> "/" <> b
        _       -> spec
  | otherwise = T.takeWhile (/= '/') spec

-- | Candidate on-disk paths for a relative specifier, in priority order:
-- extension-rewritten candidates first (ESM imports @.js@ for a @.ts@ file),
-- then the literal path, then @index.\<ext\>@ barrels.
candidatePaths :: FilePath -> Text -> [FilePath]
candidatePaths srcDir spec =
  let literal = normalise (srcDir </> T.unpack spec)
  in rewrites literal ++ [literal] ++ indexCandidates literal

rewrites :: FilePath -> [FilePath]
rewrites p = case takeExtension p of
  ".js"  -> [dropExtension p ++ ".ts", dropExtension p ++ ".tsx"]
  ".jsx" -> [dropExtension p ++ ".tsx", dropExtension p ++ ".ts"]
  ".mjs" -> [dropExtension p ++ ".mts", dropExtension p ++ ".ts", dropExtension p ++ ".tsx"]
  ".cjs" -> [dropExtension p ++ ".cts", dropExtension p ++ ".ts"]
  _      -> []

indexCandidates :: FilePath -> [FilePath]
indexCandidates p =
  let dir = takeDirectory p
  in [ dir </> "index.ts", dir </> "index.tsx", dir </> "index.js" ]

-- | Resolve a specifier to an existing node id (@Right@) or a canonical
-- external identity (@Left pkgKey@).
resolveTarget :: Map NodeId Node -> FilePath -> Text -> Either Text NodeId
resolveTarget nodeMap srcDir spec
  | "." `T.isPrefixOf` spec =
      case firstMatchingNode nodeMap (candidatePaths srcDir spec) of
        Just nid -> Right nid
        Nothing  -> Left ("unresolved:" <> spec)
  | otherwise = Left (packageKey spec)

firstMatchingNode :: Map NodeId Node -> [FilePath] -> Maybe NodeId
firstMatchingNode nodeMap candidates =
  let byFile = Map.fromList
        [ (normalizePath (toText (nodeSourceFile n)), nid)
        | (nid, n) <- Map.toList nodeMap
        , not (T.null (toText (nodeSourceFile n)))
        ]
  in msum [ Map.lookup (normalizePath (T.pack c)) byFile | c <- candidates ]

-- | A synthetic node representing an external package dependency.
externalPackageNode :: Text -> Node
externalPackageNode pkg = Node
  { nodeId         = "ext:" <> pkg
  , nodeLabel      = Data.Text.Short.fromText pkg
  , nodeFileType   = CodeFile
  , nodeSourceFile = Data.Text.Short.fromText ""
  , nodeLineStart  = Nothing
  , nodeLineEnd    = Nothing
  , nodeSignature  = Nothing
  , nodeCommunityId = Nothing
  , nodeKind       = Just (Data.Text.Short.fromText "ExternalPackage")
  , nodeDegree     = Nothing
  , nodeIsBridge   = Nothing
  , nodeExtra      = Just (object [ "layer" .= ("external" :: Text) ])
  , nodePresentBits = bitNodeKind
  }

-- | Derive @imports@ edges from @Import@-kind nodes when the source graph has
-- no real ones. Skips pairs that already carry a real @imports@ edge so the
-- derived set is a pure fallback (idempotent).
deriveImports :: Map NodeId Node -> Map EdgeId Edge -> (Map EdgeId Edge, Map NodeId Node)
deriveImports nodeMap edgeMap =
  let existingPairs = Set.fromList
        [ (edgeSource e, edgeTarget e)
        | e <- Map.elems edgeMap
        , edgeRelation e == Imports
        ]
      importNodes = [ n | n <- Map.elems nodeMap, nodeKind n == Just "Import" ]
      pairs = Set.toList (Set.fromList (mapMaybe (deriveOne existingPairs) importNodes))
      externalIds = [ t | (_, t) <- pairs, Map.notMember t nodeMap ]
      derivedEdges = Map.fromList
        [ (edgeIdFor s t, Edge (edgeIdFor s t) s t Imports 1.0 (Confidence 1.0)
             (Just (object [ "provenance" .= Derived ])))
        | (s, t) <- pairs
        ]
      extraNodes = Map.fromList [ (t, externalPackageNode (edgeIdToKey t)) | t <- externalIds ]
  in (derivedEdges, extraNodes)
  where
    deriveOne existingPairs n = do
      spec <- parseSpecifier (toText (nodeLabel n))
      srcNode <- containingFileNode nodeMap (toText (nodeSourceFile n))
      let srcId = nodeId srcNode
          srcDir = takeDirectory (T.unpack (toText (nodeSourceFile srcNode)))
      tgtId <- case resolveTarget nodeMap srcDir spec of
        Right nid  -> Just nid
        Left  pkg  -> Just ("ext:" <> pkg)
      if (srcId, tgtId) `Set.member` existingPairs
        then Nothing
        else Just (srcId, tgtId)

    edgeIdFor s t = EdgeId (s <> "->" <> t <> ":imports:derived")
    edgeIdToKey t = fromMaybe t (T.stripPrefix "ext:" t)

-- | Extracts a subgraph starting from the core subsystem nodes.
--
-- Core nodes match a subsystem pattern against their source file; boundary
-- nodes are reached over @imports@ edges (in either direction) within
-- @scMaxHops@; external nodes are import targets outside core/boundary.
-- Every kept node carries @tier@/@subsystem@/@layer@ metadata and every kept
-- edge carries a @provenance@ marker (@source@ or @derived@).
extractSubgraph :: LabeledGraph -> SubgraphConfig -> LabeledGraph
extractSubgraph g config =
  let
    allNodesMap = gNodes g
    allEdgesMap = gEdges g

    (derivedEdgesMap, extraNodesMap) =
      if scIncludeDerived config
        then deriveImports allNodesMap allEdgesMap
        else (Map.empty, Map.empty)

    effNodes = allNodesMap `Map.union` extraNodesMap
    effEdges = allEdgesMap `Map.union` derivedEdgesMap

    -- 1. Core nodes: source file matches any subsystem pattern.
    coreNodesSet = Set.fromList
      [ nid
      | (nid, n) <- Map.toList effNodes
      , any (\sub -> any (\pat -> matchPath pat n) (scSubsystemPatterns sub))
            (scSubsystems config)
      ]

    -- 2. Boundary nodes: BFS over @imports@ edges in both directions.
    boundaryNodesSet = bfs coreNodesSet (Set.toList coreNodesSet) 0
      where
        bfs visited [] _ = visited
        bfs visited _ d | d >= scMaxHops config = visited
        bfs visited current d =
          let next = Set.fromList
                [ target  | src <- current, target <- importNeighbors src ]
                `Set.union` Set.fromList
                [ source  | tgt <- current, source <- importIncoming tgt ]
          in bfs (visited `Set.union` next) (Set.toList (next `Set.difference` visited)) (d + 1)

        importNeighbors nid =
          [ edgeTarget e
          | e <- Map.elems effEdges
          , edgeSource e == nid
          , edgeRelation e == Imports
          ]

        importIncoming nid =
          [ edgeSource e
          | e <- Map.elems effEdges
          , edgeTarget e == nid
          , edgeRelation e == Imports
          ]

    -- 3. External nodes: import targets of core/boundary, not core/boundary.
    coreOrBoundary = coreNodesSet `Set.union` boundaryNodesSet
    externalNodesSet = Set.fromList
      [ edgeTarget e
      | e <- Map.elems effEdges
      , edgeRelation e == Imports
      , edgeSource e `Set.member` coreOrBoundary
      ]
      `Set.difference` coreOrBoundary

    -- 4. Collect selected nodes with tier/subsystem/layer metadata.
    allSelectedNodes = coreNodesSet `Set.union` boundaryNodesSet `Set.union` externalNodesSet

    findSubsystem nid = case Map.lookup nid effNodes of
      Nothing -> Nothing
      Just n  ->
        let matches = [ scSubsystemName sub
                      | sub <- scSubsystems config
                      , any (\pat -> matchPath pat n) (scSubsystemPatterns sub)
                      ]
        in case matches of
             (m:_) -> Just m
             []    -> Nothing

    tierOf nid
      | nid `Set.member` coreNodesSet  = CoreTier
      | nid `Set.member` boundaryNodesSet = BoundaryTier
      | otherwise = ExternalTier

    nodeWithMetadata nid n =
      let tier = tierOf nid
          sub = if tier == CoreTier then findSubsystem nid else Nothing
          layer = architecturalLayer (toText (nodeSourceFile n))
          obj = KM.fromList
             [ (Data.Aeson.Key.fromText "tier", toJSON tier)
             , (Data.Aeson.Key.fromText "subsystem", toJSON sub)
             , (Data.Aeson.Key.fromText "layer", toJSON layer)
             ]
          base = case nodeExtra n of
            Just (Object km) -> km
            _                -> KM.empty
      in n { nodeExtra = Just (Object (base `KM.union` obj)) }

    newNodesMap = Map.fromList
      [ (nid, nodeWithMetadata nid n)
      | (nid, n) <- Map.toList effNodes
      , nid `Set.member` allSelectedNodes
      ]

    edgeWithProvenance e =
      let prov = if Map.member (edgeId e) derivedEdgesMap then Derived else SourceGraph
          obj = KM.fromList [ (Data.Aeson.Key.fromText "provenance", toJSON prov) ]
          base = case edgeExtra e of
            Just (Object km) -> km
            _                -> KM.empty
      in e { edgeExtra = Just (Object (base `KM.union` obj)) }

    newEdgesMap = Map.fromList
      [ (edgeId e, edgeWithProvenance e)
      | e <- Map.elems effEdges
      , edgeSource e `Set.member` allSelectedNodes
      , edgeTarget e `Set.member` allSelectedNodes
      ]

    newAdjFwd = Map.fromListWith Set.union
      [ (edgeSource e, Set.singleton (edgeTarget e))
      | e <- Map.elems newEdgesMap
      ]

    newAdjBack = Map.fromListWith Set.union
      [ (edgeTarget e, Set.singleton (edgeSource e))
      | e <- Map.elems newEdgesMap
      ]

  in LabeledGraph
    { gNodes = newNodesMap
    , gEdges = newEdgesMap
    , gAdjFwd = newAdjFwd
    , gAdjBack = newAdjBack
    }
