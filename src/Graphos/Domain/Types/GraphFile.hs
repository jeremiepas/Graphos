{-# LANGUAGE OverloadedStrings #-}

-- | Shared graph.json contract: schema version and canonical top-level key set.
-- Both the incremental writer (Infrastructure/Export/IncrementalJSON) and the
-- loader (UseCase/Load) derive from these values, so the writer/reader key
-- sets cannot drift apart.
module Graphos.Domain.Types.GraphFile
  ( graphFileSchemaVersion
  , graphFileTopLevelKeys
  , graphFileRequiredKeys
  ) where

import Data.Text (Text)

-- | Version of the graph.json schema this build writes and fully supports.
graphFileSchemaVersion :: Text
graphFileSchemaVersion = "1"

-- | Every top-level key of a complete graph.json, in canonical write order.
graphFileTopLevelKeys :: [Text]
graphFileTopLevelKeys =
  [ "schema_version"
  , "nodes"
  , "edges"
  , "communities"
  , "cohesion"
  , "god_nodes"
  , "community_aggregates"
  , "compositions"
  , "community_labels"
  ]

-- | Keys the loader requires; the exporter writes these unconditionally.
graphFileRequiredKeys :: [Text]
graphFileRequiredKeys = ["nodes", "edges"]
