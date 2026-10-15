-- | Core data types for the @openspec-view@ workspace inspector.
--
-- These types model a single row of the OpenSpec workspace view and mirror
-- the shape reported by @openspec list --json@ so the two can cross-check each
-- other. They carry no IO or filesystem logic; see
-- 'Graphos.OpenSpecView.List' for the parser and renderer.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.OpenSpecView.Types
  ( ChangeStatus (..)
  , ChangeInfo (..)
  , parseStatus
  , statusLabel
  , openSpecChangesDir
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Prelude (Eq, FilePath, Int, Num, Show, (==), otherwise)

-- | Lifecycle status of an OpenSpec change, mirroring @openspec list@.
data ChangeStatus
  = InProgress  -- ^ at least one declared task remains unchecked
  | Complete    -- ^ every declared task is checked
  | NoTasks     -- ^ no tasks declared yet
  deriving (Show, Eq)

-- | A single row of the OpenSpec workspace view.
data ChangeInfo = ChangeInfo
  { changeName     :: Text       -- ^ directory name under @openspec/changes/@
  , changeStatus   :: ChangeStatus
  , completedTasks :: Int        -- ^ checked task boxes
  , totalTasks     :: Int        -- ^ total declared task boxes
  , lastModified   :: UTCTime    -- ^ newest artifact mtime in the change dir
  } deriving (Show, Eq)

-- | Derive 'ChangeStatus' from raw task counts.
parseStatus :: Int -> Int -> ChangeStatus
parseStatus completed total
  | total == 0 = NoTasks
  | completed == total = Complete
  | otherwise = InProgress

-- | Human-readable label matching @openspec list@ status values.
statusLabel :: ChangeStatus -> Text
statusLabel InProgress = "in-progress"
statusLabel Complete = "complete"
statusLabel NoTasks = "no-tasks"

-- | Default location of the OpenSpec changes directory relative to repo root.
openSpecChangesDir :: FilePath
openSpecChangesDir = "openspec/changes"
