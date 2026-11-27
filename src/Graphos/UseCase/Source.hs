{-# LANGUAGE OverloadedStrings #-}
-- | @graphos source <node>@: the source lines of a node, read from disk
-- (function-source-access).
--
-- Range selection, context clipping, the stale check and rendering are pure;
-- the only IO is the 'SourceReaderPort' that 'readNodeSource' calls once.
module Graphos.UseCase.Source
  ( SourceView(..)
  , SourceError(..)
  , SourceResult(..)
  , readNodeSource
  , selectSource
  , resolveSourcePath
  , nodeRange
  , spanHash
  , sourceSucceeded
  , sourceWarnings
  , renderSourceText
  , renderSourceJSON
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Short (toText)
import System.FilePath (isAbsolute, normalise, (</>))

import Graphos.Domain.Types (Node(..))
import Graphos.Domain.Types.Node (nodeBodyHash)
import Graphos.UseCase.Port.SourceReaderPort (SourceReaderPort(..))

-- | The lines of a node, as they will be shown.
data SourceView = SourceView
  { svNode   :: Node
  , svPath   :: FilePath        -- ^ the file read: source file resolved against the root
  , svStart  :: Int             -- ^ first line of the node
  , svEnd    :: Int             -- ^ last line of the node
  , svShown  :: [(Int, Text)]   -- ^ numbered lines printed, context included
  , svText   :: Text            -- ^ the node's own lines joined by newlines
  , svStale  :: Bool            -- ^ the node has a body hash and the lines differ from it
  } deriving (Eq, Show)

-- | Why the node's own lines cannot be shown.
data SourceError
  = NoLineRange                  -- ^ the node carries no line start
  | MissingFile FilePath         -- ^ resolved path that does not exist
  | ShortFile FilePath Int Int   -- ^ resolved path, its line count, the node's end line
  deriving (Eq, Show)

data SourceResult
  = SourceShown SourceView
  | SourceFailed Node SourceError
  deriving (Eq, Show)

-- | The node's line range: start to end, or the start line alone when the
-- end is absent or before the start.
nodeRange :: Node -> Maybe (Int, Int)
nodeRange n = case nodeLineStart n of
  Nothing -> Nothing
  Just s  -> Just (s, max s (fromMaybe s (nodeLineEnd n)))

-- | A node's source file resolved against the corpus root. Absolute paths are
-- kept; relative ones (the usual case, relative to the scanned directory) are
-- joined to the root.
resolveSourcePath :: FilePath -> FilePath -> FilePath
resolveSourcePath root sf
  | isAbsolute sf = normalise sf
  | otherwise     = normalise (root </> sf)

-- | Read the node's file through the port and select its lines. A node
-- without a line range is reported without touching the disk.
readNodeSource :: SourceReaderPort -> FilePath -> Int -> Node -> IO SourceResult
readNodeSource port root context n = case nodeRange n of
  Nothing -> pure (SourceFailed n NoLineRange)
  Just _  -> do
    let path = resolveSourcePath root (T.unpack (toText (nodeSourceFile n)))
    contents <- srpReadLines port path
    pure (selectSource context n path contents)

-- | Pure core of 'readNodeSource': @Nothing@ for the file means it is missing.
selectSource :: Int -> Node -> FilePath -> Maybe [Text] -> SourceResult
selectSource context n path contents = case (nodeRange n, contents) of
  (Nothing, _)            -> SourceFailed n NoLineRange
  (Just _, Nothing)       -> SourceFailed n (MissingFile path)
  (Just (s, e), Just ls)
    | length ls < e       -> SourceFailed n (ShortFile path (length ls) e)
    | otherwise ->
        let ctx       = max 0 context
            numbered  = zip [1 ..] ls
            firstLine = max 1 (s - ctx)
            lastLine  = min (length ls) (e + ctx)
            shown     = [ p | p@(i, _) <- numbered, i >= firstLine, i <= lastLine ]
            own       = [ l | (i, l) <- numbered, i >= s, i <= e ]
            text      = T.intercalate "\n" own
            stale     = case nodeBodyHash n of
              Just h  -> toText h /= spanHash text
              Nothing -> False
        in SourceShown SourceView
             { svNode  = n
             , svPath  = path
             , svStart = s
             , svEnd   = e
             , svShown = shown
             , svText  = text
             , svStale = stale
             }

-- | SHA-256 (lower-case hex) of a span: its lines joined by newlines, UTF-8.
-- This is what @body_hash@ is compared against.
spanHash :: Text -> Text
spanHash = decodeUtf8 . B16.encode . SHA256.hash . encodeUtf8

-- | The command succeeds when it printed the node's own lines (stale or not).
sourceSucceeded :: SourceResult -> Bool
sourceSucceeded (SourceShown _)    = True
sourceSucceeded (SourceFailed _ _) = False

-- | Warnings that go with printed lines (text mode writes them to stderr).
sourceWarnings :: SourceResult -> [Text]
sourceWarnings (SourceShown v)
  | svStale v =
      [ "warning: " <> T.pack (svPath v) <> " changed since the graph was built"
        <> " (the lines no longer match the node's body_hash); rebuild the graph" ]
sourceWarnings _ = []

-- | Text rendering: a header with the node id and @file:start-end@, then the
-- numbered lines; for a failure, the message naming the cause.
renderSourceText :: SourceResult -> Text
renderSourceText (SourceShown v) =
  let n = svNode v
      width = length (show (maybe 1 fst (lastMaybe (svShown v))))
      pad i = let s = show i in T.pack (replicate (width - length s) ' ' ++ s)
      header = nodeId n <> "  " <> toText (nodeSourceFile n) <> ":"
               <> T.pack (show (svStart v)) <> "-" <> T.pack (show (svEnd v))
  in T.unlines (header : [ pad i <> " | " <> l | (i, l) <- svShown v ])
renderSourceText (SourceFailed n err) = errorMessage n err <> "\n"

-- | JSON rendering: one object. Success carries @text@ and @stale@; a failure
-- carries an @error@ code and its @message@.
renderSourceJSON :: SourceResult -> Text
renderSourceJSON r = decodeUtf8 (BL.toStrict (Aeson.encode (sourceValue r)))

sourceValue :: SourceResult -> Value
sourceValue (SourceShown v) = object $
  nodeFields (svNode v)
  ++ [ "line_start" .= svStart v
     , "line_end"   .= svEnd v
     , "path"       .= svPath v
     , "text"       .= svText v
     , "stale"      .= svStale v
     ]
sourceValue (SourceFailed n err) = object $
  nodeFields n
  ++ [ "line_start" .= nodeLineStart n
     , "line_end"   .= nodeLineEnd n
     , "error"      .= errorCode err
     , "message"    .= errorMessage n err
     ]
  ++ case err of
       MissingFile p     -> [ "path" .= p ]
       ShortFile p len _ -> [ "path" .= p, "file_lines" .= len ]
       NoLineRange       -> []

nodeFields :: Node -> [(Aeson.Key, Value)]
nodeFields n =
  [ "id"          .= nodeId n
  , "label"       .= toText (nodeLabel n)
  , "kind"        .= fmap toText (nodeKind n)
  , "source_file" .= toText (nodeSourceFile n)
  ]

errorCode :: SourceError -> Text
errorCode NoLineRange       = "no_line_range"
errorCode (MissingFile _)   = "missing_file"
errorCode (ShortFile _ _ _) = "short_file"

errorMessage :: Node -> SourceError -> Text
errorMessage n NoLineRange =
  "Node " <> nodeId n <> " has no line range (source file: "
  <> toText (nodeSourceFile n) <> "); rebuild the graph with a version that records it."
errorMessage _ (MissingFile p) =
  "File not found: " <> T.pack p
  <> ". Source paths are relative to the scanned directory; pass it with --root <dir>."
errorMessage n (ShortFile p len e) =
  "File " <> T.pack p <> " has " <> T.pack (show len) <> " lines, but node "
  <> nodeId n <> " ends at line " <> T.pack (show e)
  <> "; the file changed since the graph was built, or --root points at another tree."

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)
