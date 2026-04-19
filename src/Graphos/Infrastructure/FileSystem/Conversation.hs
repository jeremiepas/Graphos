-- | Infrastructure: conversation memory persistence to filesystem.
-- Saves/loads conversation nodes as markdown files with YAML frontmatter,
-- following the same pattern as saveQueryResult in UseCase.Query.
module Graphos.Infrastructure.FileSystem.Conversation
  ( saveConversationToFile
  , loadConversationsFromDir
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, (<.>))
import Data.Maybe (fromMaybe)

import Graphos.Domain.Context (ConversationNode(..))

-- ───────────────────────────────────────────────
-- Save
-- ───────────────────────────────────────────────

-- | Save a conversation node to a markdown file with YAML frontmatter.
-- File format:
--
-- > ---
-- > id: "conv_2026_04_17_001"
-- > question: "How does MCP server work?"
-- > timestamp: "2026-04-17T18:00:00Z"
-- > relevant_nodes: ["node1", "node2"]
-- > tokens_used: 1500
-- > ---
-- > # Q: How does MCP server work?
-- >
-- > A: The MCP server exposes tools via JSON-RPC over stdio...
saveConversationToFile :: FilePath -> ConversationNode -> IO ()
saveConversationToFile memDir conv = do
  createDirectoryIfMissing True memDir
  let filepath = memDir </> T.unpack (convId conv) <.> "md"
      frontmatter = T.unlines
        [ "---"
        , "id: " <> quoteWrap (convId conv)
        , "question: " <> quoteWrap (convQuestion conv)
        , "summary: " <> quoteWrap (convSummary conv)
        , "timestamp: " <> quoteWrap (convTimestamp conv)
        , "relevant_nodes: [" <> T.intercalate ", " (map quoteWrap (convRelevantNodes conv)) <> "]"
        , "tokens_used: " <> T.pack (show (convTokensUsed conv))
        , "---"
        ]
      content = frontmatter <> "\n# Q: " <> convQuestion conv <> "\n\n" <> convSummary conv <> "\n"
  TIO.writeFile filepath content
  where
    quoteWrap t = "\"" <> t <> "\""

-- ───────────────────────────────────────────────
-- Load
-- ───────────────────────────────────────────────

-- | Load all conversation nodes from a directory.
-- Expects files in the YAML-frontmatter markdown format produced by saveConversationToFile.
loadConversationsFromDir :: FilePath -> IO [ConversationNode]
loadConversationsFromDir memDir = do
  exists <- doesFileExist memDir
  if not exists
    then pure []
    else do
      files <- listDirectory memDir
      let mdFiles = sortOn id [f | f <- files, takeExtension f == ".md"]
      conversations <- mapM (loadSingleConversation . (memDir </>)) mdFiles
      pure [c | Right c <- conversations]

-- | Load a single conversation from a file
loadSingleConversation :: FilePath -> IO (Either Text ConversationNode)
loadSingleConversation filepath = do
  content <- TIO.readFile filepath
  pure $ parseConversationFile content

-- ───────────────────────────────────────────────
-- Parsing
-- ───────────────────────────────────────────────

-- | Parse a conversation from its markdown+YAML format.
-- Minimal parser — extracts id, question, summary, timestamp, relevant_nodes, tokens_used.
parseConversationFile :: Text -> Either Text ConversationNode
parseConversationFile content =
  let lines' = T.lines content
      -- Extract frontmatter between --- delimiters
      (frontmatterLines, _bodyLines) = extractFrontmatter lines'
      -- Parse frontmatter fields
      idVal       = extractField "id:" frontmatterLines
      questionVal = extractField "question:" frontmatterLines
      summaryVal  = extractField "summary:" frontmatterLines
      timestampVal = extractField "timestamp:" frontmatterLines
      nodesVal    = extractField "relevant_nodes:" frontmatterLines
      tokensVal   = extractField "tokens_used:" frontmatterLines
  in case (idVal, questionVal) of
       (Just cid, Just q) -> Right ConversationNode
         { convId            = cid
         , convQuestion      = q
         , convSummary       = fromMaybe "" summaryVal
         , convTimestamp     = fromMaybe "" timestampVal
         , convRelevantNodes = parseNodeList nodesVal
         , convTokensUsed    = maybe 0 readInt tokensVal
         }
       _ -> Left "Failed to parse conversation file: missing id or question"

-- | Extract YAML frontmatter lines between --- delimiters
extractFrontmatter :: [Text] -> ([Text], [Text])
extractFrontmatter lines' =
  case dropWhile (/= "---") lines' of
    (_:rest) ->
      let (fm, afterClose) = span (/= "---") rest
          body = drop 1 afterClose  -- skip closing ---
      in (fm, body)
    [] -> ([], lines')

-- | Extract a field value from frontmatter lines, stripping quotes
extractField :: Text -> [Text] -> Maybe Text
extractField prefix lines' =
  case filter (T.isPrefixOf prefix) lines' of
    (line:_) -> Just $ stripQuotes $ T.strip $ T.drop (T.length prefix) line
    []       -> Nothing

-- | Strip surrounding quotes from a value
stripQuotes :: Text -> Text
stripQuotes t =
  let t1 = if T.isPrefixOf "\"" t then T.drop 1 t else t
      t2 = if T.isSuffixOf "\"" t1 then T.dropEnd 1 t1 else t1
  in t2

-- | Parse a YAML list like ["node1", "node2"]
parseNodeList :: Maybe Text -> [Text]
parseNodeList maybeText =
  case maybeText of
    Nothing -> []
    Just txt ->
      let cleaned = T.strip (T.dropWhile (`elem` ("[" :: String)) txt)
          -- Remove brackets and split by comma
          inner = T.dropWhileEnd (`elem` ("]" :: String)) cleaned
          parts = T.splitOn "," inner
      in map stripQuotes (filter (not . T.null . T.strip) parts)

-- | Safe integer read
readInt :: Text -> Int
readInt t = case reads (T.unpack t) of
  [(n, "")] -> n
  _         -> 0