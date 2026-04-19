-- | Conversation memory — pure functions for managing conversation nodes.
-- Conversation exchanges are stored in a dedicated "chat history" community
-- (community 0) that is added AFTER Leiden detection to prevent pollution.
-- IO operations (file persistence) are in Infrastructure.FileSystem.Conversation.
module Graphos.UseCase.Conversation
  ( conversationEdges
  , queryConversations
  , queryConversationsFromCommunity
  , summarizeConversation
  , matchConversationScore
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (Node(..), Edge(..), Relation(..), Confidence(..), FileType(..), CommunityMap)
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Context (ConversationNode(..), chatCommunityId)

-- ───────────────────────────────────────────────
-- Edge creation (pure, one-way)
-- ───────────────────────────────────────────────

-- | Create one-way edges linking a conversation to code nodes it references.
-- Edges go conversation → code only, never the reverse.
conversationEdges :: ConversationNode -> [Edge]
conversationEdges conv =
  [ Edge
    { edgeSource         = convId conv
    , edgeTarget         = codeNodeId
    , edgeRelation       = References
    , edgeConfidence     = Inferred
    , edgeConfidenceScore = 0.8
    , edgeSourceFile     = "memory/" <> convId conv <> ".md"
    , edgeSourceLocation = Nothing
    , edgeWeight         = 1.0
    }
  | codeNodeId <- convRelevantNodes conv
  ]

-- ───────────────────────────────────────────────
-- Conversation queries (pure)
-- ───────────────────────────────────────────────

-- | Find conversations in the graph that match query terms.
-- Searches conversation node labels (questions) for matching terms.
queryConversations :: Graph -> Text -> [ConversationNode]
queryConversations g query =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- Find all document nodes (conversations are stored as DocumentFile)
      convNodes = [(nid, n) | (nid, n) <- Map.toList (gNodes g)
                            , nodeFileType n == DocumentFile
                            , "memory/" `T.isPrefixOf` nodeSourceFile n]
      -- Score each conversation by how well it matches the query
      scored = [(conv, matchScore) | (_nid, n) <- convNodes
                                   , let conv = nodeToConversation n g
                                   , let matchScore = matchConversationScore conv terms
                                   , matchScore > 0]
      sorted = sortOn (\(_, s) -> negate s) scored
  in take 10 [conv | (conv, _) <- sorted]

-- | Find conversations that belong to the chat history community (community 0).
--   More efficient than queryConversations when you know conversations are in
--   the dedicated chat community.
queryConversationsFromCommunity :: Graph -> CommunityMap -> Text -> [ConversationNode]
queryConversationsFromCommunity g commMap query =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      chatMemberIds = Map.findWithDefault [] chatCommunityId commMap
      convNodes = [(nid, n) | nid <- chatMemberIds
                            , Just n <- [Map.lookup nid (gNodes g)]]
      scored = [(conv, score) | (_nid, n) <- convNodes
                              , let conv = nodeToConversation n g
                              , let score = matchConversationScore conv terms
                              , score > 0]
      sorted = sortOn (\(_, s) -> negate s) scored
  in take 10 [conv | (conv, _) <- sorted]

-- | Score a conversation against query terms
matchConversationScore :: ConversationNode -> [Text] -> Int
matchConversationScore conv terms =
  let questionMatches = matchTextScore (convQuestion conv) terms
      summaryMatches  = matchTextScore (convSummary conv) terms
  in questionMatches * 3 + summaryMatches

-- ───────────────────────────────────────────────
-- Summarization (pure)
-- ───────────────────────────────────────────────

-- | Create a compact summary of a conversation for context inclusion.
-- Format: "Q: {question}\nA: {summary} ({N} relevant nodes)"
summarizeConversation :: ConversationNode -> Text
summarizeConversation conv =
  T.concat
    [ "Q: ", convQuestion conv
    , "\nA: ", convSummary conv
    , " (", T.pack (show (length (convRelevantNodes conv))), " relevant nodes)"
    ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Reconstruct a ConversationNode from a graph Node.
-- Uses naming convention: conversation nodes have source files starting with "memory/".
nodeToConversation :: Node -> Graph -> ConversationNode
nodeToConversation n g =
  let nid = nodeId n
      -- Find edges from this conversation to code nodes
      relatedNodes = [edgeTarget e | ((src, _), e) <- Map.toList (gEdges g)
                                   , src == nid
                                   , edgeRelation e == References]
  in ConversationNode
    { convId            = nid
    , convQuestion      = nodeLabel n
    , convSummary       = ""  -- Summary not stored in node label, needs separate storage
    , convTimestamp     = maybe "" id (nodeCapturedAt n)
    , convRelevantNodes = relatedNodes
    , convTokensUsed    = 0   -- Token cost not stored in graph nodes
    }

-- | Score text against query terms (simple substring match)
matchTextScore :: Text -> [Text] -> Int
matchTextScore txt terms =
  let lower = T.toLower txt
  in sum [1 | t <- terms, T.isInfixOf t lower]