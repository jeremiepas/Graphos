-- | Conversation memory — pure functions for managing conversation nodes.
-- Conversation exchanges become graph nodes for persistent cross-session memory.
-- IO operations (file persistence) are in Infrastructure.FileSystem.Conversation.
module Graphos.UseCase.Conversation
  ( addConversationNode
  , queryConversations
  , conversationEdges
  , summarizeConversation
  , matchConversationScore
  ) where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types (Node(..), Edge(..), Relation(..), Confidence(..), FileType(..), Extraction(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges, mergeGraphs, buildGraph)
import Graphos.Domain.Context (ConversationNode(..))

-- ───────────────────────────────────────────────
-- Graph manipulation (pure)
-- ───────────────────────────────────────────────

-- | Add a conversation node to the graph, creating edges to referenced code nodes.
-- Returns a new graph with the conversation node and its edges added.
addConversationNode :: Graph -> ConversationNode -> Graph
addConversationNode g conv =
  let convNodeId = convId conv
      -- Create the conversation node
      convNode = Node
        { nodeId           = convNodeId
        , nodeLabel        = convQuestion conv
        , nodeFileType     = DocumentFile
        , nodeSourceFile   = "memory/" <> convNodeId <> ".md"
        , nodeSourceLocation = Nothing
        , nodeSourceUrl    = Nothing
        , nodeCapturedAt   = Just (convTimestamp conv)
        , nodeAuthor       = Nothing
        , nodeContributor  = Nothing
        }
      -- Create edges from conversation to referenced code nodes
      convEdges = [ Edge
        { edgeSource         = convNodeId
        , edgeTarget         = codeNodeId
        , edgeRelation       = References
        , edgeConfidence     = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile     = "memory/" <> convNodeId <> ".md"
        , edgeSourceLocation = Nothing
        , edgeWeight         = 1.0
        }
        | codeNodeId <- convRelevantNodes conv
        ]
      -- Merge into existing graph
      convExtraction = Extraction
        { extractionNodes      = convNode : []
        , extractionEdges      = convEdges
        , extractionHyperedges = []
        , extractionInputTokens  = 0
        , extractionOutputTokens = convTokensUsed conv
        }
  in mergeGraphs g (buildGraph False convExtraction)

-- | Create conversation edges linking a conversation to code nodes.
-- Returns a list of Edge values suitable for graph construction.
conversationEdges :: ConversationNode -> [Edge]
conversationEdges conv =
  [ Edge
    { edgeSource         = convId conv
    , edgeTarget         = codeNodeId
    , edgeRelation       = References
    , edgeConfidence     = Extracted
    , edgeConfidenceScore = 1.0
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