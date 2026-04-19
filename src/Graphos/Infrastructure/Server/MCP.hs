-- | MCP (Model Context Protocol) server for graph access
-- Implements JSON-RPC over stdio (line-delimited).
-- Exposes tools: query_graph, get_node, get_neighbors, get_community, god_nodes, graph_stats, shortest_path
--   bridge_nodes, select_context, add_conversation, conversation_history
module Graphos.Infrastructure.Server.MCP
  ( startMCPServer
  , startMCPServerFromFile
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:), (.:?), (.!=), withObject, encode, eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (toList)
import System.IO (hFlush, stdout, isEOF)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors, godNodes, articulationPoints)
import Graphos.Domain.Analysis (analyze)
import Graphos.Domain.Context (QueryComplexity(..), ConversationNode(..), budgetForComplexity, SelectedContext(..))
import Graphos.UseCase.Query (queryGraph, pathQuery, QueryResult(..))
import Graphos.UseCase.Load (loadGraphFromFile, lrGraph, lrCommunities, lrCohesion)
import Graphos.UseCase.SelectContext (selectContext, classifyComplexity)
import Graphos.UseCase.FormatContext (formatContextForLLM)
import Graphos.UseCase.Conversation (addConversationNode, queryConversations, summarizeConversation)
import Graphos.Infrastructure.FileSystem.Conversation (saveConversationToFile, loadConversationsFromDir)

-- | Start MCP server from a graph file
startMCPServerFromFile :: FilePath -> IO ()
startMCPServerFromFile path = do
  result <- loadGraphFromFile path
  case result of
    Left err -> BSLC.putStrLn $ "Error loading graph: " `BSL.append` BSL.fromStrict (TE.encodeUtf8 err)
    Right loaded -> do
      let g = lrGraph loaded
          commMap = lrCommunities loaded
          cohesion = lrCohesion loaded
          analysis = analyze g commMap cohesion
      startMCPServer g commMap analysis

-- | Start an MCP stdio server with a pre-loaded graph, community data, and analysis
startMCPServer :: Graph -> CommunityMap -> Analysis -> IO ()
startMCPServer g commMap analysis = do
  requestLoop g commMap analysis

-- ───────────────────────────────────────────────
-- Request loop
-- ───────────────────────────────────────────────

requestLoop :: Graph -> CommunityMap -> Analysis -> IO ()
requestLoop g commMap analysis = do
  eof <- isEOF
  if eof
    then pure ()
    else do
      line <- getLine
      case eitherDecode (BSL.fromStrict (TE.encodeUtf8 (T.pack line))) of
        Left err -> do
          sendError (-32700) ("Parse error: " <> T.pack err) Nothing
          requestLoop g commMap analysis
        Right req -> do
          handleRequest g commMap analysis req
          requestLoop g commMap analysis

-- ───────────────────────────────────────────────
-- Request handling
-- ───────────────────────────────────────────────

handleRequest :: Graph -> CommunityMap -> Analysis -> MCPRequest -> IO ()
handleRequest g commMap analysis req =
  case rqpMethod req of
    "initialize" -> sendBSL (encode (initializeResponse (rqpId req)))
    "tools/list" -> sendBSL (encode (toolsListResponse (rqpId req)))
    "tools/call" -> handleToolCall g commMap analysis (rqpId req) (rqpParams req)
    _ -> sendError (-32601) ("Method not found: " <> rqpMethod req) (Just (rqpId req))

-- ───────────────────────────────────────────────
-- Tool dispatch
-- ───────────────────────────────────────────────

handleToolCall :: Graph -> CommunityMap -> Analysis -> Value -> KM.KeyMap Value -> IO ()
handleToolCall g commMap analysis reqId params = do
  let toolName = case KM.lookup (Key.fromText "name") params of
                   Just (String s) -> s
                   _ -> "unknown"
      args = case KM.lookup (Key.fromText "arguments") params of
               Just (Object o) -> o
               _ -> KM.empty
  result <- case toolName of
    "query_graph"        -> handleQueryGraph g args
    "get_node"           -> handleGetNode g args
    "get_neighbors"      -> handleGetNeighbors g args
    "get_community"      -> handleGetCommunity g commMap args
    "god_nodes"          -> handleGodNodes g args
    "graph_stats"        -> handleGraphStats g args
    "shortest_path"      -> handleShortestPath g args
    "bridge_nodes"       -> handleBridgeNodes g
    "select_context"     -> handleSelectContext g commMap analysis args
    "add_conversation"   -> handleAddConversation g args
    "conversation_history" -> handleConversationHistory g args
    _ -> pure (Left ("Unknown tool: " <> toolName))
  case result of
    Right content -> sendToolResult reqId content
    Left err -> sendError (-32602) err (Just reqId)

-- ───────────────────────────────────────────────
-- Tool handlers
-- ───────────────────────────────────────────────

handleQueryGraph :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleQueryGraph g args = do
  let question = textArg args "question"
      mode = fromMaybe "bfs" (textArgMaybe args "mode")
      budget = fromMaybe 2000 (intArgMaybe args "budget")
  if T.null question
    then pure (Left "Missing required argument: question")
    else pure $ Right $ object
      [ "nodes" .= qrNodes (queryGraph g question mode budget)
      , "edges" .= qrEdges (queryGraph g question mode budget)
      , "traverse" .= qrTraverse (queryGraph g question mode budget)
      ]

handleGetNode :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleGetNode g args = do
  let nid = textArg args "node_id"
  if T.null nid
    then pure (Left "Missing required argument: node_id")
    else case Map.lookup nid (gNodes g) of
           Just n  -> pure (Right (toJSON n))
           Nothing -> pure (Left ("Node not found: " <> nid))

handleGetNeighbors :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleGetNeighbors g args = do
  let nid = textArg args "node_id"
  if T.null nid
    then pure (Left "Missing required argument: node_id")
    else case Map.lookup nid (gNodes g) of
           Nothing -> pure (Left ("Node not found: " <> nid))
           Just _ -> do
             let nbs = Set.toList (neighbors g nid)
                 neighborNodes = catMaybes [Map.lookup nb (gNodes g) | nb <- nbs]
             pure (Right (toJSON neighborNodes))

handleGetCommunity :: Graph -> CommunityMap -> KM.KeyMap Value -> IO (Either Text Value)
handleGetCommunity g commMap args = do
  let nid = textArg args "node_id"
  if T.null nid
    then pure (Left "Missing required argument: node_id")
    else case findCommunityForNode nid commMap of
           Nothing -> pure (Left ("Node not found in any community: " <> nid))
           Just cid -> case Map.lookup cid commMap of
             Just members -> pure (Right (object
               [ "community_id" .= cid
               , "members" .= length members
               , "member_ids" .= take 20 members
               , "is_bridge" .= (nid `elem` articulationPoints g)
               ]))
             Nothing -> pure (Left ("Community not found: " <> T.pack (show cid)))

findCommunityForNode :: NodeId -> CommunityMap -> Maybe CommunityId
findCommunityForNode nid commMap =
  listToMaybe [cid | (cid, members) <- Map.toList commMap, nid `elem` members]

handleGodNodes :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleGodNodes g args = do
  let topN = fromMaybe 10 (intArgMaybe args "top_n")
  pure (Right (toJSON (godNodes g topN)))

handleGraphStats :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleGraphStats g _args = do
  let nodeCount = Map.size (gNodes g)
      edgeCount = Map.size (gEdges g)
      avgDegree :: Double
      avgDegree = if nodeCount > 0
                  then fromIntegral edgeCount * 2.0 / fromIntegral nodeCount
                  else 0.0
  pure (Right (object
    [ "node_count"    .= nodeCount
    , "edge_count"   .= edgeCount
    , "avg_degree"   .= avgDegree
    ]))

handleShortestPath :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleShortestPath g args = do
  let from = textArg args "from"
      to = textArg args "to"
  if T.null from || T.null to
    then pure (Left "Missing required arguments: from, to")
    else case pathQuery g from to of
           Nothing  -> pure (Right (object ["found" .= False]))
           Just path -> pure (Right (object ["found" .= True, "path" .= path, "hops" .= (length path - 1)]))

handleBridgeNodes :: Graph -> IO (Either Text Value)
handleBridgeNodes g = do
  let bridges = articulationPoints g
  pure (Right (object
    [ "bridge_count" .= length bridges
    , "bridge_nodes" .= take 50 bridges
    ]))

-- ───────────────────────────────────────────────
-- Context optimization tool handlers
-- ───────────────────────────────────────────────

-- | Select relevant context for a query using graph-based context optimization.
-- Returns a compact markdown representation of the selected subgraph,
-- suitable for inclusion in an LLM prompt.
handleSelectContext :: Graph -> CommunityMap -> Analysis -> KM.KeyMap Value -> IO (Either Text Value)
handleSelectContext g commMap analysis args = do
  let question = textArg args "question"
      budget = fromMaybe 3000 (intArgMaybe args "budget")
  if T.null question
    then pure (Left "Missing required argument: question")
    else do
      let complexity = classifyComplexity question g
          ctxBudget = budgetForComplexity complexity budget
          selectedCtx = selectContext g commMap analysis question ctxBudget
          formatted = formatContextForLLM selectedCtx
      pure (Right (object
        [ "context" .= formatted
        , "complexity" .= showComplexity complexity
        , "nodes_selected" .= length (scNodes selectedCtx)
        , "edges_selected" .= length (scEdges selectedCtx)
        , "token_estimate" .= T.length formatted
        ]))

-- | Store a conversation exchange in the graph for persistent cross-session memory.
-- Saves the conversation to graphos-out/memory/ for future retrieval.
handleAddConversation :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleAddConversation g args = do
  let question = textArg args "question"
      summary = textArg args "answer_summary"
      sourceNodes = textListArg args "source_nodes"
  if T.null question
    then pure (Left "Missing required argument: question")
    else do
      now <- getCurrentTime
      let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" now)
          convId' = "conv_" <> T.replace ":" "" (T.replace "-" "" timestamp)
          conv = ConversationNode
            { convId            = convId'
            , convQuestion      = question
            , convSummary       = if T.null summary then "(no summary)" else summary
            , convTimestamp     = timestamp
            , convRelevantNodes = sourceNodes
            , convTokensUsed    = 0
            }
      -- Save to memory directory
      saveConversationToFile "graphos-out/memory" conv
      -- Add to graph (for in-memory querying)
      let _updatedGraph = addConversationNode g conv
      pure (Right (object
        [ "status" .= ("saved" :: Text)
        , "conversation_id" .= convId'
        , "question" .= question
        , "linked_nodes" .= length sourceNodes
        ]))

-- | Search conversation history for past exchanges matching a query.
-- Returns a list of past Q&A summaries relevant to the search terms.
handleConversationHistory :: Graph -> KM.KeyMap Value -> IO (Either Text Value)
handleConversationHistory g args = do
  let query = textArg args "query"
      limit = fromMaybe 10 (intArgMaybe args "limit")
  if T.null query
    then pure (Left "Missing required argument: query")
    else do
      let conversations = take limit (queryConversations g query)
      -- Also try loading from disk
      diskConvs <- loadConversationsFromDir "graphos-out/memory"
      let diskMatches = take limit [c | c <- diskConvs
                                      , not (T.null (convQuestion c))
                                      , any (`T.isInfixOf` T.toLower (convQuestion c))
                                            (T.words (T.toLower query))]
      pure (Right (object
        [ "total_matches" .= (length conversations + length diskMatches)
        , "in_memory" .= map (summarizeConversation) conversations
        , "from_disk" .= map (summarizeConversation) diskMatches
        ]))

-- ───────────────────────────────────────────────
-- JSON-RPC protocol types
-- ───────────────────────────────────────────────

data MCPRequest = MCPRequest
  { rqpId     :: Value
  , rqpMethod :: Text
  , rqpParams :: KM.KeyMap Value
  }

instance FromJSON MCPRequest where
  parseJSON = withObject "MCPRequest" $ \v -> MCPRequest
    <$> v .: "id"
    <*> v .: "method"
    <*> v .:? "params" .!= KM.empty

-- ───────────────────────────────────────────────
-- Response helpers
-- ───────────────────────────────────────────────

sendBSL :: BSL.ByteString -> IO ()
sendBSL bs = BSLC.putStrLn bs >> hFlush stdout

sendError :: Int -> Text -> Maybe Value -> IO ()
sendError code msg mid = do
  let resp = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= fromMaybe Null mid
        , "error" .= object ["code" .= code, "message" .= msg]
        ]
  sendBSL (encode resp)

sendToolResult :: Value -> Value -> IO ()
sendToolResult reqId content = do
  let contentText = TE.decodeUtf8 (BSL.toStrict (encode content))
      resp = object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "result" .= object ["content" .= [object ["type" .= ("text" :: Text), "text" .= contentText]]]
        ]
  sendBSL (encode resp)

initializeResponse :: Value -> Value
initializeResponse reqId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "result" .= object
    [ "protocolVersion" .= ("2024-11-05" :: Text)
    , "capabilities" .= object ["tools" .= object []]
    , "serverInfo" .= object ["name" .= ("graphos" :: Text), "version" .= ("0.1.0" :: Text)]
    ]
  ]

toolsListResponse :: Value -> Value
toolsListResponse reqId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= reqId
  , "result" .= object ["tools" .= map toolDef allTools]
  ]

allTools :: [(Text, Text, [(Text, Text, Bool)])]
allTools =
  [ ("query_graph", "Query the knowledge graph using BFS or DFS traversal", [("question", "The search question", True), ("mode", "bfs or dfs", False), ("budget", "Token budget", False)])
  , ("get_node", "Get details of a specific node", [("node_id", "Node ID to look up", True)])
  , ("get_neighbors", "Get all neighbors of a node", [("node_id", "Node ID", True)])
  , ("get_community", "Get community membership for a node", [("node_id", "Node ID", True)])
  , ("god_nodes", "Get highest-degree nodes in the graph", [("top_n", "Number of nodes", False)])
  , ("graph_stats", "Get graph statistics (nodes, edges, avg degree)", [])
  , ("shortest_path", "Find shortest path between two nodes", [("from", "Source concept", True), ("to", "Target concept", True)])
  , ("bridge_nodes", "Find articulation points (bridge nodes) whose removal disconnects the graph", [])
  , ("select_context", "Select relevant context from the graph for an LLM query. Returns compact markdown with key nodes, edges, communities, and bridge nodes.", [("question", "The query to select context for", True), ("budget", "Token budget (default: 3000)", False)])
  , ("add_conversation", "Store a conversation exchange in the graph for persistent cross-session memory. Saves to graphos-out/memory/", [("question", "The user's question", True), ("answer_summary", "Short summary of the answer", False), ("source_nodes", "List of code node IDs referenced", False)])
  , ("conversation_history", "Search past conversation exchanges matching a query", [("query", "Search terms", True), ("limit", "Max results (default: 10)", False)])
  ]

toolDef :: (Text, Text, [(Text, Text, Bool)]) -> Value
toolDef (name, desc, params) = object
  [ "name" .= name
  , "description" .= desc
  , "inputSchema" .= object
    [ "type" .= ("object" :: Text)
    , "properties" .= object [Key.fromText k .= object ["type" .= ("string" :: Text), "description" .= d] | (k, d, _) <- params]
    , "required" .= [k | (k, _, True) <- params]
    ]
  ]

-- ───────────────────────────────────────────────
-- Argument extraction helpers
-- ───────────────────────────────────────────────

textArg :: KM.KeyMap Value -> Text -> Text
textArg args key = fromMaybe "" $ case KM.lookup (Key.fromText key) args of
  Just (String s) -> Just s
  _ -> Nothing

textArgMaybe :: KM.KeyMap Value -> Text -> Maybe Text
textArgMaybe args key = case KM.lookup (Key.fromText key) args of
  Just (String s) -> Just s
  _ -> Nothing

intArgMaybe :: KM.KeyMap Value -> Text -> Maybe Int
intArgMaybe args key = case KM.lookup (Key.fromText key) args of
  Just (Number n) -> Just (round n)
  _ -> Nothing

-- | Extract a list of text values from an array argument
textListArg :: KM.KeyMap Value -> Text -> [Text]
textListArg args key = case KM.lookup (Key.fromText key) args of
  Just (Array arr) -> [s | String s <- toList arr]
  _ -> []

-- | Convert QueryComplexity to a text label
showComplexity :: QueryComplexity -> Text
showComplexity Focused       = "focused"
showComplexity ModuleLevel   = "module"
showComplexity CrossModule   = "cross_module"
showComplexity Architectural = "architectural"
showComplexity Exploratory   = "exploratory"