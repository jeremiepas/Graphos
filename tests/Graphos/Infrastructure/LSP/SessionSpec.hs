{-# LANGUAGE OverloadedStrings #-}
-- | Specs of change lsp-project-session: the initialize handshake, document
-- URIs, per-server session options, call edges from @from.uri@, the
-- @call_hierarchy@ mode, the connect-time log and the bounded restart.
--
-- Fixture @test-corpus/ts-sample@ (task 1.1), expected call pairs:
--
--   * @verdict@ (a.ts) is called by @sameFileCaller@ (a.ts)  — same file
--   * @verdict@ (a.ts) is called by @wrap@ (b.ts)            — cross file
--   * @verdict@ (a.ts) is called by @main@ (c.ts)            — cross file
module Graphos.Infrastructure.LSP.SessionSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, forM_, void)
import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value(..), object, (.=), eitherDecode, encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as B8
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import System.Directory (findExecutable)
import System.IO (hSetBinaryMode)
import System.Process
  ( createPipe, createProcess, proc, std_err, StdStream(CreatePipe)
  , terminateProcess, waitForProcess )
import Test.Hspec

import Graphos.Domain.Config.Extraction hiding (lspCommand, lspArgs)
import Graphos.Domain.Config.Validation (validateLSPServerEntry, configSectionErrors)
import Graphos.Domain.Types (Edge(..), Extraction(..), ExtractionError(..), Relation(..), extractionFromLists)
import Graphos.Infrastructure.LSP.CapabilityParse (parseServerCapabilities)
import Graphos.Infrastructure.LSP.Extraction
  ( callHierarchyGate, capSymbols, isMethodNotFound, callerNodePath
  , incomingCallToEdge, makeNodeId, extractCallHierarchy )
import Graphos.Infrastructure.LSP.Protocol
  ( JSONRPCRequest(..), DocumentSymbolResult(..), Range(..), Position(..)
  , ServerCapabilities(..), lspInitialize, fileUri, uriToFilePath )
import Graphos.Infrastructure.LSP.Transport
  ( LSPClient(..), LSPClientConfig(..), defaultLSPConfig, serverExitStatus
  , capabilityLogLine, readLSPMessage, sendLSPMessage, connectToLSP
  , isServerConnected, disconnectLSP )
import Graphos.UseCase.Extract.LSP (SessionOps(..), extractFilesWithRestart)

-- ───────────────────────────────────────────────
-- JSON helpers
-- ───────────────────────────────────────────────

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

initParams :: JSONRPCRequest -> Value
initParams = maybe Null id . jrpcParams

-- ───────────────────────────────────────────────
-- Fake server: an in-process JSON-RPC peer on two pipes
-- ───────────────────────────────────────────────

-- | Answer for one request: the JSON-RPC body without @jsonrpc@ and @id@.
type Answer = Text -> Value -> [(Key.Key, Value)]

-- | Run @act@ with an 'LSPClient' talking to a fake server thread. The
-- returned IORef lists the request methods the server received, in order.
withFakeServer :: LSPClientConfig -> Bool -> Answer -> (LSPClient -> IORef [Text] -> IO a) -> IO a
withFakeServer cfg advertised answer act = do
  (fromServer, serverOut) <- createPipe
  (serverIn, toServer) <- createPipe
  mapM_ (`hSetBinaryMode` True) [fromServer, serverOut, serverIn, toServer]
  -- A live process stands in for the server, so liveness checks pass.
  (_, _, Just errh, ph) <- createProcess (proc "sleep" ["60"]) { std_err = CreatePipe }
  methods <- newIORef []
  let loop = forever $ do
        msg <- readLSPMessage serverIn
        case msg of
          Right v@(Object o) -> case (KM.lookup "method" o, KM.lookup "id" o) of
            (Just (String m), Just i) -> do
              modifyIORef' methods (++ [m])
              let params = maybe Null id (KM.lookup "params" o)
              sendLSPMessage serverOut
                (Object (KM.fromList ([("jsonrpc", String "2.0"), ("id", i)] ++ answer m params)))
              pure v
            _ -> pure v
          _ -> threadDelay 1000 >> pure Null
  idVar <- newMVar 10
  connected <- newIORef True
  disabled <- newIORef False
  let client = LSPClient
        { lspHandle = ph, lspStdin = toServer, lspStdout = fromServer, lspStderr = errh
        , lspConfig = cfg, lspMessageId = idVar
        , lspServerCaps = ServerCapabilities True True advertised False False
        , lspConnected = connected, lspCallHierarchyDisabled = disabled }
  bracket (forkIO (void loop)) killThread $ \_ -> do
    r <- act client methods
    terminateProcess ph
    void (waitForProcess ph)
    pure r

sessionCfg :: CallHierarchyMode -> Int -> LSPClientConfig
sessionCfg mode cap = (defaultLSPConfig "/work/p")
  { lspCommand = "fake-server", lspSessionCallHierarchy = mode, lspSessionMaxSymbols = cap }

fn :: Text -> Int -> DocumentSymbolResult
fn name line = DocumentSymbolResult
  { dsrName = name, dsrKind = 12
  , dsrRange = Range (Position line 0) (Position (line + 2) 1)
  , dsrSelectionRange = Range (Position line 16) (Position line 23), dsrChildren = [] }

range0 :: Value
range0 = object [ "start" .= object ["line" .= (0 :: Int), "character" .= (0 :: Int)]
                , "end"   .= object ["line" .= (0 :: Int), "character" .= (1 :: Int)] ]

-- | A server that answers call hierarchy: every symbol has one caller,
-- @wrap@ in @/work/p/b.ts@.
callingServer :: Answer
callingServer m _ = case m of
  "textDocument/prepareCallHierarchy" ->
    [("result", toArr [object [ "name" .= ("verdict" :: Text), "kind" .= (12 :: Int)
                              , "uri" .= ("file:///work/p/a.ts" :: Text)
                              , "range" .= range0, "selectionRange" .= range0 ]])]
  "callHierarchy/incomingCalls" ->
    [("result", toArr [object [ "from" .= object [ "name" .= ("wrap" :: Text), "kind" .= (12 :: Int)
                                                 , "uri" .= ("file:///work/p/b.ts" :: Text)
                                                 , "range" .= range0, "selectionRange" .= range0 ]
                              , "fromRanges" .= ([] :: [Value]) ]])]
  _ -> [("result", Null)]
  where toArr = toJSONList

toJSONList :: [Value] -> Value
toJSONList = Array . V.fromList

methodNotFound :: Answer
methodNotFound _ _ = [("error", object ["code" .= (-32601 :: Int), "message" .= ("Unhandled method" :: Text)])]

countOf :: Text -> [Text] -> Int
countOf m = length . filter (== m)

-- ───────────────────────────────────────────────
-- Restart loop fake
-- ───────────────────────────────────────────────

-- | A fake session: a handle is a session number; the server of session @n@
-- dies while extracting the files listed for @n@.
data FakeRun = FakeRun
  { frExtracted :: IORef [(Int, FilePath)]
  , frSessions  :: IORef Int
  , frDead      :: IORef Bool
  , frLog       :: IORef [Text]
  }

fakeOps :: FakeRun -> Map.Map Int FilePath -> SessionOps Int
fakeOps fr deaths = SessionOps
  { soExtract = \h fp -> do
      modifyIORef' (frExtracted fr) (++ [(h, fp)])
      if Map.lookup h deaths == Just fp then writeIORef (frDead fr) True else pure ()
      pure (extractionFromLists [] [])
  , soConnected  = \_ -> not <$> readIORef (frDead fr)
  , soExitStatus = \_ -> pure (Just 1)
  , soReconnect  = do
      modifyIORef' (frSessions fr) (+ 1)
      writeIORef (frDead fr) False
      Right <$> readIORef (frSessions fr)
  , soDisconnect = \_ -> pure ()
  , soLogWarn    = \t -> modifyIORef' (frLog fr) (++ [t])
  }

newFakeRun :: IO FakeRun
newFakeRun = FakeRun <$> newIORef [] <*> newIORef 1 <*> newIORef False <*> newIORef []

tenFiles :: [FilePath]
tenFiles = ["f" ++ show i ++ ".ts" | i <- [1 .. 10 :: Int]]

errorsOf :: [Extraction] -> [ExtractionError]
errorsOf = concatMap extractionErrors

-- ───────────────────────────────────────────────

spec :: Spec
spec = do
  describe "lspInitialize (lsp-session: handshake)" $ do
    let req = lspInitialize 31337 Nothing "/work/p"
        params = initParams req
    it "carries the supplied process id and the root URI" $ do
      at ["processId"] params `shouldBe` Just (Number 31337)
      at ["rootUri"] params `shouldBe` Just (String "file:///work/p")
    it "declares references and callHierarchy without dynamic registration" $ do
      at ["capabilities", "textDocument", "callHierarchy", "dynamicRegistration"] params
        `shouldBe` Just (Bool False)
      at ["capabilities", "textDocument", "references", "dynamicRegistration"] params
        `shouldBe` Just (Bool False)
      at ["capabilities", "textDocument", "documentSymbol", "hierarchicalDocumentSymbolSupport"] params
        `shouldBe` Just (Bool True)
      at ["capabilities", "workspace", "symbol", "dynamicRegistration"] params
        `shouldBe` Just (Bool False)
    it "has no initializationOptions key unless configured" $
      at ["initializationOptions"] params `shouldBe` Nothing
    it "sends configured initializationOptions verbatim" $ do
      let opts = object ["tsserver" .= object ["maxTsServerMemory" .= (6144 :: Int)]]
          p = initParams (lspInitialize 1 (Just opts) "/work/p")
      at ["initializationOptions", "tsserver", "maxTsServerMemory"] p `shouldBe` Just (Number 6144)

  describe "parseServerCapabilities" $
    it "sees callHierarchyProvider advertised on declaration" $ do
      let resp = object ["id" .= (1 :: Int), "result" .= object
                   ["capabilities" .= object ["callHierarchyProvider" .= True]]]
      scpCallHierarchyProvider (parseServerCapabilities resp) `shouldBe` True

  describe "capabilityLogLine" $
    it "states the four capabilities" $ do
      let line = capabilityLogLine (ServerCapabilities True True False False False)
      forM_ ["documentSymbol=True", "references=True", "callHierarchy=False", "workspaceSymbol=False"] $ \k ->
        line `shouldContain` k

  describe "fileUri (lsp-session: absolute document URIs)" $ do
    it "resolves a relative path against the session root" $
      fileUri "/work/p" "./src/a.ts" `shouldBe` "file:///work/p/src/a.ts"
    it "keeps an absolute path" $
      fileUri "/work/p" "/work/p/src/a.ts" `shouldBe` "file:///work/p/src/a.ts"
    it "removes . and .. segments" $
      fileUri "/work/p" "src/./lib/../a.ts" `shouldBe` "file:///work/p/src/a.ts"
    it "percent-encodes, and uriToFilePath decodes back" $ do
      fileUri "/work/p" "my dir/@a.ts" `shouldBe` "file:///work/p/my%20dir/%40a.ts"
      uriToFilePath "file:///work/p/my%20dir/%40a.ts" `shouldBe` Just "/work/p/my dir/@a.ts"
    it "uriToFilePath rejects other schemes" $
      uriToFilePath "untitled:Untitled-1" `shouldBe` Nothing

  describe "LSPServerConfig session options (graphos.yaml)" $ do
    it "parses every option from YAML" $ do
      let yaml = B8.unlines
            [ "command: typescript-language-server", "args: [\"--stdio\"]", "language_id: typescript"
            , "call_hierarchy: \"on\"", "call_hierarchy_max_symbols: 0", "timeout_seconds: 60"
            , "initialization_options: { tsserver: { maxTsServerMemory: 6144 } }" ]
      case Yaml.decodeEither' yaml :: Either Yaml.ParseException LSPServerConfig of
        Left err -> expectationFailure (show err)
        Right c -> do
          lspCallHierarchyMode c `shouldBe` CallHierarchyOn
          lspCallHierarchyMaxSymbols c `shouldBe` 0
          lspTimeoutSeconds c `shouldBe` 60
          (lspInitializationOptions c >>= at ["tsserver", "maxTsServerMemory"]) `shouldBe` Just (Number 6144)
          let p = initParams (lspInitialize 1 (lspInitializationOptions c) "/work/p")
          at ["initializationOptions", "tsserver", "maxTsServerMemory"] p `shouldBe` Just (Number 6144)
    it "keeps the defaults when the options are omitted" $ do
      let yaml = "command: typescript-language-server\nargs: [\"--stdio\"]\nlanguage_id: typescript\n"
      case Yaml.decodeEither' yaml :: Either Yaml.ParseException LSPServerConfig of
        Left err -> expectationFailure (show err)
        Right c -> do
          c `shouldBe` lspServer "typescript-language-server" ["--stdio"] "typescript"
          lspCallHierarchyMode c `shouldBe` CallHierarchyAuto
          lspCallHierarchyMaxSymbols c `shouldBe` 5
          lspTimeoutSeconds c `shouldBe` 300
          lspMaxRestarts c `shouldBe` 1
          isNothing (lspInitializationOptions c) `shouldBe` True
    it "round-trips through JSON" $ do
      let c = (lspServer "gopls" [] "go") { lspCallHierarchy = "off", lspMaxRestarts = 0 }
      (eitherDecode (encode c) :: Either String LSPServerConfig) `shouldBe` Right c

  describe "validateLSPServerEntry" $ do
    let base = lspServer "typescript-language-server" ["--stdio"] "typescript"
        failsNaming c key = case validateLSPServerEntry ".ts" c of
          Left msg -> do
            msg `shouldContain` ".ts"
            msg `shouldContain` key
          Right () -> expectationFailure ("accepted an invalid " ++ key)
    it "accepts the defaults" $ validateLSPServerEntry ".ts" base `shouldBe` Right ()
    it "rejects an unknown call_hierarchy" $ failsNaming base { lspCallHierarchy = "always" } "call_hierarchy"
    it "rejects a non-positive timeout_seconds" $ failsNaming base { lspTimeoutSeconds = 0 } "timeout_seconds"
    it "rejects a negative call_hierarchy_max_symbols" $
      failsNaming base { lspCallHierarchyMaxSymbols = -1 } "call_hierarchy_max_symbols"
    it "rejects a negative max_restarts" $ failsNaming base { lspMaxRestarts = -1 } "max_restarts"
    it "runs on the lsp: section of a config document" $ do
      let doc = object ["lsp" .= object [".ts" .= object ["command" .= ("x" :: Text), "call_hierarchy" .= ("always" :: Text)]]]
      case configSectionErrors doc of
        [msg] -> msg `shouldContain` "call_hierarchy"
        other -> expectationFailure (show other)

  describe "incomingCallToEdge (lsp-edge-extraction: caller attribution)" $ do
    let root = "/work/p"
        callerPath = callerNodePath root "src/retry.ts" "/work/p/src/retry.ts"
        call fromObj = object ["from" .= fromObj, "fromRanges" .= ([] :: [Value])]
    it "attributes a cross-file caller to its own file" $ do
      let e = incomingCallToEdge callerPath "src/retry.ts" "verdict"
                (call (object ["name" .= ("wrap" :: Text), "uri" .= ("file:///work/p/src/factory.ts" :: Text)]))
      fmap edgeSource e `shouldBe` Just (makeNodeId "src/factory.ts" "wrap")
      fmap edgeTarget e `shouldBe` Just (makeNodeId "src/retry.ts" "verdict")
      fmap edgeRelation e `shouldBe` Just Calls
    it "produces no edge without a file URI" $ do
      incomingCallToEdge callerPath "src/retry.ts" "verdict" (call (object ["name" .= ("wrap" :: Text)]))
        `shouldBe` Nothing
      incomingCallToEdge callerPath "src/retry.ts" "verdict"
        (call (object ["name" .= ("wrap" :: Text), "uri" .= ("untitled:x" :: Text)])) `shouldBe` Nothing
    it "keeps the ./ form of the scanned paths" $
      callerNodePath "/work/p" "./a.ts" "/work/p/a.ts" "/work/p/lib/b.ts" `shouldBe` "./lib/b.ts"
    it "keeps the scan-directory prefix of the scanned paths" $
      callerNodePath "/r/proj" "proj/a.ts" "/r/proj/a.ts" "/r/proj/b.ts" `shouldBe` "proj/b.ts"
    it "keeps an absolute path for a caller outside the root" $
      callerNodePath "/work/p" "./a.ts" "/work/p/a.ts" "/other/b.ts" `shouldBe` "/other/b.ts"

  describe "call_hierarchy mode gate" $ do
    it "auto follows the advertised capability" $ do
      callHierarchyGate CallHierarchyAuto True False `shouldBe` True
      callHierarchyGate CallHierarchyAuto False False `shouldBe` False
    it "on ignores the capability until MethodNotFound" $ do
      callHierarchyGate CallHierarchyOn False False `shouldBe` True
      callHierarchyGate CallHierarchyOn True True `shouldBe` False
    it "off never runs" $ callHierarchyGate CallHierarchyOff True False `shouldBe` False
    it "capSymbols: 0 means every symbol" $ do
      length (capSymbols 0 [1 .. 12 :: Int]) `shouldBe` 12
      length (capSymbols 5 [1 .. 12 :: Int]) `shouldBe` 5
    it "isMethodNotFound recognises -32601" $ do
      isMethodNotFound (object ["id" .= (3 :: Int), "error" .= object ["code" .= (-32601 :: Int)]]) `shouldBe` True
      isMethodNotFound (object ["id" .= (3 :: Int), "result" .= Null]) `shouldBe` False

  describe "extractCallHierarchy against a fake server" $ do
    let syms = [fn "verdict" 3]
        twelve = [fn (T.pack ("f" ++ show i)) (i * 4) | i <- [1 .. 12 :: Int]]
    it "auto with an unadvertised capability sends nothing" $
      withFakeServer (sessionCfg CallHierarchyAuto 5) False callingServer $ \c ms -> do
        extractCallHierarchy c "/work/p/a.ts" syms `shouldReturn` []
        readIORef ms `shouldReturn` []
    it "off sends nothing even when advertised" $
      withFakeServer (sessionCfg CallHierarchyOff 5) True callingServer $ \c ms -> do
        extractCallHierarchy c "/work/p/a.ts" syms `shouldReturn` []
        readIORef ms `shouldReturn` []
    it "auto with the capability returns the incoming calls as Calls edges" $
      withFakeServer (sessionCfg CallHierarchyAuto 5) True callingServer $ \c _ -> do
        es <- extractCallHierarchy c "/work/p/a.ts" syms
        map edgeSource es `shouldBe` [makeNodeId "/work/p/b.ts" "wrap"]
    it "on ignores the capability and returns one Calls edge" $
      withFakeServer (sessionCfg CallHierarchyOn 5) False callingServer $ \c ms -> do
        es <- extractCallHierarchy c "/work/p/a.ts" syms
        length es `shouldBe` 1
        countOf "textDocument/prepareCallHierarchy" <$> readIORef ms `shouldReturn` 1
    it "on stops for the session after MethodNotFound" $
      withFakeServer (sessionCfg CallHierarchyOn 5) False methodNotFound $ \c ms -> do
        _ <- extractCallHierarchy c "/work/p/a.ts" twelve
        _ <- extractCallHierarchy c "/work/p/b.ts" twelve
        countOf "textDocument/prepareCallHierarchy" <$> readIORef ms `shouldReturn` 1
        readIORef (lspCallHierarchyDisabled c) `shouldReturn` True
    it "asks every symbol when call_hierarchy_max_symbols is 0" $
      withFakeServer (sessionCfg CallHierarchyAuto 0) True callingServer $ \c ms -> do
        _ <- extractCallHierarchy c "/work/p/a.ts" twelve
        countOf "textDocument/prepareCallHierarchy" <$> readIORef ms `shouldReturn` 12
    it "asks the first 5 symbols by default" $
      withFakeServer (sessionCfg CallHierarchyAuto 5) True callingServer $ \c ms -> do
        _ <- extractCallHierarchy c "/work/p/a.ts" twelve
        countOf "textDocument/prepareCallHierarchy" <$> readIORef ms `shouldReturn` 5

  describe "serverExitStatus" $
    it "reports the exit status of a dead server" $
      withFakeServer (sessionCfg CallHierarchyAuto 5) False callingServer $ \c0 _ -> do
        (_, _, Just errh, ph) <- createProcess (proc "sh" ["-c", "exit 3"]) { std_err = CreatePipe }
        _ <- waitForProcess ph
        serverExitStatus c0 { lspHandle = ph, lspStderr = errh } `shouldReturn` Just 3

  describe "extractFilesWithRestart (lsp-session: bounded restart)" $ do
    it "recovers one death with a restart" $ do
      fr <- newFakeRun
      (exts, _) <- extractFilesWithRestart (fakeOps fr (Map.fromList [(1, "f3.ts")])) "fake" 1 1 tenFiles
      length exts `shouldBe` 10
      errorsOf exts `shouldBe` []
      readIORef (frSessions fr) `shouldReturn` 2
      extracted <- readIORef (frExtracted fr)
      [fp | (2, fp) <- extracted] `shouldBe` drop 2 tenFiles
    it "gives up when the restarts are exhausted" $ do
      fr <- newFakeRun
      (exts, _) <- extractFilesWithRestart
        (fakeOps fr (Map.fromList [(1, "f3.ts"), (2, "f5.ts")])) "fake" 1 1 tenFiles
      map eeFile (errorsOf exts) `shouldBe` drop 4 tenFiles
      all ((== "lsp") . eeExtractor) (errorsOf exts) `shouldBe` True
      logLines <- readIORef (frLog fr)
      filter ("6 files not extracted (last exit status 1)" `T.isInfixOf`) logLines `shouldSatisfy` (\l -> length l == 1)
      any ("fake" `T.isInfixOf`) logLines `shouldBe` True
    it "starts no new session when max_restarts is 0" $ do
      fr <- newFakeRun
      (exts, _) <- extractFilesWithRestart (fakeOps fr (Map.fromList [(1, "f3.ts")])) "fake" 0 1 tenFiles
      map eeFile (errorsOf exts) `shouldBe` drop 2 tenFiles
      readIORef (frSessions fr) `shouldReturn` 1

  describe "typescript-language-server (integration)" $
    it "stays connected 10 s after initialize" $ do
      mExe <- findExecutable "typescript-language-server"
      case mExe of
        Nothing -> pendingWith "typescript-language-server is not on PATH"
        Just exe -> do
          r <- connectToLSP ((defaultLSPConfig "/tmp") { lspCommand = exe, lspArgs = ["--stdio"], lspTimeout = 60 })
          case r of
            Left err -> expectationFailure (T.unpack err)
            Right client -> do
              threadDelay 10000000
              alive <- isServerConnected client
              disconnectLSP client
              alive `shouldBe` True
