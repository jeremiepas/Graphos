-- | LSP transport layer — JSON-RPC message sending/receiving and process management.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LSP.Transport
  ( -- * LSP Client types
    LSPClient(..)
  , LSPClientConfig(..)
  , defaultLSPConfig

    -- * Lifecycle
  , connectToLSP
  , disconnectLSP

    -- * Low-level messaging
  , sendLSPMessage
  , sendLSPMessageSafe
  , readLSPMessage
  , readLSPResponseForId
  , drainNotifications

    -- * Connection state
  , isProcessAlive
  , markDisconnected
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, try, SomeException(..), IOException)
import Control.Monad (unless, void)
import Data.Aeson (ToJSON, encode, eitherDecode, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (ProcessHandle, createProcess, proc, std_in, std_out, std_err, StdStream(CreatePipe), terminateProcess, getProcessExitCode)
import System.IO (Handle, hFlush)
import System.Timeout (timeout)

import Graphos.Infrastructure.LSP.Protocol
import Graphos.Infrastructure.LSP.CapabilityParse (parseServerCapabilities)

-- ───────────────────────────────────────────────
-- LSP Client types
-- ───────────────────────────────────────────────

data LSPClientConfig = LSPClientConfig
  { lspCommand    :: FilePath
  , lspArgs       :: [String]
  , lspRootUri    :: FilePath
  , lspTimeout    :: Int  -- ^ seconds; HLS needs 300s+ due to cabal v2-repl setup
  } deriving (Eq, Show)

defaultLSPConfig :: FilePath -> LSPClientConfig
defaultLSPConfig root = LSPClientConfig
  { lspCommand = ""
  , lspArgs     = []
  , lspRootUri  = root
  , lspTimeout  = 300  -- 5 min default; HLS needs time for cabal v2-repl
  }

data LSPClient = LSPClient
  { lspHandle     :: ProcessHandle
  , lspStdin      :: Handle
  , lspStdout     :: Handle
  , lspConfig     :: LSPClientConfig
  , lspMessageId  :: MVar Int
  , lspServerCaps :: ServerCapabilities
  , lspConnected  :: IORef Bool  -- ^ True while the server process is alive and pipes are open
  }

-- ───────────────────────────────────────────────
-- LSP Protocol: sending and receiving messages
-- ───────────────────────────────────────────────

-- | Send a JSON-RPC message with proper Content-Length framing
sendLSPMessage :: ToJSON a => Handle -> a -> IO ()
sendLSPMessage h msg = do
  let content = BSL.toStrict (encode msg)
      contentLen = BS.length content
      header = B8.pack $ "Content-Length: " ++ show contentLen ++ "\r\n\r\n"
  BS.hPut h (header `BS.append` content)
  hFlush h

-- | Check if the LSP server process is still running.
-- Returns 'Nothing' when the process is still running (no exit code yet).
isProcessAlive :: ProcessHandle -> IO Bool
isProcessAlive ph = do
  mec <- getProcessExitCode ph
  pure $ case mec of
    Nothing -> True
    Just _  -> False

-- | Mark the LSP client as disconnected (e.g. after detecting a dead process).
markDisconnected :: LSPClient -> IO ()
markDisconnected client = writeIORef (lspConnected client) False

-- | Send a JSON-RPC message with Broken-pipe protection.
-- Checks if the server process is alive before writing; on any IOException
-- (including ResourceVanished / Broken pipe), marks the client as disconnected
-- and returns 'False'.  Returns 'True' on success.
sendLSPMessageSafe :: ToJSON a => LSPClient -> a -> IO Bool
sendLSPMessageSafe client msg = do
  alive <- readIORef (lspConnected client)
  if not alive
    then pure False
    else do
      processAlive <- isProcessAlive (lspHandle client)
      if not processAlive
        then do
          markDisconnected client
          putStrLn "[lsp] Warning: server process is no longer running"
          pure False
        else catch (do
          sendLSPMessage (lspStdin client) msg
          pure True
          ) $ \(e :: IOException) -> do
            markDisconnected client
            putStrLn $ "[lsp] Warning: failed to send message (connection lost): " ++ show e
            pure False

-- | Read a byte until newline, stripping \r
readLineLF :: Handle -> IO String
readLineLF h = do
  let loop acc = do
        c <- BS.hGet h 1
        if BS.null c
          then pure (reverse acc)
          else case BS.head c of
            10 -> pure (reverse acc)   -- \n = end of line
            13 -> loop acc             -- \r = skip
            b  -> loop (toEnum (fromEnum b) : acc)
  loop []

-- | Read a single LSP message (Content-Length header + JSON body)
readLSPMessage :: Handle -> IO (Either String Value)
readLSPMessage outh = catch (do
  let findHeader = do
        line <- readLineLF outh
        if null line
          then findHeader
          else do
            let contentLen = parseContentLength line
            if contentLen < 0
              then findHeader
              else do
                skipHeaders
                result <- timeout 10000000 (BSL.hGet outh contentLen)
                case result of
                  Nothing -> pure $ Left "Timeout reading body"
                  Just bodyBytes -> case eitherDecode bodyBytes of
                    Right val -> pure $ Right val
                    Left err  -> pure $ Left $ "JSON parse error (" ++ show contentLen ++ " bytes): " ++ err
  mResult <- timeout 10000000 findHeader
  case mResult of
    Nothing -> pure $ Left "Timeout waiting for LSP response"
    Just result -> pure result
  ) $ \(e :: SomeException) -> pure $ Left $ "Read error: " ++ show e
  where
    skipHeaders = do
      line <- readLineLF outh
      if null line then pure ()
      else skipHeaders

-- | Parse Content-Length from header line
parseContentLength :: String -> Int
parseContentLength header =
  let prefix :: String = "Content-Length:"
      trimmed = dropWhile (== ' ') $ drop (length prefix) (takeWhile (/= '\r') header)
  in case reads trimmed of
       [(n, "")] -> n
       _ -> -1

-- | Read LSP messages until we get one with a matching "id" field
readLSPResponseForId :: Handle -> Int -> IO (Either String Value)
readLSPResponseForId outh targetId = loop
  where
    loop = do
      result <- readLSPMessage outh
      case result of
        Left err -> pure $ Left err
        Right val@(Object o) ->
          case KM.lookup "id" o of
            Just (Aeson.Number n) | round n == targetId -> pure $ Right val
            Just (Aeson.Number _) -> loop
            _ -> loop
        Right _ -> loop

-- | Drain all pending notifications from LSP server
drainNotifications :: Handle -> Int -> IO ()
drainNotifications outh micros = do
  mMsg <- timeout micros (readLSPMessage outh)
  case mMsg of
    Just (Right _) -> drainNotifications outh micros
    _ -> pure ()

-- ───────────────────────────────────────────────
-- LSP Client lifecycle
-- ───────────────────────────────────────────────

connectToLSP :: LSPClientConfig -> IO (Either Text LSPClient)
connectToLSP config = catch (do
  putStrLn $ "[lsp] Starting: " ++ lspCommand config ++ " " ++ unwords (lspArgs config)
  let processSpec = proc (lspCommand config) (lspArgs config)
  (minH, moutH, _, ph) <- createProcess processSpec
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  case (minH, moutH) of
    (Just inh, Just outh) -> do
      idVar <- newMVar 2
      connectedRef <- newIORef True

      let initMsg = lspInitialize (lspRootUri config)
      sendLSPMessage inh initMsg

      -- HLS needs significantly more time to initialize because it runs
      -- cabal v2-repl to determine build flags (can take minutes on first run).
      -- Other LSP servers typically respond within seconds.
      let initTimeoutMicros = case lspCommand config of
            cmd | "haskell-language-server" `isInfixOf` cmd -> max (lspTimeout config) 300 * 1000000
                | otherwise -> lspTimeout config * 1000000
      initResp <- timeout initTimeoutMicros (readLSPResponseForId outh 1)
      case initResp of
        Nothing -> do
          putStrLn $ "[lsp] Initialize failed: Timeout (" ++ show (lspTimeout config) ++ "s) waiting for LSP response"
          putStrLn $ "[lsp] Tip: HLS needs cabal v2-repl to resolve build flags — this can take minutes on first run"
          terminateProcess ph
          pure $ Left $ T.pack $ "LSP initialize failed: Timeout (" ++ show (lspTimeout config) ++ "s) waiting for LSP response"
        Just (Left err) -> do
          putStrLn $ "[lsp] Initialize failed: " ++ err
          terminateProcess ph
          pure $ Left $ T.pack $ "LSP initialize failed: " ++ err
        Just (Right respVal) -> do
          putStrLn "[lsp] Initialize successful"
          sendLSPMessage inh lspInitialized

          let drainMicros = case lspCommand config of
                cmd | "haskell-language-server" `isInfixOf` cmd -> 15000000
                    | otherwise -> 3000000
          drainNotifications outh drainMicros

          let caps = parseServerCapabilities respVal
          putStrLn $ "[lsp] Server capabilities: documentSymbol=" ++ show (scpDocumentSymbolProvider caps)
                   ++ " workspaceSymbol=" ++ show (scpWorkspaceSymbolProvider caps)

          putStrLn $ "[lsp] Connected to " ++ lspCommand config
          pure $ Right LSPClient
            { lspHandle     = ph
            , lspStdin      = inh
            , lspStdout     = outh
            , lspConfig     = config
            , lspMessageId  = idVar
            , lspServerCaps = caps
            , lspConnected  = connectedRef
            }
    _ -> pure $ Left $ T.pack "Failed to create LSP process handles"
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "LSP connection error: " ++ show e

disconnectLSP :: LSPClient -> IO ()
disconnectLSP client = do
  alive <- readIORef (lspConnected client)
  unless (not alive) $ do
    result <- try $ do
      _ <- sendLSPMessageSafe client lspShutdown
      _ <- timeout 3000000 (readLSPResponseForId (lspStdout client) 999)
      pure ()
    case result of
      Left (_ :: SomeException) -> pure ()
      Right _ -> pure ()
    void (catch (sendLSPMessageSafe client lspExit) $ \(_ :: SomeException) -> pure False)
  markDisconnected client
  threadDelay 100000
  terminateProcess (lspHandle client)
  putStrLn $ "[lsp] Disconnected from " ++ lspCommand (lspConfig client)