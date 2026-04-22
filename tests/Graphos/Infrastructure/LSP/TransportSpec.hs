module Graphos.Infrastructure.LSP.TransportSpec where

import Control.Concurrent.MVar (newMVar)
import Data.IORef (newIORef, readIORef)
import System.Process (ProcessHandle, createProcess, proc, terminateProcess, StdStream(CreatePipe), std_in, std_out, std_err)
import Test.Hspec

import Graphos.Infrastructure.LSP.Protocol (ServerCapabilities(..), lspShutdown)
import Graphos.Infrastructure.LSP.Transport
  ( LSPClient(..)
  , LSPClientConfig(..)
  , isProcessAlive
  , markDisconnected
  , sendLSPMessageSafe
  )

-- | Build a minimal LSPClient for testing connection-state logic.
-- We spawn a short-lived process (cat) so we can observe the alive check.
mkTestClient :: IO LSPClient
mkTestClient = do
  (Just inh, Just outh, _, ph) <- createProcess (proc "cat" [])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  idVar <- newMVar 100
  connectedRef <- newIORef True
  pure LSPClient
    { lspHandle     = ph
    , lspStdin      = inh
    , lspStdout     = outh
    , lspConfig     = LSPClientConfig "" [] "" 5
    , lspMessageId  = idVar
    , lspServerCaps = ServerCapabilities False False False False False
    , lspConnected  = connectedRef
    }

spec :: Spec
spec = do
  describe "isProcessAlive" $ do
    it "returns True for a running process" $ do
      client <- mkTestClient
      alive <- isProcessAlive (lspHandle client)
      alive `shouldBe` True
      terminateProcess (lspHandle client)

    it "returns False after the process is terminated" $ do
      client <- mkTestClient
      terminateProcess (lspHandle client)
      -- Give the OS a moment to reap the process
      _ <- waitForExit (lspHandle client)
      alive <- isProcessAlive (lspHandle client)
      alive `shouldBe` False

  describe "markDisconnected" $ do
    it "sets lspConnected to False" $ do
      client <- mkTestClient
      readIORef (lspConnected client) `shouldReturn` True
      markDisconnected client
      readIORef (lspConnected client) `shouldReturn` False
      terminateProcess (lspHandle client)

  describe "sendLSPMessageSafe" $ do
    it "returns True when the server is alive" $ do
      client <- mkTestClient
      result <- sendLSPMessageSafe client lspShutdown
      result `shouldBe` True
      terminateProcess (lspHandle client)

    it "returns False when already marked disconnected" $ do
      client <- mkTestClient
      markDisconnected client
      result <- sendLSPMessageSafe client lspShutdown
      result `shouldBe` False
      terminateProcess (lspHandle client)

    it "returns False and marks disconnected when the process is dead" $ do
      client <- mkTestClient
      terminateProcess (lspHandle client)
      _ <- waitForExit (lspHandle client)
      result <- sendLSPMessageSafe client lspShutdown
      result `shouldBe` False
      readIORef (lspConnected client) `shouldReturn` False

-- | Helper: block until the process exits (or timeout after 2s).
waitForExit :: ProcessHandle -> IO ()
waitForExit = go
  where
    go _ph = do
      alive <- isProcessAlive _ph
      if alive then go _ph else pure ()