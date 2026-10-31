-- | Secure execution of @curl@ invocations.
--
-- Background: AVI-1487 reported that API keys and database passwords were passed
-- to @curl@ as process arguments, exposing them to every local user for the
-- duration of the call via @ps@ and @/proc/<pid>/cmdline@ (CWE-778), and that
-- payload files were written to predictable, world-readable paths (CWE-377 /
-- CWE-400). This module closes both holes without pulling in a new dependency:
--
--   * 'runCurl' routes sensitive option values through a mode-0600 temporary
--     @curl --config@ file, so credentials never enter the argument vector.
--
--   * 'withSecureTempFile' writes payloads to a mode-0600 file inside a private
--     mode-0700 per-process directory and always deletes it afterwards.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Http
  ( runCurl
  , CurlResult(..)
  , withSecureTempFile
  ) where

import Control.Exception (bracket, catch, SomeException)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  (createDirectoryIfMissing, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath, takeFileName)
import System.IO (openTempFile, hClose)
import System.Process (readProcessWithExitCode)
import System.Posix.Files (setFileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Types (FileMode)

-- | Result of a @curl@ call, normalised to @Text@ output / @Text@ error.
data CurlResult
  = CurlSuccess Text
  | CurlFailed Int Text
  deriving (Eq, Show)

-- | Run @curl@ without ever placing sensitive material in the argument vector.
--
-- Arguments are split into three groups:
--
--   * 'first' - non-secret options applied before the config (@-s@, @--max-time@,
--     @-X POST@, non-sensitive @-H@ headers such as the content-type).
--   * 'secrets' - @(option, value)@ pairs whose values carry sensitive material
--     @(auth header), @-u@ basic-auth). These are
--     written to the temporary @--config@ file rather than passed on the command
--     line, so they never appear in @/proc/<pid>/cmdline@.
--   * 'trailing' - non-secret options applied after the config (@--data-binary@,
--     the endpoint URL).
runCurl :: [String] -> [(String, String)] -> [String] -> IO CurlResult
runCurl first secrets trailing =
  withSecureTempFile "config" (BSL8.pack (buildConfig secrets)) $ \cfgPath -> do
    let args = first ++ ["--config", cfgPath] ++ trailing
    (code, out, err) <- readProcessWithExitCode "curl" args ""
    pure $ case code of
      ExitSuccess -> CurlSuccess (T.pack out)
      ExitFailure n -> CurlFailed n (T.pack err)

-- | Render sensitive @(option, value)@ pairs as @curl --config@ lines. Each
-- value is double-quote wrapped with backslash/double-quote escaping so that
-- arbitrary tokens and passwords survive the config parser intact.
buildConfig :: [(String, String)] -> String
buildConfig = unlines . fmap line
  where
    line (opt, val) = opt <> " " <> quote val
    quote s = "\"" <> escape s <> "\""
    -- Escape backslash first, then double-quote, so neither can terminate the
    -- quoted string early.
    escape = concatMap step
      where
        step '\\' = "\\\\"
        step '"'  = "\\\""
        step c    = [c]

-- | Run @cont@ with the path of a throwaway, owner-only temp file named @label@
-- containing @content@, then always delete it. The file lives in a private
-- mode-0700 per-process directory, is written mode-0600, and is removed after.
--
-- The private directory defeats symlink-following writes (an unprivileged user
-- cannot drop a file here); the mode-0600 file defeats read-based disclosure.
-- The per-process directory name keeps concurrent invocations isolated while
-- letting nested 'runCurl'/'withSecureTempFile' calls share one directory.
withSecureTempFile :: FilePath -> BSL8.ByteString -> (FilePath -> IO a) -> IO a
withSecureTempFile label content cont = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  -- Private per-process directory: never followable for an unprivileged user.
  let dir = joinPath [tmp, "graphos-" ++ show pid]
  createDirectoryIfMissing True dir
  setFileMode dir (0o700 :: FileMode)
  -- Unique name (O_EXCL) defeats symlink-following and concurrent races.
  let safeLabel = takeFileName label
  (path, h) <- openTempFile dir ("graphos-" ++ safeLabel ++ "-XXXXXX")
  -- Lock the mode down before writing anything, so even a briefly-live file
  -- is never world-readable (CWE-377).
  setFileMode path (0o600 :: FileMode)
  hClose h
  bracket
    (do BSL8.writeFile path content
        setFileMode path (0o600 :: FileMode)
        pure path)
    (\p -> removeFile p `catch` \(_ :: SomeException) -> pure ())
    cont
