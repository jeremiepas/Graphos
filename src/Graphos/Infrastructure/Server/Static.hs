{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Server.Static
  ( startStaticServer
  , startServeServer
  , staticApp
  , staticCandidates
  , firstExisting
  , FileCandidate(..)
  ) where

import Network.Wai
import Data.String (fromString)
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, setBeforeMainLoop, defaultSettings)
import Network.HTTP.Types (status200, status404, status405, hContentType, methodGet, methodOptions)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import System.Directory (doesFileExist, doesDirectoryExist, canonicalizePath)
import System.FilePath
  ( (</>), takeExtension, normalise, makeRelative, takeDirectory, dropTrailingPathSeparator
  , isAbsolute, splitDirectories )
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Control.Monad (filterM)
import Control.Exception (catch, SomeException(..))
import Data.IORef (IORef, newIORef)

import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo)
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult)
import Graphos.Infrastructure.Server.QueryAPI (apiAppRef, SharedLoad(..))

mimeTypes :: Map.Map String BS8.ByteString
mimeTypes = Map.fromList
  [ (".html", "text/html; charset=utf-8")
  , (".json", "application/json")
  , (".js",   "application/javascript")
  , (".css",  "text/css; charset=utf-8")
  , (".svg",  "image/svg+xml")
  , (".png",  "image/png")
  , (".ico",  "image/x-icon")
  , (".woff", "font/woff")
  , (".woff2", "font/woff2")
  , (".map",  "application/json")
  , (".md",   "text/markdown; charset=utf-8")
  , (".txt",  "text/plain; charset=utf-8")
  ]

mimeTypeFor :: String -> BS8.ByteString
mimeTypeFor ext = Map.findWithDefault "application/octet-stream" ext mimeTypes

-- | A candidate file to serve, paired with the directory it must stay inside.
-- The root is what makes the traversal guard checkable per directory: the
-- resolved path must remain under its own root.
data FileCandidate = FileCandidate
  { fcRoot :: FilePath
  , fcPath :: FilePath
  } deriving (Eq, Show)

-- | Directories that may hold the frontend bundle. The bundle directory
-- itself comes first (the Studio's @index.html@/@studio.js@/@vendor/@ output,
-- or a published bundle that carries the viewer assets at its root), followed
-- by the viewer layouts a @graphos-studio@ checkout keeps beside the Studio
-- output (@viewer/@ and @assets/viewer/@). This makes both
-- @--frontend-dir graphos-studio@ and @--frontend-dir graphos-studio/public@
-- resolve @viewer.js@, @viewer.css@ and the vendored renderer.
frontendAssetRoots :: FilePath -> [FilePath]
frontendAssetRoots frontendDir = nub
  [ frontendDir
  , frontendDir </> "viewer"
  , frontendDir </> "assets" </> "viewer"
  , takeDirectory (dropTrailingPathSeparator frontendDir) </> "assets" </> "viewer"
  ]

-- | Ordered candidates for one request: the graph output directory first
-- (so @graph.html@ and the exported views win), then the frontend bundle.
--
-- An empty request path serves @graph.html@ from the output directory and
-- falls back to the bundle's @index.html@ (the Studio) when the graph has not
-- been exported yet.
staticCandidates :: FilePath -> Maybe FilePath -> [T.Text] -> [FileCandidate]
staticCandidates outputDir mFrontendDir pathParts = outputCandidates ++ frontendCandidates
  where
    relPath = foldr (</>) "" (map T.unpack pathParts)
    outputCandidates
      | null relPath = [FileCandidate outputDir (outputDir </> "graph.html")]
      | otherwise    = [FileCandidate outputDir (outputDir </> relPath)]
    frontendCandidates = case mFrontendDir of
      Nothing -> []
      Just frontendDir
        | null relPath -> [FileCandidate frontendDir (frontendDir </> "index.html")]
        | otherwise    ->
            [ FileCandidate root (root </> relPath)
            | root <- frontendAssetRoots frontendDir
            ]

-- | First candidate that exists and stays inside its root.
firstExisting :: [FileCandidate] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (FileCandidate root path : rest) = do
  cRoot <- canonicalizePath root `catch` \(_ :: SomeException) -> pure (normalise root)
  cPath <- canonicalizePath path `catch` \(_ :: SomeException) -> pure "/dev/null"
  -- 'makeRelative' returns the absolute path unchanged when it is not under
  -- the root, so both that and an explicit parent component mean "escaped".
  let rel = makeRelative cRoot cPath
      escaped = isAbsolute rel || case splitDirectories rel of
        ("..":_) -> True
        _        -> False
  if not escaped
    then do
      exists <- doesFileExist cPath
      if exists then pure (Just cPath) else firstExisting rest
    else firstExisting rest

staticApp :: FilePath -> Maybe FilePath -> Application
staticApp rootDir mFrontendDir req respond = do
  let method = requestMethod req
  if method == methodOptions
    then respond $ responseLBS status200 corsHeaders ""
    else if method /= methodGet
      then respond $ responseLBS status405 [] "Method not allowed"
      else do
        found <- firstExisting (staticCandidates rootDir mFrontendDir (pathInfo req))
        case found of
          Just cPath -> do
            contents <- BSL.readFile cPath
            let mime = mimeTypeFor (takeExtension cPath)
            respond $ responseLBS status200
              ((hContentType, mime) : corsHeaders) contents
          Nothing -> respond $ responseLBS status404 corsHeaders "Not found"
  where
    corsHeaders = [("Access-Control-Allow-Origin", "*")]

-- | Resolve the frontend bundle: an explicit @--frontend-dir@ is used as
-- given; otherwise the conventional @graphos-studio@ checkouts beside the
-- current directory are tried, so @graphos serve@ works out of the box when
-- the frontend repository is checked out next to this one.
resolveFrontendDir :: Maybe FilePath -> IO (Maybe FilePath)
resolveFrontendDir (Just fd) = pure (Just fd)
resolveFrontendDir Nothing =
  listToMaybe <$> filterM doesDirectoryExist
    [ "graphos-studio/public", "../graphos-studio/public" ]

-- | Announce the frontend bundle and warn when it is absent: serving still
-- works for the exported graph, but the viewer assets would 404.
describeFrontend :: Bool -> (Maybe FilePath, Maybe FilePath) -> IO ()
describeFrontend apiOnly (requested, resolved) = case (apiOnly, resolved) of
  (True, _) -> pure ()
  (False, Nothing) ->
    hPutStrLn stderr "[serve] No frontend bundle found; only the graph output is served. Pass --frontend-dir pointing at graphos-studio/public."
  (False, Just fd) -> do
    present <- doesDirectoryExist fd
    case (requested, present) of
      (Just _, False) ->
        hPutStrLn stderr $ "[serve] Frontend bundle not found at " ++ fd
          ++ "; viewer assets will 404 (pass --frontend-dir pointing at graphos-studio/public)."
      _ -> hPutStrLn stderr $ "[serve] Serving frontend bundle from " ++ fd

startStaticServer :: FilePath -> Maybe FilePath -> Int -> IO ()
startStaticServer dir requestedFrontendDir port = do
  env <- defaultLogEnv LogInfo
  mFrontendDir <- resolveFrontendDir requestedFrontendDir
  describeFrontend False (requestedFrontendDir, mFrontendDir)
  let app = staticApp dir mFrontendDir
      warpSettings = setPort port
                   $ setHost "127.0.0.1"
                   $ setBeforeMainLoop (logInfo env $ T.pack $ "Serving " ++ dir ++ " at http://localhost:" ++ show port ++ "/graph.html")
                   $ defaultSettings
  runSettings warpSettings app

-- | Combined static + query API server.
startServeServer :: FilePath -> Maybe FilePath -> FilePath -> Int -> Bool -> Bool -> String -> IO ()
startServeServer dir requestedFrontendDir graphPath port apiOnly noApi bindHost = do
  env <- defaultLogEnv LogInfo
  mFrontendDir <- resolveFrontendDir requestedFrontendDir
  describeFrontend apiOnly (requestedFrontendDir, mFrontendDir)
  loadResult <- loadGraphFromFile graphPath
  case loadResult of
    Left err -> do
      hPutStrLn stderr $ "[serve] Error loading graph: " ++ T.unpack err
      exitWith (ExitFailure 1)
    Right lr -> do
      ref <- newIORef lr
      let app = serveApp dir mFrontendDir graphPath ref apiOnly noApi
          warpSettings = setPort port
                       $ setHost (fromString bindHost)
                       $ setBeforeMainLoop (logInfo env $ T.pack $ "Serving " ++ dir ++ " + API on http://" ++ bindHost ++ ":" ++ show port)
                       $ defaultSettings
      runSettings warpSettings app

serveApp :: FilePath -> Maybe FilePath -> FilePath -> IORef LoadResult -> Bool -> Bool -> Application
serveApp dir mFrontendDir graphPath ref apiOnly noApi req respond
  | apiOnly   = apiAppHandler graphPath ref req respond
  | noApi     = staticApp dir mFrontendDir req respond
  | otherwise =
      case pathInfo req of
        ("api":_) -> apiAppHandler graphPath ref req respond
        _         -> staticApp dir mFrontendDir req respond

-- | The API surface over the shared mutable load state: reads serve the
-- live graph; /api/cypher/mutate replaces it in memory (and persists back
-- to graphPath when requested).
apiAppHandler :: FilePath -> IORef LoadResult -> Application
apiAppHandler graphPath ref = apiAppRef (SharedLoadRef ref graphPath)
