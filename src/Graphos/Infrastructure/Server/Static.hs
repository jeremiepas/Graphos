{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Server.Static
  ( startStaticServer
  , startServeServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, setBeforeMainLoop, defaultSettings)
import Network.HTTP.Types (status200, status404, status405, hContentType, methodGet, methodOptions)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath ((</>), takeExtension, normalise, makeRelative)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map.Strict as Map
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
  , (".md",   "text/markdown; charset=utf-8")
  , (".txt",  "text/plain; charset=utf-8")
  ]

mimeTypeFor :: String -> BS8.ByteString
mimeTypeFor ext = Map.findWithDefault "application/octet-stream" ext mimeTypes

staticApp :: FilePath -> Application
staticApp rootDir req respond = do
  let method = requestMethod req
  if method == methodOptions
    then respond $ responseLBS status200 [("Access-Control-Allow-Origin", "*")] ""
    else if method /= methodGet
      then respond $ responseLBS status405 [] "Method not allowed"
      else do
        let pathParts = pathInfo req
            relPath = foldr (</>) "" (map T.unpack pathParts)
            fullPath = normalise (rootDir </> if null relPath then "graph.html" else relPath)
        cRoot <- canonicalizePath rootDir
        cPath <- (canonicalizePath fullPath) `catch` (\(_ :: SomeException) -> pure "/dev/null")
        let rel = makeRelative cRoot cPath
        if null rel || take 2 rel /= ".."
          then do
            exists <- doesFileExist cPath
            if exists
              then do
                contents <- BSL.readFile cPath
                let mime = mimeTypeFor (takeExtension cPath)
                respond $ responseLBS status200
                  [ (hContentType, mime)
                  , ("Access-Control-Allow-Origin", "*")
                  ] contents
              else respond $ responseLBS status404 [("Access-Control-Allow-Origin", "*")] "Not found"
          else respond $ responseLBS status404 [] "Forbidden"

startStaticServer :: FilePath -> Int -> IO ()
startStaticServer dir port = do
  env <- defaultLogEnv LogInfo
  let app = staticApp dir
      warpSettings = setPort port
                   $ setHost "0.0.0.0"
                   $ setBeforeMainLoop (logInfo env $ T.pack $ "Serving " ++ dir ++ " at http://localhost:" ++ show port ++ "/graph.html")
                   $ defaultSettings
  runSettings warpSettings app

-- | Combined static + query API server.
startServeServer :: FilePath -> FilePath -> Int -> Bool -> Bool -> IO ()
startServeServer dir graphPath port apiOnly noApi = do
  env <- defaultLogEnv LogInfo
  loadResult <- loadGraphFromFile graphPath
  case loadResult of
    Left err -> do
      hPutStrLn stderr $ "[serve] Error loading graph: " ++ T.unpack err
      exitWith (ExitFailure 1)
    Right lr -> do
      ref <- newIORef lr
      let app = serveApp dir graphPath ref apiOnly noApi
          warpSettings = setPort port
                       $ setHost "0.0.0.0"
                       $ setBeforeMainLoop (logInfo env $ T.pack $ "Serving " ++ dir ++ " + API on http://localhost:" ++ show port)
                       $ defaultSettings
      runSettings warpSettings app

serveApp :: FilePath -> FilePath -> IORef LoadResult -> Bool -> Bool -> Application
serveApp dir graphPath ref apiOnly noApi req respond
  | apiOnly   = apiAppHandler graphPath ref req respond
  | noApi     = staticApp dir req respond
  | otherwise =
      case pathInfo req of
        ("api":_) -> apiAppHandler graphPath ref req respond
        _         -> staticApp dir req respond

-- | The API surface over the shared mutable load state: reads serve the
-- live graph; /api/cypher/mutate replaces it in memory (and persists back
-- to graphPath when requested).
apiAppHandler :: FilePath -> IORef LoadResult -> Application
apiAppHandler graphPath ref = apiAppRef (SharedLoadRef ref graphPath)