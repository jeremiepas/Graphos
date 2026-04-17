{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Server.Static
  ( startStaticServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, setBeforeMainLoop, defaultSettings)
import Network.HTTP.Types (status200, status404, status405, hContentType, methodGet, methodOptions)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import System.Directory (doesFileExist, canonicalizePath)
import System.FilePath ((</>), takeExtension, normalise, makeRelative)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (catch, SomeException(..))

import Graphos.Infrastructure.Logging (LogLevel(..), defaultLogEnv, logInfo)

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
  env <- defaultLogEnv LevelInfo
  let app = staticApp dir
      warpSettings = setPort port
                   $ setHost "0.0.0.0"
                   $ setBeforeMainLoop (logInfo env $ T.pack $ "Serving " ++ dir ++ " at http://localhost:" ++ show port ++ "/graph.html")
                   $ defaultSettings
  runSettings warpSettings app