module Graphos.Infrastructure.Server.StaticSpec where

import Test.Hspec
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Graphos.Infrastructure.Server.Static
  ( FileCandidate(..), firstExisting, staticCandidates )

spec :: Spec
spec = do
  describe "staticCandidates" $ do
    let out = "graphos-out"
        bundle = "graphos-studio/public"

    it "serves the exported graph before the frontend bundle" $ do
      staticCandidates out (Just bundle) ["graph.html"]
        `shouldBe` [ FileCandidate out (out <> "/graph.html")
                   , FileCandidate bundle (bundle <> "/graph.html")
                   , FileCandidate (bundle <> "/viewer") (bundle <> "/viewer/graph.html")
                   , FileCandidate (bundle <> "/assets/viewer") (bundle <> "/assets/viewer/graph.html")
                   , FileCandidate "graphos-studio/assets/viewer" "graphos-studio/assets/viewer/graph.html"
                   ]

    it "falls back to the bundle's index.html for the root request" $ do
      staticCandidates out (Just bundle) []
        `shouldBe` [ FileCandidate out (out <> "/graph.html")
                   , FileCandidate bundle (bundle <> "/index.html")
                   ]

    it "resolves viewer assets from the sibling assets/viewer checkout layout" $ do
      let candidates = staticCandidates out (Just bundle) ["viewer.js"]
      fcPath (candidates !! 4) `shouldBe` "graphos-studio/assets/viewer/viewer.js"

    it "resolves viewer assets from the bundle root" $ do
      let candidates = staticCandidates out (Just bundle) ["vendor", "vis-network.min.js"]
      fcPath (candidates !! 1) `shouldBe` bundle <> "/vendor/vis-network.min.js"

    it "serves only the graph output when no frontend bundle is configured" $ do
      staticCandidates out Nothing ["graph.html"]
        `shouldBe` [FileCandidate out (out <> "/graph.html")]

    it "accepts a graphos-studio checkout root as the frontend dir" $ do
      let candidates = staticCandidates out (Just "graphos-studio") ["viewer.css"]
      fcPath (candidates !! 3) `shouldBe` "graphos-studio/assets/viewer/viewer.css"

  describe "firstExisting traversal guard" $ do
    it "serves a candidate inside its own root" $
      withSystemTempDirectory "graphos-static" $ \base -> do
        let root = base </> "out"
        createDirectoryIfMissing True root
        writeFile (root </> "graph.html") "<html></html>"
        found <- firstExisting [FileCandidate root (root </> "graph.html")]
        found `shouldSatisfy` maybe False (\p -> "graph.html" `isSuffixOf` p)

    it "rejects a candidate that escapes its root even when the file exists" $
      withSystemTempDirectory "graphos-static" $ \base -> do
        let root = base </> "out"
        createDirectoryIfMissing True root
        writeFile (base </> "secret.txt") "secret"
        found <- firstExisting [FileCandidate root (root </> ".." </> "secret.txt")]
        found `shouldBe` Nothing

    it "skips a missing candidate and keeps looking" $
      withSystemTempDirectory "graphos-static" $ \base -> do
        let root = base </> "out"
        createDirectoryIfMissing True root
        writeFile (base </> "index.html") "<html></html>"
        found <- firstExisting
          [ FileCandidate root (root </> "graph.html")
          , FileCandidate base (base </> "index.html")
          ]
        found `shouldSatisfy` maybe False (\p -> "index.html" `isSuffixOf` p)

isSuffixOf :: String -> String -> Bool
isSuffixOf suffix s = drop (length s - length suffix) s == suffix
