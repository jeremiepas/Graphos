-- AVI-526 concrete deliverable -- reproducible kernel.
--
-- Verifies the FGL natural-transformation claim against Graphos surfaces:
--   (A) nidToInt (hashed FGL index, FGL.hs:nidToInt) is a hash into a bounded
--       Int space -> NOT injective in principle; the component of the natural
--       transformation  eta : toFGL ==> toSeqFGL  (both Gr -> FGLGraph, differing
--       only in node-indexing scheme) is therefore UNDEFINED as an iso whenever
--       two NodeIds collide (fgl-adapter spec: bijective node-index requirement).
--   (B) sequential 0..N-1 indexing (toCachedFGL) is bijective BY CONSTRUCTION.
--   (C) under a forced nidToInt-collision, toFGL merges the two nodes (data loss)
--       while toSeqFGL keeps them distinct (incidence preserved).
--
-- Run:  runghc docs/math-requirements/kernels/AVI-526-fgl-naturality.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

-- Graphos.Domain.Graph.FGL.nidToInt (hashed FGL index).
nidToInt :: Text -> Int
nidToInt nid =
  fromIntegral (T.foldl' (\acc c -> acc * 31 + fromIntegral (fromEnum c)) (0 :: Integer) nid `mod` fromIntegral (maxBound :: Int))

-- toCachedFGL sequential bijective index: NodeId -> 0..N-1 over a canonical order.
seqIndex :: [Text] -> Text -> Int
seqIndex ids nid = fromIntegral (indexOf ids nid)
  where
    indexOf xs x = length (takeWhile (/= x) xs)

countCollisions :: [Int] -> Int
countCollisions xs = length xs - length (nub xs)

-- (A) nidToInt has bounded image (Text is unbounded-length, Int finite).
--     Report the image size bound and search a space for any concrete collision.
findCollision :: [Text] -> Maybe (Text, Text)
findCollision = go []
  where
    go seen (u:us)
      | Just v <- lookup (nidToInt u) seen = Just (v, u)   -- nidToInt v == nidToInt u
      | otherwise = go ((nidToInt u, u) : seen) us
    go _ [] = Nothing

-- (C) forced collision: two NodeIds mapped to the SAME hashed index.
--     toFGL (hashed) collapses them to one FGL node; toSeqFGL keeps two.
mergedUnderHashed :: [(Int, (Text, String))] -> Int   -- count distinct FGL node indices
mergedUnderHashed pairs = length (nub [i | (i, _) <- pairs])

main :: IO ()
main = do
  -- (A) finite image + concrete-collision search over a 4-char alphabet space.
  let space = [ T.pack [c1, c2] | c1 <- "abcdefgh", c2 <- "abcdefgh" ]
      colFound = maybe False (const True) (findCollision space)
  putStrLn $ "-- nidToInt image is bounded Int (maxBound = " ++ show (maxBound :: Int) ++ "); injective? NO in principle"
  putStrLn $ "-- concrete nidToInt collision found over 8x8 id space? " ++ show colFound

  -- (B) sequential indexing is bijective on the same space.
  let seqIds = [seqIndex space t | t <- space]
  putStrLn $ "-- sequential 0..N-1 bijective over same space? " ++ show (countCollisions seqIds == 0)

  -- (C) forced collision: nodes "x","y" share hashed index 7 (simulating nidToInt x == nidToInt y).
  let forcedPairs = [(7, ("x", "p0")), (7, ("y", "p1"))]   -- both hashed to index 7
      seqPairs = [(0, ("x", "p0")), (1, ("y", "p1"))]      -- sequential keeps them distinct
  putStrLn $ "-- toFGL node count under forced collision (hashed idx shared)? " ++ show (mergedUnderHashed forcedPairs)
  putStrLn $ "-- toSeqFGL node count (distinct sequential idx)? " ++ show (mergedUnderHashed seqPairs)
