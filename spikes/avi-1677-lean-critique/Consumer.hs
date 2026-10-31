{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.Word (Word8, Word64)

foreign import ccall "graphos_surprise_score"
  graphos_surprise_score :: Word8 -> Word8 -> Word8 -> Word8 -> Word64

main :: IO ()
main = do
  let cases =
        [ ( 0, 1, 1, 0, 6 )   -- conf<0.3 + cross-filetype + cross-community
        , ( 0, 1, 1, 1, 10 )  -- same, but inferred edge
        , ( 9, 0, 0, 0, 1 )   -- high confidence, same file type / community
        ]
  mapM_ run cases
  putStrLn "Stage-0 spike: all fixtures matched."

  where
    run (conf, cf, cc, inf, expected) = do
      let got = graphos_surprise_score conf cf cc inf
      if got == fromIntegral expected
        then putStrLn ("OK  conf=" ++ show conf ++ " cf=" ++ show cf
                      ++ " cc=" ++ show cc ++ " inferred=" ++ show inf
                      ++ " -> " ++ show got)
        else error ("MISMATCH expected " ++ show expected ++ " got " ++ show got)
