module Decls where

verdict :: Int -> Int
verdict x = helper x + 42

threshold = 10
helper x = x * 2

-- | Open a connection.
connect
  :: String
  -> IO Int
connect host = do
  let n = length host
  _ <- pure ()
  -- a comment inside the body

  (\_ -> pure ()) n
  -- a trailing comment inside the body
  (pure . (+ 1)) n

{- A block comment.
commented = 0
-}

-- | The mode.
--
-- This comment documents the declaration below and is not part of
-- the range of 'connect'.
data Mode
  = Auto
  | On
  | Off

-- | Documents the next declaration, not the previous one.
{-# INLINE size #-}
size :: Mode -> Int
size _ = 3

classify :: Int -> Mode
classify 0 = Off
classify 1 = On
classify _ = Auto

instance Show Mode where
  show _ = "mode"

render :: Mode -> String
render m =
  show m
