{-# LANGUAGE ForeignFunctionInterface #-}

-- | Compile-time bridge to the Lean 4 critique library (AVI-1677).
--
-- This module is only compiled when the @lean-critique@ Cabal flag is
-- enabled.  In that mode Graphos links @libgraphoslean@ (produced from the
-- @graphos-lean-critique@ project) at build time and calls the exported C ABI
-- through @foreign import ccall@.  When the flag is disabled the Haskell
-- critique implementation in "Graphos.Domain.Analysis" remains the sole
-- backend, which keeps the default build free of any Lean toolchain
-- dependency.
module Graphos.Infrastructure.Critique.Lean
  ( leanCritiqueAvailable
  , leanCritiqueAbiVersion
  , leanSurpriseScore
  , leanCritiqueConfig
  ) where

import Data.Word (Word8, Word32, Word64)

-- | 'True' only when this module has been compiled against the linked Lean
-- library.  Callers use it to select between the Lean backend and the Haskell
-- reference implementation.
leanCritiqueAvailable :: Bool
leanCritiqueAvailable = True

-- | ABI version reported by the linked library.  Must agree with
-- 'GRAPHOS_LEAN_CRITIQUE_ABI_VERSION' in @graphos_lean_critique.h@.
leanCritiqueAbiVersion :: IO Word32
leanCritiqueAbiVersion = c_leanCritiqueAbiVersion

-- | Numeric score arm mirroring 'Graphos.Domain.Analysis.surpriseScore'.
-- Arguments are the frozen ABI v0 encoding (see contract-inventory.md).
leanSurpriseScore :: Word8 -> Word8 -> Word8 -> Word8 -> Word64
leanSurpriseScore = c_leanSurpriseScore

-- | Configuration query for the Lean critique bridge.
-- Returns whether the Lean backend is available and the ABI version action.
{-# NOINLINE leanCritiqueConfig #-}
leanCritiqueConfig :: IO (Bool, Word32)
leanCritiqueConfig = do
  v <- leanCritiqueAbiVersion
  pure (leanCritiqueAvailable, v)

foreign import ccall unsafe "graphos_lean_critique_abi_version"
  c_leanCritiqueAbiVersion :: IO Word32

foreign import ccall unsafe "graphos_surprise_score"
  c_leanSurpriseScore :: Word8 -> Word8 -> Word8 -> Word8 -> Word64