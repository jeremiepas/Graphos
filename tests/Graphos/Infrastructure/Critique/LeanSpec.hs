{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parity gate for the Lean 4 critique bridge (AVI-1677).
--
-- The default build keeps the Haskell critique as the sole backend, so this
-- spec pins the frozen parity baseline (fixtures 6 / 10 / 1) against the
-- Haskell reference. With the @lean-critique@ flag enabled it additionally
-- asserts that the linked Lean library is reported as available.
module Graphos.Infrastructure.Critique.LeanSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Graphos.Domain.Analysis (surpriseScore)
import Graphos.Domain.Types
  ( Confidence(..), Edge(..), EdgeId(..), NodeId, Relation(..) )
#ifdef LEAN_CRITIQUE
import Graphos.Infrastructure.Critique.Lean (leanCritiqueAvailable)
#endif

spec :: Spec
spec = describe "Lean critique bridge parity baseline (AVI-1677)" $ do
  let score rel c comms (uSrc, vSrc) =
        fst (surpriseScore undefined
               (Edge (EdgeId "e") "u" "v" rel 1.0 (Confidence c) Nothing)
               (Map.fromList comms) uSrc vSrc "u" "v")
      comms12 = [("u" :: NodeId, 1), ("v", 2)]

  it "fixture 6: low confidence, cross file type, cross community" $
    score Calls 0.1 comms12 ("a.py", "b.pdf") `shouldBe` 6

  it "fixture 10: inferred connection" $
    score Inferred 0.1 comms12 ("a.py", "b.pdf") `shouldBe` 10

  it "fixture 1: high confidence, same file type and community" $
    score Calls 0.9 [("u", 1), ("v", 1)] ("a.py", "a.py") `shouldBe` 1

#ifdef LEAN_CRITIQUE
  it "reports the linked Lean backend as available" $
    leanCritiqueAvailable `shouldBe` True
#endif
