{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Graphos.UseCase.InferDocumentSpec as D
import qualified Graphos.UseCase.DocLinkParitySpec as P
import qualified Graphos.UseCase.FormatContextSpec as F
import qualified Graphos.Domain.TypesSpec as T

main :: IO ()
main = do
  hspec D.spec
  hspec P.spec
  hspec F.spec
  hspec T.spec
