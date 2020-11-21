{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Euphrates.Spec (spec) where

import Test.Hspec
import Euphrates.Core
import Clash.Prelude

-- These example networks come from the tests in fgl, which in turn borrows
-- them from "Introduction to Algorithms" (Cormen, Leiserson, Rivest).
exampleNetwork1 :: Vec 4 (Vec 4 Int)
exampleNetwork1 =
  (0 :> 1000 :> 1000 :> 0 :> Nil) :>
  (0 :> 0 :> 1 :> 1000 :> Nil) :>
  (0 :> 0 :> 0 :> 1000 :> Nil) :>
  (0 :> 0 :> 0 :> 0 :> Nil) :>
  Nil

exampleNetwork2 :: Vec 6 (Vec 6 Int)
exampleNetwork2 =
  (0 :> 16 :> 13 :> 0 :> 0 :> 0 :> Nil) :>
  (0 :> 0 :> 10 :> 12 :> 0 :> 0 :> Nil) :>
  (0 :> 4 :> 0 :> 0 :> 14 :> 0 :> Nil) :>
  (0 :> 0 :> 9 :> 0 :> 0 :> 20 :> Nil) :>
  (0 :> 0 :> 0 :> 7 :> 0 :> 4 :> Nil) :>
  (0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil) :>
  Nil

spec :: Spec
spec = do
  describe "exampleNetwork1" $ do
    it "has a max flow of 2000" $ do
      runNetwork @System exampleNetwork1 (network d4) `shouldBe` 2000

  describe "exampleNetwork2" $ do
    it "has a max flow of 23" $ do
      runNetwork @System exampleNetwork2 (network d6) `shouldBe` 23
