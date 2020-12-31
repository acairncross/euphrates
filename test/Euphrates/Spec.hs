{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Euphrates.Spec (spec) where

import Euphrates.Core
import Euphrates.UART (uartRx, uartTx)
import Euphrates.Utils (reifySNat)

import Clash.Prelude
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Numeric.Natural (Natural)
import Test.Hspec
import qualified Prelude as P

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

to8N1 :: BitPack a => BitSize a ~ 8 => Int -> a -> [Bit]
to8N1 clocksPerBaud n =
  P.replicate clocksPerBaud low P.++
  P.concatMap (P.replicate clocksPerBaud) (toList . reverse . bv2v . pack $ n) P.++
  P.replicate clocksPerBaud high

to8N1Multi :: BitPack a => BitSize a ~ 8 => Int -> Int -> [a] -> [Bit]
to8N1Multi clocksPerBaud clocksPerIdle xs =
  intercalate (P.replicate clocksPerIdle high) (P.map (to8N1 clocksPerBaud) xs)

spaceValues :: Int -> Int -> [a] -> [Maybe a]
spaceValues clocksPerBaud clocksPerIdle xs =
  P.concatMap (\x -> Just x : P.replicate (10 * clocksPerBaud + clocksPerIdle) Nothing) xs

spec :: Spec
spec = do
  describe "exampleNetwork1" $ do
    it "has a max flow of 2000" $ do
      runNetwork @System exampleNetwork1 (network d4) `shouldBe` 2000

  describe "exampleNetwork2" $ do
    it "has a max flow of 23" $ do
      runNetwork @System exampleNetwork2 (network d6) `shouldBe` 23

  describe "UART" $ do
    let clocksPerBaud = 111 :: Natural
    let clocksPerIdle = 0
    let baudDuration = clocksPerBaud * snatToNatural (clockPeriod @System)
    let values = [12, 34, 56, 78] :: [BitVector 8]

    it "receiver" $ do
      let serializedValues = to8N1Multi (fromIntegral clocksPerBaud) clocksPerIdle values
      let output = simulate @System (reifySNat baudDuration uartRx) serializedValues
      (P.take (P.length values) . catMaybes $ output) `shouldBe` values

    it "round trip" $ do
      let spacedValues =
            spaceValues (fromIntegral clocksPerBaud) clocksPerIdle values
              P.++ P.repeat Nothing
      let uartTxRx
            :: HiddenClockResetEnable dom
            => Signal dom (Maybe (BitVector 8))
            -> Signal dom (Maybe (BitVector 8))
          uartTxRx = reifySNat baudDuration uartRx . fst . reifySNat baudDuration uartTx
      let output = simulate @System uartTxRx spacedValues
      (P.take (P.length values) . catMaybes $ output) `shouldBe` values
