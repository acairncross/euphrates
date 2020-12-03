{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Euphrates.Utils where

import Clash.Prelude
import Control.Monad.State
import Data.Reflection (reifyNat)
import Numeric.Natural (Natural)

mealyState
 :: HiddenClockResetEnable dom
 => NFDataX s
 => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

stickify :: HiddenClockResetEnable dom => NFDataX a => Signal dom (Maybe a) -> Signal dom (Maybe a)
stickify xm = let ym = liftA2 (<|>) xm (register Nothing ym) in ym

reifySNat :: Natural -> (forall n. SNat n -> b) -> b
reifySNat n k = reifyNat (toInteger n) (k . snatProxy)

-- Divide 1s by rate, rounding up - type level version of hzToPeriod
type HzToPeriod (rate :: Nat) = (Seconds 1 + rate - 1) `Div` rate

type Seconds      (s  :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds  (1_000 * us)
type Nanoseconds  (ns :: Nat) = Picoseconds  (1_000 * ns)
type Picoseconds  (ps :: Nat) = ps
