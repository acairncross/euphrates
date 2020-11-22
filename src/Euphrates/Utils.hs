module Euphrates.Utils (mealyState) where

import Clash.Prelude
import Control.Monad.State

mealyState
 :: HiddenClockResetEnable dom
 => NFDataX s
 => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)
