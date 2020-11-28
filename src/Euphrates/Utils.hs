module Euphrates.Utils (mealyState, stickify) where

import Clash.Prelude
import Control.Monad.State

mealyState
 :: HiddenClockResetEnable dom
 => NFDataX s
 => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)

stickify :: HiddenClockResetEnable dom => NFDataX a => Signal dom (Maybe a) -> Signal dom (Maybe a)
stickify xm = let ym = liftA2 (<|>) xm (register Nothing ym) in ym
