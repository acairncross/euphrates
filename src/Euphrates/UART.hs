{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Euphrates.UART (receiver) where

import Clash.Prelude
import Control.Monad.State
import Data.Word

data RxState n
  = RxIdle
  | RxBit Word32 (RxBit n)
  deriving (Generic, NFDataX, Eq, Show)

data RxBit n
  = StartBit
  | DataBit (BitVector n) (Index n)
  | StopBit (BitVector n)
  deriving (Generic, NFDataX, Eq, Show)

receiverT :: KnownNat n => Word32 -> Bit -> State (RxState n) (Maybe (BitVector n))
receiverT clocksPerBaud input = get >>= \case
  RxIdle -> do
    when (input == low) $ put (RxBit 0 (StartBit))
    return Nothing
  RxBit cnt rxBit -> do
    let cnt1 = cnt + 1
    let baudHalfDone = cnt1 == clocksPerBaud `div` 2
    let baudDone = cnt1 == clocksPerBaud
    let cnt' = if baudDone then 0 else cnt1
    case rxBit of
      StartBit -> do
        let rxBit' = if baudDone then DataBit 0 0 else StartBit
        put $ RxBit cnt' rxBit'
        return Nothing
      DataBit datum i -> do
        if baudHalfDone
          then put $ RxBit cnt' (DataBit (shiftBitL datum input) i)
          else if baudDone
            then put $ RxBit cnt' (if i == maxBound then StopBit datum else DataBit datum (i+1))
            else put $ RxBit cnt' rxBit
        return Nothing
      StopBit datum ->
        if baudHalfDone
          then put RxIdle >> return (Just datum)
          else put (RxBit cnt' rxBit) >> return Nothing
    
-- | Receives an 8N1 UART input
receiver
  :: forall dom
   . HiddenClockResetEnable dom
  => Word32
  -> Signal dom Bit
  -- ^ UART Rx
  -> Signal dom (Maybe (BitVector 8))
receiver clocksPerBaud urx =
  mealyState (receiverT clocksPerBaud) RxIdle (register low . register low $ urx)

shiftBitL :: forall n. KnownNat n => BitVector n -> Bit -> BitVector n
shiftBitL bs b =
  let (_, bs') = bitCoerce $ bs ++# pack b :: (Bit, BitVector n)
  in bs'

mealyState
 :: HiddenClockResetEnable dom
 => NFDataX s
 => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s in (s', y)
