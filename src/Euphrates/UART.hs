{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Euphrates.UART (uartRx, uartTx) where

import Euphrates.Utils (mealyState)

import Clash.Prelude
import Control.Monad.State
import Data.Word

shiftBitR :: forall n. KnownNat n => BitVector n -> Bit -> BitVector n
shiftBitR bs b =
  let (bs', _) = bitCoerce $ pack b ++# bs :: (BitVector n, Bit)
  in bs'

data RxState n
  = RxIdle
  | RxBit Word32 (RxBit n)
  deriving (Generic, NFDataX, Eq, Show)

data RxBit n
  = RxStartBit
  | RxDataBit (BitVector n) (Index n)
  | RxStopBit (BitVector n)
  deriving (Generic, NFDataX, Eq, Show)

uartRxT :: KnownNat n => Word32 -> Bit -> State (RxState n) (Maybe (BitVector n))
uartRxT clocksPerBaud input = get >>= \case
  RxIdle -> do
    when (input == low) $ put (RxBit 0 (RxStartBit))
    return Nothing
  RxBit cnt rxBit -> do
    let cnt1 = cnt + 1
    let baudHalfDone = cnt1 == clocksPerBaud `shiftR` 1
    let baudDone = cnt1 == clocksPerBaud
    let cnt' = if baudDone then 0 else cnt1
    case rxBit of
      RxStartBit -> do
        let rxBit' = if baudDone then RxDataBit 0 0 else RxStartBit
        put $ RxBit cnt' rxBit'
        return Nothing
      RxDataBit datum i -> do
        if baudHalfDone
          then put $ RxBit cnt' (RxDataBit (shiftBitR datum input) i)
          else if baudDone
            then put $ RxBit cnt' (if i == maxBound then RxStopBit datum else RxDataBit datum (i+1))
            else put $ RxBit cnt' rxBit
        return Nothing
      RxStopBit datum ->
        if baudHalfDone
          then put RxIdle >> return (Just datum)
          else put (RxBit cnt' rxBit) >> return Nothing
    
-- | Receives an 8N1 UART input. Expects LSB first.
uartRx
  :: forall dom baudDuration
   . HiddenClockResetEnable dom
  => SNat baudDuration
  -- ^ Duration of baud in picoseconds
  -> Signal dom Bit
  -- ^ UART Rx
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Output byte
uartRx baudDuration urx =
  let clocksPerBaud = fromIntegral $
        snatToInteger baudDuration `div` snatToInteger (clockPeriod @dom)
      urx'' = register high . register high $ urx
  in mealyState (uartRxT clocksPerBaud) RxIdle urx''
{-# NOINLINE uartRx #-}

data TxState n
  = TxIdle
  | TxBit Word32 (TxBit n)
  deriving (Generic, NFDataX, Eq, Show)

data TxBit n
  = TxStartBit (BitVector n)
  | TxDataBit (BitVector n) (Index n)
  | TxStopBit
  deriving (Generic, NFDataX, Eq, Show)

uartTxT :: KnownNat n => Word32 -> Maybe (BitVector n) -> State (TxState n) (Bit, Bool)
uartTxT clocksPerBaud input = get >>= \case
  TxIdle -> case input of
    Just input' -> put (TxBit clocksPerBaud (TxStartBit input')) >> return (high, True)
    Nothing -> return (high, False)
  TxBit cnt txBit ->
    case txBit of
      TxStartBit datum -> do
        put $ if cnt == 0
          then TxBit clocksPerBaud (TxDataBit datum 0)
          else TxBit (cnt - 1) txBit
        return (low, True)
      TxDataBit datum i -> do
        put $ if cnt == 0
          then TxBit clocksPerBaud $
            if i == maxBound then TxStopBit else TxDataBit (rotateR datum 1) (i+1)
          else TxBit (cnt - 1) (TxDataBit datum i)
        return (lsb datum, True)
      TxStopBit -> do
        put $ if cnt == 0
          then TxIdle
          else TxBit (cnt - 1) TxStopBit
        return (high, True)

uartTx
  :: forall dom baudDuration
   . HiddenClockResetEnable dom
  => SNat baudDuration
  -> Signal dom (Maybe (BitVector 8))
  -> (Signal dom Bit, Signal dom Bool)
uartTx baudDuration utx =
  let clocksPerBaud = fromIntegral $
        snatToInteger baudDuration `div` snatToInteger (clockPeriod @dom)
      utx'' = register Nothing . register Nothing $ utx
  in unbundle $ mealyState (uartTxT clocksPerBaud) TxIdle utx''
{-# NOINLINE uartTx #-}
