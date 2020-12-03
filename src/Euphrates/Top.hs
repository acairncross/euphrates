{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- createDomain creates an ophan instance of KnownDomain
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Euphrates.Top where

import Euphrates.Core (network, networkRx, excessesToFlowValue)
import Euphrates.UART (uartRx)
import Euphrates.Utils (HzToPeriod)

import Clash.Annotations.TH (makeTopEntity)
import Clash.Prelude

import Data.Function ((&))

-- Domain with 25MHz clock
createDomain vXilinxSystem{vName="Dom25", vPeriod=hzToPeriod 25e6}

euphrates'
  :: HiddenClockResetEnable dom
  => Signal dom Bit
  -> Signal dom (Signed 8)
euphrates' input =
  input
  & uartRx (SNat @(HzToPeriod 115200))
  & register Nothing
  & networkRx d4
  & (fmap . fmap . fmap . fmap) unpack
  & network @_ @_ @(Signed 8) d4
  & (\(_flows, excesses) -> excesses)
  & register (repeat 0)
  & fmap ((maybe 0 id) . excessesToFlowValue)

euphrates
  :: "clk_25mhz" ::: Clock Dom25
  -> "ftdi_txd" ::: Signal Dom25 Bit
  -> "led" ::: Signal Dom25 (Signed 8)
euphrates clk input =
  withClockResetEnable clk resetGen enableGen $
    euphrates' input

makeTopEntity 'euphrates
