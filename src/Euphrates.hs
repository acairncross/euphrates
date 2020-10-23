{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Euphrates (node, network, excessesToFlowValue, runNetwork) where

import Clash.Prelude
import Data.Function (on)
import Data.Maybe (isNothing, fromJust)
import qualified Prelude as P

-- Ordering on a extended with positive infinity.
compareWithInf :: Ord a => Maybe a -> Maybe a -> Ordering
compareWithInf (Just x) (Just y) = compare x y
compareWithInf (Just _) Nothing = LT
compareWithInf Nothing (Just _) = GT
compareWithInf Nothing Nothing = EQ

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y = case cmp x y of
  GT -> y
  _  -> x

-- Functions from Clash.Prelude with more convenient constraints
at' :: forall n m a. m + 1 <= n => SNat m -> Vec n a -> a
at' = leToPlus @(m+1) @n at 

init' :: forall n a. 1 <= n => Vec n a -> Vec (n-1) a
init' = leToPlus @1 @n init

last' :: forall n a. 1 <= n => Vec n a -> a
last' = leToPlus @1 @n last

-- | Note: If there is a residual (self-)edge from u to u, then relabelling might
-- only relabel to h(u) + 1
node
  :: forall n u a
   . 1 <= n
  => u + 1 <= n
  => KnownNat n
  => Real a
  => SNat u
  -- ^ Self
  -> Vec n a
  -- ^ Capacities
  -> a
  -- ^ Excess
  -> Vec n a
  -- ^ Heights
  -> Vec n a
  -- ^ Flows
  -> (a, Vec n a)
  -- ^ (Height', Flow update)
node u cs e hs fs =
  let -- Residual capacities
      rcs = zipWith (-) cs fs

      -- Height of each residual graph neighbor (infinite height if not a
      -- residual graph neighbor)
      hsM = zipWith (\h rc -> if rc > 0 then Just h else Nothing) hs rcs 

      -- Neighbor with smallest height
      (v, hvM) =
        fold @(n-1) (minBy (compareWithInf `on` snd)) (zip indicesI hsM)

      -- Height of self
      hu = at' u hs

  in case compareSNat u d0 of
    SNatLE -> (hu, repeat 0) -- u == 0 (self == source)
    SNatGT -> case compareSNat (SNat @n) (succSNat u) of
      SNatLE -> (hu, repeat 0) -- n == u+1 (self == sink)
      SNatGT -> case hvM of
        Just hv | e > 0 ->
          if hu > hv
            then -- push
              let d = min e (rcs !! v)
              in (hu, replace v d (repeat 0))
            else -- relabel
              (hv+1, repeat 0)
        _ -> -- No excess or no outgoing residual edges
          (hu, repeat 0)
{-# NOINLINE node #-}

network
  :: forall dom n a
   . 2 <= n
  => Real a
  => NFDataX a
  => SNat n
  -> (HiddenClockResetEnable dom
    => Signal dom Bool
    -- ^ Valid / Reset Flows
    -> Signal dom (Vec n (Vec n a))
    -- ^ Capacities
    -> (Signal dom (Vec n (Vec n a)), Signal dom (Vec n a)))
    -- ^ (Flows, Excesses)
network n@SNat = \flowRst css ->
  let nodes :: HiddenClockResetEnable dom => Signal dom (Vec n (a, Vec n a))
      nodes = bundle $
        smap @n
          (\u () ->
            (node @n u) <$> (at' u <$> css) <*> (at' u <$> es) <*> hs <*> (at' u <$> fss))
          (replicate n ())

      -- Heights'
      hs' :: HiddenClockResetEnable dom => Signal dom (Vec n a)
      -- Flow updates/deltas
      fssD :: HiddenClockResetEnable dom => Signal dom (Vec n (Vec n a))
      (hs', fssD) =
        let (<^$^>) = fmap . map
        in (fst <^$^> nodes, snd <^$^> nodes)

      -- Initially all heights set to 0 except source set to n
      hs0 :: Vec n a
      hs0 = replace (0 :: Int) (snatToNum n) (replicate n 0)

      -- Heights
      hs :: HiddenClockResetEnable dom => Signal dom (Vec n a)
      hs = register hs0 hs'

      -- Initially all flows set to 0 except outgoing edges from the source
      -- are saturated
      capacitiesToInitialFlows :: Vec n (Vec n a) -> Vec n (Vec n a)
      capacitiesToInitialFlows css0 =
        let cs = at' d0 css0
        in zipWith (\c fs -> replace (0 :: Int) (negate c) fs) cs
          $ replace (0 :: Int) cs
          $ replicate n
          $ replicate n 0

      fss :: HiddenClockResetEnable dom => Signal dom (Vec n (Vec n a))
      fss =
        let -- The equal and opposite flow update to fssD
            fssDT :: Signal dom (Vec n (Vec n a))
            fssDT = (map . map $ negate) . transpose <$> fssD

            -- Addition lifted over Signal and two layers of Vec
            (^+^) = liftA2 . zipWith . zipWith $ (+)

            fss0 :: Vec n (Vec n a)
            fss0 = replicate n $ replicate n 0

        in
          mux flowRst
            (capacitiesToInitialFlows <$> css)
            (register fss0 (fss ^+^ fssD ^+^ fssDT))

      -- Excesses
      es :: HiddenClockResetEnable dom => Signal dom (Vec n a)
      es = (fmap . map) (negate . sum) fss

  in (fss, es)
{-# NOINLINE network #-}

-- | Given node excesses, return 'Just' the flow value if there is in fact a
-- a flow, and return 'Nothing' otherwise (e.g. if there is a pre-flow).
excessesToFlowValue
  :: KnownNat n
  => 2 <= n
  => Num a
  => Ord a
  => Vec n a
  -> Maybe a
excessesToFlowValue es =
  if P.any (> 0) (init' es) then Nothing else Just (last' es)

-- | Compute a `network`'s flow value.
runNetwork
  :: forall dom n a
   . KnownDomain dom
  => KnownNat n
  => 2 <= n
  => NFDataX a
  => Num a
  => Ord a
  => Vec n (Vec n a)
  -- ^ Capacities
  -> (HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Vec n (Vec n a))
    -> (Signal dom (Vec n (Vec n a)), Signal dom (Vec n a)))
  -> a
runNetwork css nw =
  let valid :: HiddenClockResetEnable dom => Signal dom Bool
      valid = register True $ pure False

      samples = sample $ fmap excessesToFlowValue $ snd $ nw valid (pure css)
  in fromJust . P.head . P.dropWhile isNothing $ samples
