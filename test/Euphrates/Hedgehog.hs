{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Euphrates.Hedgehog where

import Clash.Prelude
import Data.Graph.Inductive.Query.MaxFlow (maxFlow)
import Euphrates.Core
import Hedgehog
import qualified Data.Graph.Inductive as G
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

-- A random flow network with integer capacities from 0 to 100 (no "self"
-- capacities)
flowNetwork :: MonadGen m => 1 <= n => SNat n -> m (Vec n (Vec n Int))
flowNetwork n@SNat = removeSelfEdges matrix
  where
    removeSelfEdges mat = zipWith (\i cs -> replace i 0 cs) (indices n) <$> mat
    matrix = sequence $ replicate n row
    row = sequence $ replicate n (Gen.int (Range.constant 0 100))

-- Convert a graph represented as a matrix to an FGL graph
matrixToGraph :: SNat n -> Vec n (Vec n Int) -> G.Gr () Int
matrixToGraph n css = G.mkGraph
  (P.zip [0..snatToNum n - 1] (P.repeat ()))
  [ (i, j, c)
  | (i, cs) <- P.zip [0..] . toList $ P.zip [0..] . toList <$> css
  , (j, c) <- cs
  , c /= 0
  ]

-- Max flows should never be negative
prop_non_negative :: Property
prop_non_negative = property $ do
  css <- forAll (flowNetwork d4)
  assert (runNetwork @System css (network d4) >= 0)

-- Use FGL as a reference implementation
prop_matches_fgl :: Property
prop_matches_fgl = property $ do
  css <- forAll (flowNetwork d8)
  runNetwork @System css (network d8) === maxFlow (matrixToGraph d8 css) 0 7
