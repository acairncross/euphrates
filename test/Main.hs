module Main where

import Hedgehog
import Test.Hspec (hspec)
import qualified Euphrates.Hedgehog
import qualified Euphrates.Spec

main :: IO ()
main = do
  hspec Euphrates.Spec.spec
  _ <- check Euphrates.Hedgehog.prop_non_negative
  _ <- check Euphrates.Hedgehog.prop_matches_fgl
  pure ()
