module Test.Main where

import Prelude

import Effect (Effect)
import Test.ReadDTS.Instantiation (suite) as Test.ReadDTS.Instantiation
import Test.Unit.Main (runTest) as Test


main :: Effect Unit
main = do
  Test.runTest Test.ReadDTS.Instantiation.suite
