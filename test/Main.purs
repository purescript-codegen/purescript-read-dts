module Test.Main where

import Prelude

import Effect (Effect)
import Test.Codegen (suite) as Test.Codegen
import Test.Golden.ThreeJs (suite) as Test.Golden.ThreeJs
import Test.MultiModule (suite) as Test.MultiModule
import Test.NonRecType (suite) as Test.NonRecType
import Test.RecType (suite) as Test.RecType
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Test.NonRecType.suite
  Test.RecType.suite
  Test.MultiModule.suite
  Test.Golden.ThreeJs.suite
  Test.Codegen.suite
