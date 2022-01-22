module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.NonRecType (suite) as Test.NonRecType
import Test.RecType (suite) as Test.RecType
import Test.Unit.Main (runTest)
import TypeScript.Class.Compiler.Program (createCompilerHost)
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, moduleKind, scriptTarget)

main :: Effect Unit
main = runTest do
  Test.NonRecType.suite
  Test.RecType.suite
