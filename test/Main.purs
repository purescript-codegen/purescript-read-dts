module Test.Main where

import Prelude

import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Effect (Effect)
import Test.ReadDTS (suite) as Test.ReadDTS
import Test.Unit.Main (runTest)
import TypeScript.Class.Compiler.Program (createCompilerHost)
import TypeScript.Compiler.Types (moduleKind, scriptTarget)

main :: Effect Unit
main = do
  compilerHost <- createCompilerHost $ NoProblem.coerce
    { target: scriptTarget."ES5"
    , module: moduleKind."CommonJS"
    , strictNullChecks: false
    }

  runTest (Test.ReadDTS.suite compilerHost)
