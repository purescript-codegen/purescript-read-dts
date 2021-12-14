module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem.Closed (coerce) as NoProblem
import Effect (Effect)
import Effect.Class (liftEffect)
import ReadDTS.TypeScript.Testing (handleMemoryFiles, inMemoryCompilerHost) as Testing
import Test.ReadDTS (suite) as Test.ReadDTS
import Test.Unit.Main (runTest)
import TypeScript.Class.Compiler.Program (createCompilerHost)
import TypeScript.Compiler.Program (createProgram)
import TypeScript.Compiler.Types (CompilerOptions, moduleKind, scriptTarget)

main :: Effect Unit
main = do
  let
    opts :: CompilerOptions
    opts = NoProblem.coerce { module: moduleKind."CommonJS", target: scriptTarget."ES5", strictNullChecks: true }

    compile rootNames files = liftEffect do
      -- | We have to load es library
      -- | because without it we are not
      -- | able to handle even arrays.
      host <- liftEffect $ Testing.inMemoryCompilerHost
        files
        "node_modules/typescript/lib/lib.es5.d.ts"

      createProgram rootNames opts (Just host)

  runTest (Test.ReadDTS.suite compile)
