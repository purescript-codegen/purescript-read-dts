module TypeScript.Class.Compiler.Program where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import TypeScript.Compiler.Program (createCompilerHost, createProgram) as Program
import TypeScript.Compiler.Types (CompilerHost, CompilerOptions, Program)

createCompilerHost :: forall m. MonadEffect m => CompilerOptions -> m CompilerHost
createCompilerHost = liftEffect <<< Program.createCompilerHost

createProgram :: forall m. MonadEffect m => Array String -> CompilerOptions -> Maybe CompilerHost -> m Program
createProgram rootNames opts = liftEffect <<< Program.createProgram rootNames opts

-- emit :: forall m. MonadEffect m => Program -> m Unit
-- emit 
-- 
